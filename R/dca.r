#' Decision Curve Analysis
#'
#' Diagnostic and prognostic models are typically evaluated with measures of
#' accuracy that do not address clinical consequences.
#' Decision-analytic techniques allow assessment of clinical outcomes but often
#' require collection of additional information may be cumbersome to apply to
#' models that yield a continuous result. Decision curve analysis is a method
#' for evaluating and comparing prediction models that incorporates clinical
#' consequences, requires only the data set on which the models are tested,
#' and can be applied to models that have either continuous or dichotomous results.
#' The dca function performs decision curve analysis for binary outcomes.
#' See http://www.decisioncurveanalysis.org for more information.
#'
#' @author Daniel D Sjoberg
#'
#' @param formula formula
#' @param data a data frame containing the outcome of the outcome predictions.
#' @param thresholds vector of threshold probabilities between 0 and 1.
#' @param label named list of variable labels, e.g. `list(age = "Age, years)`
#' @param harm named list of harms associated with a test. Default is `NULL`
#' @param as_probability character vector including names of variables
#' that will be converted to a probability.
#' @param time if outcome is survival, `time=` specifies the time the
#' assessment is made
#'
#' @return List including net benefit of each variable
#'
#' @examples
#' dca(cancer ~ cancerpredmarker, data = df_dca)
#'
#' dca(Surv(ttcancer, cancer) ~ cancerpredmarker, data = df_dca, time = 1)
#'
#' @export

dca <- function(formula, data, thresholds = seq(0, 1, length.out = 101),
                label = NULL, harm = NULL, as_probability = character(), time = NULL) {
  # checking inputs ------------------------------------------------------------
  if (!is.data.frame(data)) stop("`data=` must be a data frame")
  if (!inherits(formula, "formula")) stop("`formula=` must be a formula")

  # prepping data --------------------------------------------------------------
  thresholds <- thresholds[thresholds > 0 & thresholds < 1]
  label <- list(all = "Treat All", none = "Treat None") %>% purrr::list_modify(!!!label)
  model_frame <- stats::model.frame(formula, data)
  outcome_name <- names(model_frame)[1]
  if (any(c("all", "none") %in% names(model_frame))) {
    stop("Variables cannot be named 'all' or 'none': they are reserved.", call. = FALSE)
  }

  outcome_type <-
    dplyr::case_when(
      inherits(model_frame[[outcome_name]], "Surv") ~ "survival",
      dplyr::n_distinct(model_frame[[outcome_name]]) == 2L ~ "binary",
      dplyr::n_distinct(model_frame[[outcome_name]]) == 1L &&
        inherits(model_frame[[outcome_name]], "factor") &&
        length(attr(model_frame[[outcome_name]], "level")) == 2L ~ "binary",
      dplyr::n_distinct(model_frame[[outcome_name]]) == 1L &&
        inherits(model_frame[[outcome_name]], "logical") ~ "binary"
    )
  if (is.na(outcome_type))
    paste("Outcome type not supported. Expecting a binary endpoint",
          "or an object of class 'Surv'.") %>%
    stop(call. = FALSE)
  if (outcome_type == "survival" && !is.null(time))
    stop("`time=` must be specified for survival endpoints.")

  # for binary outcomes, make the outcome a factor to both levels always appear in `table()` results
  if (outcome_type == "binary") {
    model_frame[[outcome_name]] <-
      .convert_to_binary_fct(model_frame[[outcome_name]], quiet = FALSE)
  }

  # convert to probability if requested ----------------------------------------
  for (v in as_probability) {
    model_frame[[v]] <- .convert_to_risk(model_frame[[outcome_name]],
                                         model_frame[[v]],
                                         outcome_type = outcome_type,
                                         time = time)
  }
  for (v in names(model_frame) %>% setdiff(outcome_name)) {
    if (any(!dplyr::between(model_frame[[v]], 0L, 1L))) {
      glue::glue("Error in {v}. All covariates/risks must be between 0 and 1.") %>%
        stop(call. = FALSE)
    }
  }

  # add treat all and treat none -----------------------------------------------
  model_frame <-
    model_frame %>%
    dplyr::mutate(
      all = 0L,
      none = 1L,
      .after = .data[[outcome_name]]
    )

  # calculate net benefit ------------------------------------------------------
  result <-
    names(model_frame) %>%
    setdiff(outcome_name) %>%
    lapply(
      function(x) {
        .calculate_test_consequences(model_frame[[outcome_name]],
                                     model_frame[[x]],
                                     thresholds) %>%
          dplyr::mutate(
            variable = x,
            label = .env$label[[x]] %||% x,
            .before = .data$threshold
          )
      }
    ) %>%
    dplyr::bind_rows() %>%
    # adding harm
    dplyr::left_join(
      rbind(data.frame(variable = character(), harm = numeric()),
            tibble::enframe(harm, name = "variable", value = "harm")),
      by = "variable"
    ) %>%
    dplyr::mutate(
      harm = dplyr::coalesce(harm, 0),
      net_benefit = .data$tp_rate - .data$threshold / (1 - .data$threshold) * .data$fp_rate - .data$harm
    ) %>%
    tibble::as_tibble()

}

.calculate_test_consequences <- function(outcome, risk, threshold) {
  tibble::tibble(threshold = threshold) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      table =
        table(outcome, .convert_to_binary_fct(risk >= .data$threshold)) %>%
        list(),
      n = sum(.data$table),
      tp_rate = table[2, 2] / .data$n,
      fp_rate = table[1, 2] / .data$n,
      tn_rate = table[1, 1] / .data$n,
      fn_rate = table[2, 1] / .data$n,
      test_pos_rate = tp_rate + fp_rate,
      test_neg_rate = tn_rate + fn_rate
    ) %>%
    dplyr::select(.data$threshold, .data$n, .data$test_pos_rate, .data$test_neg_rate,
                  everything(), -.data$table)
}

.convert_to_binary_fct <- function(x, quiet = TRUE) {
  # if not logical, convert to lgl
  if (!inherits(x, "logical")) {
    outcome_levels_sorted <- unique(x) %>% sort()
    if (!quiet)
      glue::glue("Assuming '{outcome_levels_sorted[2]}' is [Event] ",
                 "and '{outcome_levels_sorted[1]}' is [non-Event]") %>%
      message()
    x <-
      dplyr::case_when(
        x %in% outcome_levels_sorted[1] ~ FALSE,
        x %in% outcome_levels_sorted[2] ~ TRUE
      )
  }
  # convert lgl to fct
  factor(x, levels = c(FALSE, TRUE))
}

.convert_to_risk <- function(outcome, variable, outcome_type, time = NULL) {
  if (outcome_type == "binary")
    risk <-
      glm(outcome ~ variable, family = binomial) %>%
      predict()
  else if (outcome_type == "survival") {
    # construct data frame
    df <- data.frame(outcome = outcome, variable = variable)
    new_df <- data.frame(outcome = outcome, variable = variable)
    new_df$outcome[, 1] <- time
    # build model, and get predictions for time point of interest
    risk <-
      survival::coxph(outcome ~ variable, data = df) %>%
      predict(newdata = new_df, type = "expected") %>%
      {exp(-.)}
  }

  risk
}






















# #ONLY KEEPING COMPLETE CASES
# data=data[stats::complete.cases(data[append(outcome,predictors)]),append(outcome,predictors)]
#
# # outcome MUST BE CODED AS 0 AND 1
# if (max(data[[outcome]])>1 | min(data[[outcome]])<0) {
#   stop("outcome cannot be less than 0 or greater than 1")
# }
# # xstart IS BETWEEN 0 AND 1
# if (xstart<0 | xstart>1) {
#   stop("xstart must lie between 0 and 1")
# }
#
# # xstop IS BETWEEN 0 AND 1
# if (xstop<0 | xstop>1) {
#   stop("xstop must lie between 0 and 1")
# }
#
# # xby IS BETWEEN 0 AND 1
# if (xby<=0 | xby>=1) {
#   stop("xby must lie between 0 and 1")
# }
#
# # xstart IS BEFORE xstop
# if (xstart>=xstop) {
#   stop("xstop must be larger than xstart")
# }
#
# #STORING THE NUMBER OF PREDICTORS SPECIFIED
# pred.n=length(predictors)
#
# #IF probability SPECIFIED ENSURING THAT EACH PREDICTOR IS INDICATED AS A YES OR NO
# if (length(probability)>0 & pred.n!=length(probability)) {
#   stop("Number of probabilities specified must be the same as the number of predictors being checked.")
# }
#
# #IF harm SPECIFIED ENSURING THAT EACH PREDICTOR HAS A SPECIFIED HARM
# if (length(harm)>0 & pred.n!=length(harm)) {
#   stop("Number of harms specified must be the same as the number of predictors being checked.")
# }
#
# #INITIALIZING DEFAULT VALUES FOR PROBABILITES AND HARMS IF NOT SPECIFIED
# if (length(harm)==0) {
#   harm=rep(0,pred.n)
# }
# if (length(probability)==0) {
#   probability=rep(TRUE,pred.n)
# }
#
#
# #CHECKING THAT EACH probability ELEMENT IS EQUAL TO YES OR NO,
# #AND CHECKING THAT PROBABILITIES ARE BETWEEN 0 and 1
# #IF NOT A PROB THEN CONVERTING WITH A LOGISTIC REGRESSION
# for(m in 1:pred.n) {
#   if (probability[m]!=TRUE & probability[m]!=FALSE) {
#     stop("Each element of probability vector must be TRUE or FALSE")
#   }
#   if (probability[m]==TRUE & (max(data[predictors[m]])>1 | min(data[predictors[m]])<0)) {
#     stop(paste(predictors[m],"must be between 0 and 1 OR sepcified as a non-probability in the probability option",sep=" "))
#   }
#   if(probability[m]==FALSE) {
#     model=NULL
#     pred=NULL
#     model=stats::glm(data.matrix(data[outcome]) ~ data.matrix(data[predictors[m]]), family=stats::binomial("logit"))
#     pred=data.frame(model$fitted.values)
#     pred=data.frame(pred)
#     names(pred)=predictors[m]
#     data=cbind(data[names(data)!=predictors[m]],pred)
#     print(paste(predictors[m],"converted to a probability with logistic regression. Due to linearity assumption, miscalibration may occur.",sep=" "))
#   }
# }
#
# # THE PREDICTOR NAMES CANNOT BE EQUAL TO all OR none.
# if (length(predictors[predictors=="all" | predictors=="none"])) {
#   stop("Prediction names cannot be equal to all or none.")
# }
#
# #########  CALCULATING NET BENEFIT   #########
# N=dim(data)[1]
# event.rate=colMeans(data[outcome])
#
# # CREATING DATAFRAME THAT IS ONE LINE PER THRESHOLD PER all AND none STRATEGY
# nb=data.frame(seq(from=xstart, to=xstop, by=xby))
# names(nb)="threshold"
# interv=nb
#
# nb["all"]=event.rate - (1-event.rate)*nb$threshold/(1-nb$threshold)
# nb["none"]=0
#
# # CYCLING THROUGH EACH PREDICTOR AND CALCULATING NET BENEFIT
# for(m in 1:pred.n){
#   for(t in 1:length(nb$threshold)){
#     # COUNTING TRUE POSITIVES AT EACH THRESHOLD
#     tp=mean(data[data[[predictors[m]]]>=nb$threshold[t],outcome])*sum(data[[predictors[m]]]>=nb$threshold[t])
#     # COUNTING FALSE POSITIVES AT EACH THRESHOLD
#     fp=(1-mean(data[data[[predictors[m]]]>=nb$threshold[t],outcome]))*sum(data[[predictors[m]]]>=nb$threshold[t])
#     #setting TP and FP to 0 if no observations meet threshold prob.
#     if (sum(data[[predictors[m]]]>=nb$threshold[t])==0) {
#       tp=0
#       fp=0
#     }
#
#     # CALCULATING NET BENEFIT
#     nb[t,predictors[m]]=tp/N - fp/N*(nb$threshold[t]/(1-nb$threshold[t])) - harm[m]
#   }
#   interv[predictors[m]]=(nb[predictors[m]] - nb["all"])*interventionper/(interv$threshold/(1-interv$threshold))
# }
#
# # CYCLING THROUGH EACH PREDICTOR AND SMOOTH NET BENEFIT AND INTERVENTIONS AVOIDED
# for(m in 1:pred.n) {
#   if (smooth==TRUE){
#     lws=stats::loess(data.matrix(nb[!is.na(nb[[predictors[m]]]),predictors[m]]) ~ data.matrix(nb[!is.na(nb[[predictors[m]]]),"threshold"]),span=loess.span)
#     nb[!is.na(nb[[predictors[m]]]),paste(predictors[m],"_sm",sep="")]=lws$fitted
#
#     lws=stats::loess(data.matrix(interv[!is.na(nb[[predictors[m]]]),predictors[m]]) ~ data.matrix(interv[!is.na(nb[[predictors[m]]]),"threshold"]),span=loess.span)
#     interv[!is.na(nb[[predictors[m]]]),paste(predictors[m],"_sm",sep="")]=lws$fitted
#   }
# }
#
# # PLOTTING GRAPH IF REQUESTED
# if (graph==TRUE) {
#
#   # PLOTTING INTERVENTIONS AVOIDED IF REQUESTED
#   if(intervention==TRUE) {
#     # initialize the legend label, color, and width using the standard specs of the none and all lines
#     legendlabel <- NULL
#     legendcolor <- NULL
#     legendwidth <- NULL
#     legendpattern <- NULL
#
#     #getting maximum number of avoided interventions
#     ymax=max(interv[predictors],na.rm = TRUE)
#
#     #INITIALIZING EMPTY PLOT WITH LABELS
#     graphics::plot(x=nb$threshold, y=nb$all, type="n" ,xlim=c(xstart, xstop), ylim=c(ymin, ymax), xlab="Threshold probability", ylab=paste("Net reduction in interventions per",interventionper,"patients"))
#
#     #PLOTTING INTERVENTIONS AVOIDED FOR EACH PREDICTOR
#     for(m in 1:pred.n) {
#       if (smooth==TRUE){
#         graphics::lines(interv$threshold,data.matrix(interv[paste(predictors[m],"_sm",sep="")]),col=m,lty=2)
#       } else {
#         graphics::lines(interv$threshold,data.matrix(interv[predictors[m]]),col=m,lty=2)
#       }
#
#       # adding each model to the legend
#       legendlabel <- c(legendlabel, predictors[m])
#       legendcolor <- c(legendcolor, m)
#       legendwidth <- c(legendwidth, 1)
#       legendpattern <- c(legendpattern, 2)
#     }
#   } else {
#   # PLOTTING NET BENEFIT IF REQUESTED
#
#     # initialize the legend label, color, and width using the standard specs of the none and all lines
#     legendlabel <- c("None", "All")
#     legendcolor <- c(17, 8)
#     legendwidth <- c(2, 2)
#     legendpattern <- c(1, 1)
#
#     #getting maximum net benefit
#     ymax=max(nb[names(nb)!="threshold"],na.rm = TRUE)
#
#     # inializing new benfit plot with treat all option
#     graphics::plot(x=nb$threshold, y=nb$all, type="l", col=8, lwd=2 ,xlim=c(xstart, xstop), ylim=c(ymin, ymax), xlab="Threshold probability", ylab="Net benefit")
#     # adding treat none option
#     graphics::lines(x=nb$threshold, y=nb$none,lwd=2)
#     #PLOTTING net benefit FOR EACH PREDICTOR
#     for(m in 1:pred.n) {
#       if (smooth==TRUE){
#         graphics::lines(nb$threshold,data.matrix(nb[paste(predictors[m],"_sm",sep="")]),col=m,lty=2)
#       } else {
#         graphics::lines(nb$threshold,data.matrix(nb[predictors[m]]),col=m,lty=2)
#       }
#       # adding each model to the legend
#       legendlabel <- c(legendlabel, predictors[m])
#       legendcolor <- c(legendcolor, m)
#       legendwidth <- c(legendwidth, 1)
#       legendpattern <- c(legendpattern, 2)
#     }
#   }
#   # then add the legend
#   graphics::legend("topright", legendlabel, cex=0.8, col=legendcolor, lwd=legendwidth, lty=legendpattern)
#
# }
#
# #RETURNING RESULTS
# results=list()
# results$N=N
# results$predictors=data.frame(cbind(predictors,harm,probability))
# names(results$predictors)=c("predictor","harm.applied","probability")
# results$interventions.avoided.per=interventionper
# results$net.benefit=nb
# results$interventions.avoided=interv
#
# return(results)
