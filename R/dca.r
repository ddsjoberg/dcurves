#' Perform Decision Curve Analysis
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
#' @param prevalence When NULL, the prevalence is estimated from `data=`.
#' If the data passed is a case-control set, the population prevalence
#' may be set with this argument.
#'
#' @return List including net benefit of each variable
#' @seealso [`autoplot.dca()`], [`as_tibble.dca()`]
#' @export
#'
#' @examples
#' # calculate DCA with binary endpoint
#' dca(cancer ~ cancerpredmarker,
#'     data = df_binary,
#'     label = list(cancerpredmarker = "Prediction Model")) %>%
#'   # plot DCA curves with ggplot
#'   autoplot(smooth = TRUE) +
#'   # add ggplot formatting
#'   ggplot2::labs(x = "Treatment Threshold Probability")
#'
#' # calculate DCA with time to event endpoint
#' dca(Surv(ttcancer, cancer) ~ cancerpredmarker, data = df_surv, time = 1)

dca <- function(formula, data, thresholds = seq(0.01, 0.99, length.out = 99),
                label = NULL, harm = NULL, as_probability = character(0L),
                time = NULL, prevalence = NULL) {
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
      length(unique(model_frame[[outcome_name]])) == 2L ~ "binary",
      length(unique(model_frame[[outcome_name]])) == 1L &&
        inherits(model_frame[[outcome_name]], "factor") &&
        length(attr(model_frame[[outcome_name]], "level")) == 2L ~ "binary",
      length(unique(model_frame[[outcome_name]])) == 1L &&
        inherits(model_frame[[outcome_name]], "logical") ~ "binary"
    )
  if (is.na(outcome_type))
    paste("Outcome type not supported. Expecting a binary endpoint",
          "or an object of class 'Surv'.") %>%
    stop(call. = FALSE)
  if (outcome_type == "survival" && is.null(time))
    stop("`time=` must be specified for survival endpoints.")

  # for binary outcomes, make the outcome a factor so both levels always appear in `table()` results
  if (outcome_type == "binary") {
    model_frame[[outcome_name]] <-
      .convert_to_binary_fct(model_frame[[outcome_name]], quiet = FALSE)
  }

  # convert to probability if requested ----------------------------------------
  as_probability <-
    model_frame %>%
    dplyr::select(-dplyr::all_of(outcome_name)) %>%
    dplyr::select(dplyr::all_of(as_probability))
  for (v in names(as_probability)) {
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
      all = 1L,
      none = 0L,
      .after = .data[[outcome_name]]
    )

  # calculate net benefit ------------------------------------------------------
  dca_result <-
    names(model_frame) %>%
    setdiff(outcome_name) %>%
    lapply(
      function(x) {
        .calculate_test_consequences(model_frame[[outcome_name]],
                                     model_frame[[x]],
                                     thresholds = thresholds,
                                     outcome_type = outcome_type,
                                     prevalence = prevalence,
                                     time = time) %>%
          dplyr::mutate(
            variable = x,
            label = .env$label[[x]] %||% attr(model_frame[[x]], "label") %||% x,
            harm = .env$harm[[x]] %||% 0,
            .before = .data$threshold
          )
      }
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      label = factor(.data$label, levels = unique(.data$label)),
      harm = dplyr::coalesce(harm, 0),
      net_benefit =
        .data$tp_rate - .data$threshold / (1 - .data$threshold) * .data$fp_rate - .data$harm
    )
    tibble::as_tibble()

  # return results -------------------------------------------------------------
  lst_result <-
    list(
      call = match.call(),
      n = dca_result$n[1],
      prevalence = dca_result$prevalence[1],
      time = time,
      dca = dca_result
    ) %>%
    purrr::compact()
  class(lst_result) <- c("dca", class(lst_result))
  lst_result
}

.calculate_test_consequences <- function(outcome, risk, thresholds, outcome_type,
                                         prevalence, time) {
  df <-
    tibble::tibble(threshold = thresholds,
                   n = length(outcome))
  # case-control population prev
  if (!is.null(prevalence)) df$prevalence <- prevalence
  # survival endpoint prev
  else if (outcome_type == "survival") {
    outcome_prev <- .surv_to_risk(outcome ~ 1, time = time, quiet = TRUE) # TODO: print the multistate model note only once
    if (is.na(outcome_prev))
      paste("Cannot calculate outcome prevalence at specified time,",
            "likely due to no observed data beyond selected time.") %>%
      stop(call. = FALSE)
    df$prevalence <- outcome_prev
  }
  # typical binary prev
  else df$prevalence <- table(outcome)[2] / length(outcome)

  if (outcome_type == "binary") {
    df <-
      df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        test_pos_rate =
          .convert_to_binary_fct(risk >= .data$threshold) %>%
          table() %>%
          purrr::pluck(2) %>%
          {. / .data$n},
        tp_rate =
          mean(risk[outcome == "TRUE"] >= .data$threshold) * .data$prevalence,
        fp_rate =
          mean(risk[outcome == "FALSE"] >= .data$threshold) * (1 - .data$prevalence),
      )
  }
  else if (outcome_type == "survival") {
    df <-
      df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        test_pos_rate =
          .convert_to_binary_fct(risk >= .data$threshold) %>%
          table() %>%
          purrr::pluck(2) %>%
          `/`(.data$n),
        risk_rate_among_test_pos =
          tryCatch(
            .surv_to_risk(outcome[risk >= .data$threshold] ~ 1, time = time),
            error = function(e) {
              if (length(outcome[risk >= .data$threshold]) == 0L) return(0)
              NA_real_
            }
          ),
        tp_rate = .data$risk_rate_among_test_pos * .data$test_pos_rate,
        fp_rate = (1 - .data$risk_rate_among_test_pos) * .data$test_pos_rate,
      )
  }

  df %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::any_of(c("threshold", "prevalence", "n", "tp_rate", "fp_rate")))
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

.convert_to_risk <- function(outcome, variable, outcome_type, time = NULL, prevalence = NULL) {
  if (outcome_type == "binary" && !is.null(prevalence))
    stop("Cannot convert to risks in case-control setting.")

  if (outcome_type == "binary")
    risk <-
      stats::glm(outcome ~ variable, family = stats::binomial) %>%
      stats::predict(type = "response")
  else if (outcome_type == "survival") {
    # construct data frame
    df <- data.frame(outcome = outcome, variable = variable)
    new_df <- data.frame(outcome = outcome, variable = variable)
    new_df$outcome[, 1] <- time
    # build model, and get predictions for time point of interest
    risk <-
      survival::coxph(outcome ~ variable, data = df) %>%
      stats::predict(newdata = new_df, type = "expected") %>%
      {exp(-.)}
  }

  risk
}


.surv_to_risk <- function(outcome, time, quiet = TRUE) {
  df_tidy <-
    survival::survfit(outcome) %>%
    broom::tidy()

  # if multistate (i.e. competing risks) delete states not of interest
  if ("state" %in% names(df_tidy)) {
    state <- unique(df_tidy$state) %>% setdiff("(s0)") %>% purrr::pluck(1)
    df_tidy <- df_tidy %>% dplyr::filter(.data$state %in% .env$state)
    if (isFALSE(quiet))
      paste0("Multi-state model detected. Showing probabilities into state '", state, "'") %>%
      message()
  }
  # if regular survfit() model, convert survival to risk
  else {
    df_tidy <- dplyr::mutate(df_tidy, estimate = 1 - .data$estimate)
  }

  # if no observed times after spefified time, return NA
  if (max(df_tidy$time) < time) return(NA_real_)

  df_tidy %>%
    dplyr::filter(.data$time <= .env$time) %>%
    dplyr::slice_tail() %>%
    dplyr::pull(.data$estimate)
}
