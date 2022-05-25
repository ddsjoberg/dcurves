#' Perform Decision Curve Analysis
#'
#' Diagnostic and prognostic models are typically evaluated with measures of
#' accuracy that do not address clinical consequences.
#' Decision-analytic techniques allow assessment of clinical outcomes but often
#' require collection of additional information may be cumbersome to apply to
#' models that yield a continuous result. Decision curve analysis is a method
#' for evaluating and comparing prediction models that incorporates clinical
#' consequences, requires only the data set on which the models are tested,
#' and can be applied to models that have either continuous or dichotomous
#' results.
#' The dca function performs decision curve analysis for binary outcomes.
#' Review the
#' [DCA Vignette](https://www.danieldsjoberg.com/dcurves/articles/dca.html)
#' for a detailed walk-through of various applications.
#' Also, see [www.decisioncurveanalysis.org](https://www.mskcc.org/departments/epidemiology-biostatistics/biostatistics/decision-curve-analysis) for more information.
#'
#' @author Daniel D Sjoberg
#'
#' @param formula a formula with the outcome on the LHS and a sum of
#' markers/covariates to test on the RHS
#' @param data a data frame containing the variables in `formula=`.
#' @param thresholds vector of threshold probabilities between 0 and 1.
#' Default is `seq(0, 0.99, by = 0.01)`. Thresholds at zero are replaced
#' with 10e-10.
#' @param label named list of variable labels, e.g. `list(age = "Age, years)`
#' @param harm named list of harms associated with a test. Default is `NULL`
#' @param as_probability character vector including names of variables
#' that will be converted to a probability. Details below.
#' @param time if outcome is survival, `time=` specifies the time the
#' assessment is made
#' @param prevalence When `NULL`, the prevalence is estimated from `data=`.
#' If the data passed is a case-control set, the population prevalence
#' may be set with this argument.
#'
#' @section as_probability argument:
#' While the `as_probability=` argument can be used to convert a marker to the
#' probability scale, use the argument only when the consequences are fully
#' understood. For example, when the outcome is binary, logistic regression
#' is used to convert the marker to a probability. The logistic regression
#' model assumes linearity on the log-odds scale and can induce
#' miscalibration when this assumption is not true. Miscalibration in a
#' model will adversely affect performance on decision curve
#' analysis. Similarly, when the outcome is time-to-event, Cox Proportional
#' Hazards regression is used to convert the marker to a probability.
#' The Cox model also has a linearity assumption and additionally assumes
#' proportional hazards over the follow-up period. When these assumptions
#' are violated, important miscalibration may occur.
#'
#' Instead of using the `as_probability=` argument, it is suggested to perform
#' the regression modeling outside of the `dca()` function utilizing methods,
#' such as non-linear modeling, as appropriate.
#'
#' @return List including net benefit of each variable
#' @seealso [`net_intervention_avoided()`], [`standardized_net_benefit()`], [`plot.dca()`],
#' [`as_tibble.dca()`]
#' @export
#'
#' @examples
#' # calculate DCA with binary endpoint
#' dca(cancer ~ cancerpredmarker + marker,
#'     data = df_binary,
#'     as_probability = "marker",
#'     label = list(cancerpredmarker = "Prediction Model", marker = "Biomarker")) %>%
#'   # plot DCA curves with ggplot
#'   plot(smooth = TRUE) +
#'   # add ggplot formatting
#'   ggplot2::labs(x = "Treatment Threshold Probability")
#'
#' # calculate DCA with time to event endpoint
#' dca(Surv(ttcancer, cancer) ~ cancerpredmarker, data = df_surv, time = 1)
dca <- function(formula, data, thresholds = seq(0, 0.99, by = 0.01),
                label = NULL, harm = NULL, as_probability = character(),
                time = NULL, prevalence = NULL) {
  # checking inputs ------------------------------------------------------------
  if (!is.data.frame(data))
    stop("`data=` must be a data frame", call. = FALSE)
  if (!inherits(formula, "formula"))
    stop("`formula=` must be a formula", call. = FALSE)
  if (!is.null(label) && (!rlang::is_list(label) || !rlang::is_named(label)))
    stop("`label=` must be a named list or NULL.", call. = FALSE)
  if (!is.null(harm) && (!rlang::is_list(harm) || !rlang::is_named(harm)))
    stop("`harm=` must be a named list or NULL.", call. = FALSE)
  if (!is.character(as_probability))
    stop("`as_probability=` must be character.", call. = FALSE)

  # prepping data --------------------------------------------------------------
  thresholds <- thresholds[thresholds >= 0 & thresholds <= 1]

  label <-
    list(all = "Treat All", none = "Treat None") %>%
    purrr::list_modify(!!!label)
  model_frame <- stats::model.frame(formula, data)
  outcome_name <- names(model_frame)[1]
  if (any(c("all", "none") %in% names(model_frame))) {
    stop("Variables cannot be named 'all' or 'none': they are reserved.",
         call. = FALSE)
  }

  outcome_type <- .outcome_type(model_frame, outcome_name, time)

  # convert to probability if requested ----------------------------------------
  model_frame <- .as_probability(model_frame, outcome_name, outcome_type, as_probability, time)

  # add treat all and treat none -----------------------------------------------
  model_frame <-
    model_frame %>%
    dplyr::mutate(
      all = 1,
      none = 0,
      dplyr::across(.cols = -dplyr::all_of(outcome_name),
                    .fns = ~dplyr::case_when(. == 0 ~ 0 - .Machine$double.eps,
                                             . == 1 ~ 1 + .Machine$double.eps,
                                             TRUE ~ .)),
      .after = .data[[outcome_name]]
    )

  # calculate net benefit ------------------------------------------------------
  dca_result <-
    test_consequences_data_frame(
      model_frame = model_frame,
      outcome_name = outcome_name,
      outcome_type = outcome_type,
      statistics = c("pos_rate", "tp_rate", "fp_rate", "harm", "net_benefit"),
      thresholds = thresholds,
      label = label,
      time = time,
      prevalence = prevalence,
      harm = harm
    )

  # return results -------------------------------------------------------------
  lst_result <-
    list(
      call = match.call(),
      y = outcome_name,
      n = dca_result$n[1],
      prevalence = dca_result$pos_rate[1],
      time = time,
      dca = dca_result
    ) %>%
    purrr::compact()
  class(lst_result) <- "dca"
  lst_result
}


#' Calculate risks
#'
#' @param outcome outcome object
#' @param variable variable
#' @param outcome_type type of outcome
#' @param time time to calculate risk if Surv() outcome
#' @param prevalence specified prevalence (if cannot be estimated from data)
#'
#' @noRd
#' @keywords internal
.convert_to_risk <- function(outcome, variable, outcome_type,
                             time = NULL, prevalence = NULL) {
  if (outcome_type == "binary" && !is.null(prevalence)) {
    stop("Cannot convert to risks in case-control setting.")
  }

  if (outcome_type == "binary") {
    risk <-
      stats::glm(outcome ~ variable, family = stats::binomial) %>%
      stats::predict(type = "response")
  } else if (outcome_type == "survival") {
    # construct data frame
    df <- data.frame(outcome = outcome, variable = variable)
    new_df <- data.frame(outcome = outcome, variable = variable)
    new_df$outcome[, 1] <- time
    # build model, and get predictions for time point of interest
    risk <-
      survival::coxph(outcome ~ variable, data = df) %>%
      stats::predict(newdata = new_df, type = "expected") %>%
      {
        exp(-.)
      }
  }

  risk
}


#' Convert a `Surv()` object to a n-time risk estimate
#'
#' @param outcome `Surv()` object
#' @param time numeric time
#' @param quiet logical, default is TRUE
#'
#' @noRd
#' @keywords internal
.surv_to_risk <- function(outcome, time, quiet = TRUE) {
  df_tidy <-
    survival::survfit(outcome) %>%
    broom::tidy()

  # if multistate (i.e. competing risks) delete states not of interest
  if ("state" %in% names(df_tidy)) {
    state <- unique(df_tidy$state) %>%
      setdiff("(s0)") %>%
      purrr::pluck(1)
    df_tidy <- df_tidy %>% dplyr::filter(.data$state %in% .env$state)
    if (!isTRUE(quiet)) {
      glue::glue(
        "Multi-state model detected. Showing probabilities into state '{state}'") %>%
        message()
    }
  }
  # if regular survfit() model, convert survival to risk
  else {
    df_tidy <- dplyr::mutate(df_tidy, estimate = 1 - .data$estimate)
  }

  # if no observed times after specified time, return NA
  if (max(df_tidy$time) < time) {
    return(NA_real_)
  }

  df_tidy <-
    df_tidy %>%
    dplyr::filter(.data$time <= .env$time)

  # if no observed times after time point, return NA
  if (nrow(df_tidy) == 0L) {
    return(NA_real_)
  }

  df_tidy %>%
    dplyr::slice_tail() %>%
    dplyr::pull(.data$estimate)
}

.as_probability <- function(model_frame, outcome_name, outcome_type, as_probability, time) {
  as_probability <-
    model_frame %>%
    dplyr::select(-dplyr::all_of(outcome_name)) %>%
    dplyr::select(dplyr::all_of(as_probability))
  for (v in names(as_probability)) {
    model_frame[[v]] <- .convert_to_risk(model_frame[[outcome_name]],
                                         model_frame[[v]],
                                         outcome_type = outcome_type,
                                         time = time
    )
  }
  .check_probability_range(model_frame, outcome_name)

  model_frame
}

