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
#' [DCA Vignette](http://www.danieldsjoberg.com/dcurves/articles/dca.html)
#' for a detailed walk-through of various applications.
#' Also, see http://www.decisioncurveanalysis.org for more information.
#'
#' @author Daniel D Sjoberg
#'
#' @param formula a formula with the outcome on the LHS and a sum of
#' markers/covariates to test on the RHS
#' @param data a data frame containing the variables in `formula=`.
#' @param thresholds vector of threshold probabilities between 0 and 1.
#' Default is `seq(0.01, 0.99, by = 0.01)`
#' @param label named list of variable labels, e.g. `list(age = "Age, years)`
#' @param harm named list of harms associated with a test. Default is `NULL`
#' @param as_probability character vector including names of variables
#' that will be converted to a probability.
#' @param strategies Default strategies to include: 0, 1, or 2 of ("all", "none").
#' @param time if outcome is survival, `time=` specifies the time the
#' assessment is made
#' @param prevalence When `NULL`, the prevalence is estimated from `data=`.
#' If the data passed is a case-control set, the population prevalence
#' may be set with this argument.
#'
#' @return List including net benefit of each variable
#' @seealso [`net_intervention_avoided()`], [`plot.dca()`],
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
dca <- function(formula, data, thresholds = seq(0.01, 0.99, by = 0.01),
                label = NULL, harm = NULL, as_probability = character(0L),
                strategies = c("all", "none"),
                time = NULL, prevalence = NULL) {
  # checking inputs ------------------------------------------------------------
  if (!is.data.frame(data)) stop("`data=` must be a data frame")
  if (!inherits(formula, "formula")) stop("`formula=` must be a formula")

  # prepping data --------------------------------------------------------------
  thresholds <- thresholds[thresholds >= 0 & thresholds < 1]

  label_start <- list()
  if ("all" %in% strategies) {
    label_start$all = "Treat All"
  }

  if ("none" %in% strategies) {
    label_start$none = "Treat None"
  }

  label <- label_start %>%
    purrr::list_modify(!!!label)
  model_frame <- stats::model.frame(formula, data)
  outcome_name <- names(model_frame)[1]
  if (any(c("all", "none") %in% names(model_frame))) {
    stop("Variables cannot be named 'all' or 'none': they are reserved.",
      call. = FALSE
    )
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
  if (is.na(outcome_type)) {
    paste(
      "Outcome type not supported. Expecting a binary endpoint",
      "or an object of class 'Surv'."
    ) %>%
      stop(call. = FALSE)
  }
  if (outcome_type == "survival" && is.null(time)) {
    stop("`time=` must be specified for survival endpoints.")
  }

  # for binary outcomes, make the outcome a factor
  # so both levels always appear in `table()` results
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
      time = time
    )
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

  if (!"all" %in% strategies) {
    model_frame$all = NULL
  }

  if (!"none" %in% strategies) {
    model_frame$none = NULL
  }

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
          time = time
        ) %>%
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
        .data$tp_rate - .data$threshold /
          (1 - .data$threshold) * .data$fp_rate - .data$harm
    ) %>%
    tibble::as_tibble()

  # return results -------------------------------------------------------------
  lst_result <-
    list(
      call = match.call(),
      y = outcome_name,
      n = dca_result$n[1],
      prevalence = dca_result$prevalence[1],
      time = time,
      dca = dca_result
    ) %>%
    purrr::compact()
  class(lst_result) <- "dca"
  lst_result
}


#' Calculate a test's consequences
#'
#' @param outcome outcome vector
#' @param risk vector of risks
#' @param thresholds threshold probs vector
#' @param outcome_type type of outcome
#' @param time time to calculate risk if Surv() outcome
#' @param prevalence specifed prevleance (if cannot be estimated from data)
#'
#' @noRd
#' @keywords internal
.calculate_test_consequences <- function(outcome, risk, thresholds, outcome_type,
                                         prevalence, time) {
  df <-
    tibble::tibble(
      threshold = thresholds,
      n = length(outcome)
    )
  # case-control population prev
  if (!is.null(prevalence)) {
    df$prevalence <- prevalence
  } # survival endpoint prev
  else if (outcome_type == "survival") {
    outcome_prev <- .surv_to_risk(outcome ~ 1, time = time, quiet = TRUE) # TODO: print the multistate model note only once
    if (is.na(outcome_prev)) {
      paste(
        "Cannot calculate outcome prevalence at specified time,",
        "likely due to no observed data beyond selected time."
      ) %>%
        stop(call. = FALSE)
    }
    df$prevalence <- outcome_prev
  }
  # typical binary prev
  else {
    df$prevalence <- table(outcome)[2] / length(outcome)
  }

  if (outcome_type == "binary") {
    df <-
      df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        test_pos_rate =
          .convert_to_binary_fct(risk >= .data$threshold) %>%
            table() %>%
            purrr::pluck(2) %>% {
              . / .data$n
            },
        tp_rate =
          mean(risk[outcome == "TRUE"] >= .data$threshold) * .data$prevalence %>%
          unname(),
        fp_rate =
          mean(risk[outcome == "FALSE"] >= .data$threshold) * (1 - .data$prevalence) %>%
          unname(),
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
            {. / .data$n},
        risk_rate_among_test_pos =
          tryCatch(
            .surv_to_risk(outcome[risk >= .data$threshold] ~ 1, time = time),
            error = function(e) {
              if (length(outcome[risk >= .data$threshold]) == 0L) {
                return(0)
              }
              NA_real_
            }
          ),
        tp_rate = .data$risk_rate_among_test_pos * .data$test_pos_rate %>%
          unname(),
        fp_rate = (1 - .data$risk_rate_among_test_pos) * .data$test_pos_rate %>%
          unname(),
      )
  }

  df %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::any_of(c(
      "threshold", "prevalence",
      "n", "tp_rate", "fp_rate"
    )))
}


#' Convert binary outcome to factor
#'
#' @param x a vector
#' @param quiet logical. default is TRUE
#'
#' @noRd
#' @keywords internal
.convert_to_binary_fct <- function(x, quiet = TRUE) {
  # if not logical, convert to lgl
  if (!inherits(x, "logical")) {
    outcome_levels_sorted <- unique(x) %>% sort()
    if (!quiet) {
      glue::glue(
        "Assuming '{outcome_levels_sorted[2]}' is [Event] ",
        "and '{outcome_levels_sorted[1]}' is [non-Event]"
      ) %>%
        message()
    }
    x <-
      dplyr::case_when(
        x %in% outcome_levels_sorted[1] ~ FALSE,
        x %in% outcome_levels_sorted[2] ~ TRUE
      )
  }
  # convert lgl to fct
  factor(x, levels = c(FALSE, TRUE))
}

#' Calculate risks
#'
#' @param outcome outcome object
#' @param variable variable
#' @param outcome_type type of outcome
#' @param time time to calculate risk if Surv() outcome
#' @param prevalence specifed prevleance (if cannot be estimated from data)
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

  df_tidy %>%
    dplyr::filter(.data$time <= .env$time) %>%
    dplyr::slice_tail() %>%
    dplyr::pull(.data$estimate)
}
