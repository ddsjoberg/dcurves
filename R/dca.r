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
#' @param prevalence When NULL, the prevalence is estimated from `data=`.
#' If the data passed is a case-control set, the population prevalence
#' may be set with this argument.
#'
#' @return List including net benefit of each variable
#'
#' @examples
#' dca(cancer ~ cancerpredmarker, data = df_dca)
#'
#' # dca(Surv(ttcancer, cancer) ~ cancerpredmarker, data = df_dca, time = 1)
#'
#' @export

dca <- function(formula, data, thresholds = seq(0.01, 0.99, length.out = 99),
                label = NULL, harm = NULL, as_probability = character(),
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
  as_probability <-
    model_frame %>%
    dplyr::select(-dplyr::all_of(outcome_name)) %>%
    dplyr::select(dplyr::all_of(as_probability))
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
                                     thresholds) %>%
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
    ) %>%
    dplyr::left_join(
      dplyr::filter(., .data$variable %in% "all") %>%
        dplyr::select(.data$threshold, net_benefit_all = .data$net_benefit),
      by = "threshold"
    ) %>%
    dplyr::mutate(
      net_intervention_avoided =
        (.data$net_benefit - .data$net_benefit_all) / (.data$threshold / (1 - .data$threshold))
    ) %>%
    dplyr::select(-.data$net_benefit_all) %>%
    tibble::as_tibble()

  # return results -------------------------------------------------------------
  lst_result <-
    list(
      call = match.call(),
      n = dca_result$n[1],
      prevalence = dca_result$tp_rate[1] + dca_result$fn_rate[1],
      dca = dca_result
    )
  class(lst_result) <- c("dca", class(lst_result))
  lst_result
}

.calculate_test_consequences <- function(outcome, risk, threshold, outcome_type) {
  if (outcome_type == "binary") {
    df <-
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
        fn_rate = table[2, 1] / .data$n
      )
  }
  else if (outcome_type == "survival") {

  }

  df %>%
    dplyr::mutate(
      test_pos_rate = .data$tp_rate + .data$fp_rate,
      test_neg_rate = .data$tn_rate + .data$fn_rate
    ) %>%
    dplyr::select(.data$threshold, .data$n, .data$test_pos_rate, .data$test_neg_rate,
                  dplyr::everything(), -.data$table)
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
      stats::predict()
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


