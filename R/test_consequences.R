#' Test Consequences
#'
#' @inheritParams dca
#' @param statistics Charactger vector with statistics to return. See below for details
#'
#' @section statistics:
#'
#' The following diagnostic statistics are available to return.
#'
#' \if{html}{\figure{two-by-two-table.jpg}{options: width=25\%}}
#'
#' ```{r, eval = TRUE, echo = FALSE}
#' knitr::kable(
#'   tibble::tribble(
#'     ~`**Statistic**`,            ~`**Abbreviation**`, ~`**Definition**`,
#'     "Outcome Positive Rate",     '`"pos_rate"`',      "`(a + c) / (a + b + c + d)`",
#'     "Outcome Negative Rate",     '`"neg_rate"`',      "`(b + d) / (a + b + c + d)`",
#'     "True Positive Rate",        '`"tp_rate"`',       "`a / (a + b + c + d)`",
#'     "False Positive Rate",       '`"fp_rate"`',       "`b / (a + b + c + d)`",
#'     "False Negative Rate",       '`"fn_rate"`',       "`c / (a + b + c + d)`",
#'     "True Negative Rate",        '`"tn_rate"`',       "`d / (a + b + c + d)`",
#'     "Test Positive Rate",        '`"test_pos_rate"`', "`(a + b) / (a + b + c + d)`",
#'     "Test Negative Rate",        '`"test_neg_rate"`', "`(c + d) / (a + b + c + d)`",
#'     "Positive Predictive Value", '`"ppv"`',           "`a / (a + b)`",
#'     "Negative Predictive Value", '`"npv"`',           "`d / (c + d)`",
#'     "Sensitivity",               '`"sens"`',          "`1 / (a + c)`",
#'     "Specificity",               '`"spec"`',          "`d / (b + d)`",
#'     "Positive Likelihood Ratio", '`"lr_pos"`',        "`sens / (1 - spec)`",
#'     "Negative Likelihood Ratio", '`"lr_neg"`',        "`(1 - sens) / spec`"
#'   )
#' )
#'
#' @return a tibble with test consequences
#' @export
#'
#' @examples
#' test_consequences(cancer ~ cancerpredmarker, data = df_binary)

test_consequences <- function(formula, data,
                              statistics = c("pos_rate", "neg_rate",
                                             "tp_rate", "fp_rate",
                                             "tn_rate", "fn_rate",
                                             "test_pos_rate", "test_neg_rate",
                                             "ppv", "npv",
                                             "sens", "spec",
                                             "lr_pos", "lr_neg"),
                              thresholds = seq(0, 1, by = 0.25), label = NULL,
                              time = NULL, prevalence = NULL) {
  # checking inputs ------------------------------------------------------------
  if (!is.data.frame(data))
    stop("`data=` must be a data frame", call. = FALSE)
  if (!inherits(formula, "formula"))
    stop("`formula=` must be a formula", call. = FALSE)
  if (!is.null(label) && (!rlang::is_list(label) || !rlang::is_named(label)))
    stop("`label=` must be a named list or NULL.", call. = FALSE)
  statistics <- match.arg(statistics, several.ok = TRUE)

  # prepping data --------------------------------------------------------------
  thresholds <- thresholds[thresholds >= 0 & thresholds <= 1]

  model_frame <- stats::model.frame(formula, data)
  outcome_name <- names(model_frame)[1]
  outcome_type <- .outcome_type(model_frame, outcome_name, time)

  test_consequences_data_frame(model_frame = model_frame, outcome_name = outcome_name,
                               outcome_type = outcome_type, statistics = statistics,
                               thresholds = thresholds, label = label,
                               time = time, prevalence = prevalence, harm = NULL)
}

test_consequences_data_frame <- function(model_frame, outcome_name, outcome_type,
                                         statistics,
                                         thresholds = seq(0, 1, by = 0.25), label = NULL,
                                         time = NULL, prevalence = NULL, harm = NULL) {

  # for binary outcomes, make the outcome a factor
  # so both levels always appear in `table()` results
  outcome_type <- .outcome_type(model_frame, outcome_name, time)
  if (outcome_type == "binary") {
    model_frame[[outcome_name]] <-
      .convert_to_binary_fct(model_frame[[outcome_name]], quiet = FALSE)
  }

  # calculate initial set of diagnostic stats ----------------------------------
  df_results <-
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
            harm = .env$harm[[x]] %||% 0
          )
      }
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      label = factor(.data$label, levels = unique(.data$label)),
      neg_rate = 1 - .data$pos_rate,
      fn_rate = .data$pos_rate - .data$tp_rate,
      tn_rate = .data$neg_rate - .data$fp_rate
    ) %>%
    tibble::as_tibble()

  # add other stats, as requested
  if ("net_benefit" %in% statistics) {
    df_results$net_benefit =
      df_results$tp_rate - df_results$threshold /
      (1 - df_results$threshold) * df_results$fp_rate - df_results$harm
  }
  if ("test_neg_rate" %in% statistics) {
    df_results$test_neg_rate  <- df_results$fn_rate + df_results$tn_rate
  }
  if ("ppv" %in% statistics) {
    df_results$ppv  <- df_results$tp_rate / (df_results$tp_rate + df_results$fp_rate)
  }
  if ("npv" %in% statistics) {
    df_results$npv  <- df_results$tn_rate / (df_results$tn_rate + df_results$fn_rate)
  }
  if (any(c("sens", "lr_pos", "lr_neg") %in% statistics)) {
    df_results$sens  <- df_results$tp_rate / (df_results$tp_rate + df_results$fn_rate)
  }
  if (any(c("spec", "lr_pos", "lr_neg") %in% statistics)) {
    df_results$spec  <- df_results$tn_rate / (df_results$tn_rate + df_results$fp_rate)
  }
  if ("lr_pos" %in% statistics) {
    df_results$lr_pos  <- df_results$sens / (1 - df_results$spec)
  }
  if ("lr_neg" %in% statistics) {
    df_results$lr_neg  <- (1 - df_results$sens) / df_results$spec
  }

  df_results %>%
    dplyr::select(dplyr::all_of(c("variable", "label", "n", "threshold", statistics))) %>%
    dplyr::ungroup()
}


.outcome_type <- function(model_frame, outcome_name, time) {
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

  outcome_type
}

.check_probability_range <- function(model_frame, outcome_name) {
  for (v in names(model_frame) %>% setdiff(c(outcome_name, "all", "none"))) {
    if (any(!dplyr::between(model_frame[[v]], 0L, 1L))) {
      glue::glue("Error in {v}. All covariates/risks must be between 0 and 1.") %>%
        stop(call. = FALSE)
    }
  }
  return(invisible())
}

#' Calculate a test's consequences
#'
#' @param outcome outcome vector
#' @param risk vector of risks
#' @param thresholds threshold probs vector
#' @param outcome_type type of outcome
#' @param time time to calculate risk if Surv() outcome
#' @param prevalence specified prevalence (if cannot be estimated from data)
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
    df$pos_rate <- prevalence
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
    df$pos_rate <- outcome_prev
  }
  # typical binary prev
  else {
    df$pos_rate <- table(outcome)[2] / length(outcome)
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
          mean(risk[outcome == "TRUE"] >= .data$threshold) * .data$pos_rate %>%
          unname(),
        fp_rate =
          mean(risk[outcome == "FALSE"] >= .data$threshold) * (1 - .data$pos_rate) %>%
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
      "threshold", "n", "pos_rate",
      "test_pos_rate", "tp_rate", "fp_rate"
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
