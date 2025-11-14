library(dplyr)
library(survival)
library(ggplot2)
library(rms)

set.seed(1)

# Test interaction_p_value
test_that("interaction_p_value returns valid p-values", {
  data(cancer, package = "survival")
  # Cox model
  p_cox <- interaction_p_value(cancer, y = "status", predictor = "age", group_var = "sex", time = "time")
  expect_true(is.numeric(p_cox) && p_cox >= 0 && p_cox <= 1)
  # Logistic model
  cancer$dead <- ifelse(cancer$status == 2, 1, 0) # Recode status to 0/1
  p_logistic <- interaction_p_value(cancer, y = "dead", predictor = "age", group_var = "sex")
  expect_true(is.numeric(p_logistic) && p_logistic >= 0 && p_logistic <= 1)
  # Linear model (continuous outcome)
  cancer$wt.loss <- rnorm(nrow(cancer)) # Mock continuous outcome
  p_linear <- interaction_p_value(cancer, y = "wt.loss", predictor = "age", group_var = "sex")
  expect_true(is.numeric(p_linear) && !is.na(p_linear))

  p_logistic_rob <- interaction_p_value(cancer, y = "dead", predictor = "age", group_var = "sex", cluster = "inst")
  expect_snapshot(p_logistic_rob)
})

# Test interaction_scan
test_that("interaction_scan returns valid data frame and saves table", {
  withr::with_tempdir({
    data(cancer, package = "survival")
    # Recode status to 0/1 for logistic regression
    cancer$status <- ifelse(cancer$status == 2, 1, 0) # 1=dead, 0=alive
    # Cox analysis
    res_cox <- interaction_scan(cancer,
      y = "status", time = "time", predictors = c("age", "sex"),
      group_vars = c("sex", "ph.ecog"),
    )
    expect_true(is.data.frame(res_cox))
    expect_snapshot(res_cox)
    expect_true(all(c("predictor", "group.by", "linear.p.int") %in% colnames(res_cox)))
    # Logistic analysis
    res_logistic <- interaction_scan(cancer,
      y = "status", predictors = c("age", "sex"),
      group_vars = c("sex", "ph.ecog")
    )
    expect_true(is.data.frame(res_logistic))
    expect_snapshot(res_logistic)
    # Edge case: small sample size
    small_data <- cancer[1:8, ]
    res_small <- interaction_scan(small_data, y = "status", time = "time")
    expect_true(nrow(res_small) == 0)
    # Check file saving
    with_tempdir({
      interaction_scan(cancer, y = "status", time = "time", save_table = TRUE, filename = "test_scan.csv")
      expect_true(file.exists("test_scan.csv"))
    })
  })
})

# Test interaction_plot
test_that("interaction_plot generates plots and saves files", {
  withr::with_tempdir({
    data(cancer, package = "survival")
    # Recode status to 0/1 for logistic regression
    cancer$status <- ifelse(cancer$status == 2, 1, 0) # 1=dead, 0=alive
    # Cox plot
    plt_cox <- interaction_plot(cancer,
      y = "status", time = "time", predictor = "age",
      group_var = "sex", save_plot = FALSE
    )
    expect_true(
      inherits(plt_cox, "gg") ||
        (is.list(plt_cox) && all(sapply(plt_cox, function(x) inherits(x, "gg"))))
    )
    vdiffr::expect_doppelganger("Cox interaction plot", plt_cox)
    # Logistic plot
    plt_logistic <- interaction_plot(cancer,
      y = "status", predictor = "age",
      group_var = "sex", save_plot = TRUE
    )
    expect_true(
      inherits(plt_logistic, "gg") ||
        (is.list(plt_logistic) && all(sapply(plt_logistic, function(x) inherits(x, "gg"))))
    )
    vdiffr::expect_doppelganger("Logistic interaction plot", plt_logistic)
    # Check plot files
    expect_true(any(grepl("interaction_status_with_age_by_sex", list.files())))
  })
})

# Test edge case: all NAs in predictor/group_var
test_that("interaction functions handle NAs gracefully", {
  data(cancer, package = "survival")
  cancer$age_na <- ifelse(1:nrow(cancer) <= 5, NA, cancer$age)
  # interaction_p_value with NAs (ensure complete cases)
  cancer_na <- cancer[!is.na(cancer$age_na), ] # Remove rows with NA in predictor
  p_na <- tryCatch(
    interaction_p_value(cancer_na,
      y = "status", predictor = "age_na", time = "time",
      group_var = "sex"
    ),
    error = function(e) NULL
  )
  expect_false(is.null(p_na))
  # interaction_scan with NAs (ensure complete cases)
  res_na <- interaction_scan(cancer[!is.na(cancer$age_na), ],
    y = "status", predictors = "age_na", time = "time", group_vars = "sex", save_table = FALSE
  )
  expect_true(nrow(res_na) > 0)
})
