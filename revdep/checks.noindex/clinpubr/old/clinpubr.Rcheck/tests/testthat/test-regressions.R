library(survival)
library(dplyr)
library(vdiffr)

# Test data setup
data(cancer, package = "survival")
cancer$dead <- cancer$status == 2

# Helper function to check file existence
check_file_exists <- function(filename) {
  expect_true(file.exists(filename))
}

# Test regression_basic_results - Cox model
test_that("regression_basic_results works for Cox regression", {
  withr::with_tempdir({
    results <- regression_basic_results(
      cancer,
      x = "age", y = "status", time = "time", save_output = FALSE,
      model_covs = list(Crude = c(), Model1 = c("ph.karno"))
    )
    expect_true(is.list(results))
    expect_named(results, c("table", "plots"))
    expect_snapshot(results$table)
    vdiffr::expect_doppelganger("cox_kmplot_quartile", results$plots$x.quartile)
    vdiffr::expect_doppelganger("cox_kmplot_median", results$plots$x.median)
    # Test file output
    p <- regression_basic_results(
      cancer,
      x = "age", y = "status", time = "time",
      model_covs = list(Crude = c()), save_output = TRUE
    )
    check_file_exists("table_cox_status_by_age.csv")
    check_file_exists("kmplot_cox_status_by_age_group_x.quartile.png")
  })
})

# Test regression_basic_results - Logistic model
test_that("regression_basic_results works for logistic regression", {
  withr::with_tempdir({
    results <- regression_basic_results(
      cancer,
      x = "age", y = "dead", save_output = FALSE,
      model_covs = list(Crude = c(), Model1 = c("ph.karno"))
    )
    expect_true(is.list(results))
    expect_named(results, c("table", "plots"))
    expect_snapshot(results$table)

    # Test file output
    p <- regression_basic_results(
      cancer,
      x = "age", y = "dead",
      model_covs = list(Crude = c()), save_output = TRUE
    )
    check_file_exists("table_logistic_dead_by_age.csv")
  })
})

test_that("regression_basic_results works for linear regression", {
  set.seed(1)
  results <- regression_basic_results(
    cancer,
    x = "age", y = "wt.loss",
    model_covs = list(Crude = c()), save_output = FALSE
  )
  expect_snapshot(results$table)
})

# Test error handling
test_that("regression_basic_results throws errors for invalid inputs", {
  expect_error(
    regression_basic_results(cancer, x = "age", y = "status", model_covs = c("age"), save_output = FALSE),
    "conflict of model variables!"
  )
  expect_error(
    regression_basic_results(cancer, x = "age", y = "status", model_covs = 123, save_output = FALSE),
    "`model_covs` should be a character vector of covariates or a named list of covariates of multiple models."
  )
})

# Test regression_forest
test_that("regression_forest generates forest plot", {
  withr::with_tempdir({
    p <- regression_forest(
      cancer,
      model_vars = c("age", "sex", "wt.loss"), y = "status", time = "time",
      as_univariate = TRUE, save_plot = FALSE
    )
    expect_true(inherits(p, "gtable"))
    expect_s3_class(p, "gtable")
    # vdiffr::expect_doppelganger("forest_plot_univariate", p)


    p2 <- regression_forest(
      cancer,
      model_vars = c("age", "sex", "wt.loss"), y = "status", time = "time",
      as_univariate = FALSE, save_plot = FALSE
    )
    expect_s3_class(p2, "gtable")
    # vdiffr::expect_doppelganger("forest_plot_multivariate", p2)

    p3 <- regression_forest(
      cancer,
      model_vars = list(
        Crude = c("age"),
        Model1 = c("age", "sex", "ph.karno"),
        Model2 = c("age", "sex", "ph.karno", "wt.loss")
      ),
      y = "status", time = "time",
      save_plot = TRUE, filename = "forest_plot.png"
    )
    expect_s3_class(p3, "gtable")
    check_file_exists("forest_plot.png")
    # vdiffr::expect_doppelganger("forest_plot_multiple_models", p3)
  })
})

# Test regression_scan
test_that("regression_scan returns expected structure", {
  set.seed(1)
  data(cancer, package = "survival")
  withr::with_tempdir({
    res <- regression_scan(cancer, y = "status", time = "time", save_table = TRUE)
    expect_true(is.data.frame(res))
    expect_true(all(c(
      "predictor", "nvalid", "original.HR", "original.pval", "original.padj",
      "logarithm.HR", "logarithm.pval", "logarithm.padj", "categorized.HR",
      "categorized.pval", "categorized.padj", "rcs.overall.pval",
      "rcs.overall.padj", "rcs.nonlinear.pval", "rcs.nonlinear.padj",
      "best.var.trans"
    ) %in% colnames(res)))
    expect_snapshot(res)
    check_file_exists("cox_regression_scan_status.csv")
  })
})

# Test regression_fit
test_that("regression_fit returns model results", {
  set.seed(1)
  data(cancer, package = "survival")
  fit <- regression_fit(data = cancer, y = "status", predictor = "age", time = "time")
  expect_s3_class(fit, "data.frame")
  expect_snapshot(fit)
})

# Test fit_model
test_that("fit_model handles different analysis types", {
  set.seed(1)
  data(cancer, package = "survival")
  # Cox model
  fit_cox <- fit_model(Surv(time, status) ~ age, data = cancer, analysis_type = "cox")
  expect_s3_class(fit_cox, "coxph")
  expect_snapshot(summary(fit_cox)$coefficients)

  fit_cox <- fit_model(Surv(time, status) ~ age, data = cancer, analysis_type = "cox", rms = TRUE)
  expect_s3_class(fit_cox, "cph")
  expect_snapshot(anova(fit_cox))

  # Logistic model
  cancer$dead <- cancer$status == 2
  fit_logistic <- fit_model(dead ~ age, data = cancer, analysis_type = "logistic")
  expect_s3_class(fit_logistic, "glm")
  expect_snapshot(summary(fit_logistic)$coefficients)
})

test_that("regression_fit works with time2", {
  set.seed(123)
  data(cancer, package = "survival")
  # Create a dummy time2 variable
  cancer$time2 <- cancer$time + runif(nrow(cancer), 0, 100)

  # Test cox model with time2
  fit_time2 <- regression_fit(
    data = cancer, y = "status", predictor = "age",
    time = "time", time2 = "time2", returned = "full"
  )

  # Manually create formula and fit model to compare
  fit_manual <- survival::coxph(Surv(time, time2, status) ~ age, data = cancer)

  expect_s3_class(fit_time2, "data.frame")
  expect_equal(fit_time2$estimate, as.vector(exp(coef(fit_manual))), tolerance = 1e-6)
})

test_that("regression_fit works cluster", {
  set.seed(123)
  data(cancer, package = "survival")
  # Create a dummy time2 variable
  cancer$time2 <- cancer$time + runif(nrow(cancer), 0, 100)

  # Test cox model with time2
  fit_cluster <- regression_fit(
    data = cancer, y = "status", predictor = "age",
    time = "time", time2 = "time2", returned = "full", cluster = "inst"
  )

  expect_s3_class(fit_cluster, "data.frame")
  expect_snapshot(fit_cluster)

  cancer$status = cancer$status-1
  fit_cluster2 <- regression_fit(
    data = cancer, y = "status", predictor = "age", rcs_knots = 4,
    time = NULL, time2 = NULL, returned = "full", cluster = "inst"
  )

  expect_s3_class(fit_cluster2, "data.frame")
  expect_snapshot(fit_cluster2)
})

test_that("regression_basic_results works with time2", {
  set.seed(123)
  data(cancer, package = "survival")
  # Create a dummy time2 variable
  cancer$time2 <- cancer$time + runif(nrow(cancer), 0, 100)

  results <- regression_basic_results(
    data = cancer, x = "age", y = "status",
    time = "time", time2 = "time2", save_output = FALSE
  )

  expect_type(results, "list")
  expect_true("table" %in% names(results))
  expect_s3_class(results$table, "data.frame")
  # Check if HR is calculated
  expect_true(any(grepl("HR", results$table[1, ])))
})

test_that("regression_forest works with time2", {
  set.seed(123)
  data(cancer, package = "survival")
  # Create a dummy time2 variable
  cancer$time2 <- cancer$time + runif(nrow(cancer), 0, 100)

  plot <- regression_forest(
    data = cancer,
    model_vars = c("age", "sex"),
    y = "status",
    time = "time",
    time2 = "time2",
    save_plot = FALSE
  )

  expect_s3_class(plot, "gtable")
})

test_that("regression_scan works with time2", {
  set.seed(123)
  data(cancer, package = "survival")
  # Create a dummy time2 variable
  cancer$time2 <- cancer$time + runif(nrow(cancer), 0, 100)

  scan_results <- regression_scan(
    data = cancer,
    y = "status",
    time = "time",
    time2 = "time2",
    predictors = c("age", "sex"),
    save_table = FALSE
  )

  expect_s3_class(scan_results, "data.frame")
  expect_true("age" %in% scan_results$predictor)
  expect_true("sex" %in% scan_results$predictor)
  expect_true(any(grepl("HR", colnames(scan_results))))
})
