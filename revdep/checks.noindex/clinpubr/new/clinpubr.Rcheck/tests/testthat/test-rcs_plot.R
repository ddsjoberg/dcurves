library(ggplot2)
library(rms)
library(survival)
library(withr)

# Load test data
data(cancer, package = "survival")
cancer$dead <- cancer$status == 2 # Binary outcome for logistic model

set.seed(1) # For reproducibility

test_that("rcs_plot works for Cox model", {
  withr::with_tempdir({
    result <- rcs_plot(cancer, x = "age", y = "status", time = "time", covars = "ph.karno", save_plot = TRUE)
    expect_s3_class(result, "gg")
    vdiffr::expect_doppelganger("Cox model RCS plot", result)
    expect_true(file.exists(list.files(pattern = ".png")))
  })
})

test_that("rcs_plot works for linear model", {
  withr::with_tempdir({
    result <- rcs_plot(cancer, x = "age", y = "wt.loss", covars = "ph.karno")
    expect_s3_class(result, "gg")
    vdiffr::expect_doppelganger("Linear model RCS plot", result)

    # Test with return_details
    details <- rcs_plot(cancer, x = "age", y = "wt.loss", covars = "ph.karno", save_plot = FALSE, return_details = TRUE)
    expect_named(details, c(
      "aics", "knot", "n.valid", "n.plot", "phassump", "phresidual",
      "pvalue_all", "pvalue_nonlin", "ref", "plot"
    ))
  })
})

test_that("rcs_plot works for logistic model", {
  withr::with_tempdir({
    result <- rcs_plot(cancer, x = "age", y = "dead", covars = "ph.karno", save_plot = FALSE)
    vdiffr::expect_doppelganger("Logistic model RCS plot", result)
    expect_s3_class(result, "gg")
    expect_false(any(grepl(".png", list.files())))
  })
})

test_that("rcs_plot handles custom knots", {
  result <- rcs_plot(cancer, x = "age", y = "dead", knot = 5, return_details = TRUE, save_plot = FALSE)
  expect_equal(result$knot, 5)
  expect_s3_class(result$plot, "gg")
  vdiffr::expect_doppelganger("Logistic model RCS plot 5 knots", result$plot)
})

test_that("rcs_plot finds best knots", {
  result <- rcs_plot(cancer, x = "age", y = "dead", knot = NULL, return_details = TRUE, save_plot = FALSE)
  expect_snapshot(result$aics)
  expect_equal(result$knot, 5)
})

test_that("rcs_plot warns about missing data", {
  cancer_missing <- cancer
  cancer_missing$age[1:5] <- NA
  expect_warning(rcs_plot(cancer_missing, x = "age", y = "dead", save_plot = FALSE), "incomplete cases excluded")
})

test_that("rcs_plot returns details when requested", {
  details <- rcs_plot(cancer, x = "age", y = "dead", return_details = TRUE, save_plot = FALSE)
  expect_named(details, c("aics", "knot", "n.valid", "n.plot", "phassump", "phresidual", "pvalue_all", "pvalue_nonlin", "ref", "plot"))
  # Exclude plot from snapshot as it's tested with vdiffr
  details_no_plot <- details[!names(details) %in% "plot"]
  expect_snapshot(details_no_plot)
})


test_that("rcs_plot handles custom y_min and y_max", {
  result <- rcs_plot(cancer, x = "age", y = "status", time = "time", y_lim = c(0.5, 2.5), save_plot = FALSE)
  expect_s3_class(result, "gg")
  # Verify y limits
  y_limits <- ggplot2::ggplot_build(result)$layout$panel_params[[1]]$y.range
  expect_equal(y_limits, c(0.5, 2.5))
  vdiffr::expect_doppelganger("cox model RCS plot y 0.5 to 2.5", result)
})

test_that("rcs_plot handles custom transformation", {
  result <- rcs_plot(cancer,
    x = "age", y = "dead",
    covars = "ph.karno", save_plot = FALSE, add_hist = F, trans = "log10"
  )
  expect_s3_class(result, "gg")
  vdiffr::expect_doppelganger("logistic model RCS plot y log10 transformed", result)
})

test_that("rcs_plot uses different ref values", {
  # Test ref = x_mean
  result_mean <- rcs_plot(cancer, x = "age", y = "dead", ref = "x_mean", save_plot = FALSE)
  vdiffr::expect_doppelganger("logistic model RCS plot ref mean", result_mean)
  # Test ref = numeric value
  result_num <- rcs_plot(cancer, x = "age", y = "dead", ref = 60, save_plot = FALSE)
  vdiffr::expect_doppelganger("logistic model RCS plot ref 60", result_num)

  result <- rcs_plot(cancer, x = "age", y = "dead", group_by_ref = FALSE, save_plot = FALSE)
  vdiffr::expect_doppelganger("logistic model RCS plot no group", result)
})


test_that("break_at generates correct breaks", {
  # Test ref_val included in breaks
  bks <- break_at(xlim = c(0, 10), breaks = 5, ref_val = 3)
  expect_true(3 %in% bks)
  # Test breaks cover xlim
  expect_equal(bks, c(-2.0, 0.5, 3.0, 5.5, 8.0, 10.5))
})


test_that("filter_rcs_predictors filters correctly", {
  # Create test data with numeric and non-numeric predictors
  test_data <- data.frame(
    numeric1 = 1:10,
    numeric2 = rep(1, 10), # Only 1 unique value
    factor1 = factor(letters[1:10]),
    numeric3 = rnorm(10)
  )
  # Should return numeric1 and numeric3 (numeric with >5 unique values)
  filtered <- filter_rcs_predictors(test_data, predictors = c("numeric1", "numeric2", "factor1", "numeric3"))
  expect_equal(sort(filtered), c("numeric1", "numeric3"))
})
