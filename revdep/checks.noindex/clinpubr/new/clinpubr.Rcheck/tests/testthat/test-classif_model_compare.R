library(withr)
library(survival)

test_that("classif_model_compare outputs correct files", {
  skip_if_not_installed("pROC")
  skip_if_not_installed("caret")
  skip_if_not_installed("dcurves")
  skip_if_not_installed("ResourceSelection")
  data(cancer, package = "survival")
  df <- cancer
  df$status <- ifelse(df$status == 1, 0, 1)
  df$model1 <- runif(nrow(df))
  df$model2 <- df$model1 * 0.8 + 0.1

  with_tempdir({
    classif_model_compare(df, "status", c("model1", "model2"), save_output = TRUE,
                          output_prefix = "test")

    expect_true(file.exists("test_table.csv"))
    expect_true(file.exists("test_dca.png"))
    expect_true(file.exists("test_roc.png"))
    expect_true(file.exists("test_calibration.png"))

    tbl <- read.csv("test_table.csv")
    expect_equal(nrow(tbl), 2)
    expect_true(all(c("AUC", "Accuracy", "Sensitivity") %in% names(tbl)))
  })
})

test_that("save_output=FALSE suppresses file generation", {
  skip_if_not_installed("pROC")
  skip_if_not_installed("caret")
  skip_if_not_installed("dcurves")
  skip_if_not_installed("ResourceSelection")
  
  set.seed(123)
  data <- data.frame(
    target = factor(rbinom(100, 1, 0.5)),
    model = runif(100)
  )

  res <- classif_model_compare(data, "target", "model", save_output = FALSE)

  expect_false(file.exists("model_compare_table.csv"))
  expect_false(file.exists("model_compare_dca.png"))
  expect_named(res, c("metric_table", "dca_plot", "roc_plot", "pr_plot", "calibration_plot"))

  vdiffr::expect_doppelganger("dca-plot", res$dca_plot)
  vdiffr::expect_doppelganger("roc-plot", res$roc_plot)
  vdiffr::expect_doppelganger("pr-plot", res$pr_plot)
  vdiffr::expect_doppelganger("calibration-plot", res$calibration_plot)
  expect_snapshot(res$metric_table)
})

test_that("as_probability handles value conversion", {
  skip_if_not_installed("pROC")
  skip_if_not_installed("caret")
  skip_if_not_installed("dcurves")
  skip_if_not_installed("ResourceSelection")
  df <- data.frame(
    target = factor(rbinom(100, 1, 0.5)),
    modelA = runif(100),
    modelB = runif(100) * 5 - 2
  )

  res1 <- classif_model_compare(df, "target", "modelA",
    as_probability = TRUE,
    save_output = FALSE
  )
  expect_true(all(res1$metric_table$Brier <= 1))

  expect_error(
    classif_model_compare(df, "target", "modelB", as_probability = FALSE),
    "not in range 0 to 1"
  )
})

test_that("parameter validation works", {
  skip_if_not_installed("pROC")
  skip_if_not_installed("caret")
  skip_if_not_installed("dcurves")
  skip_if_not_installed("ResourceSelection")
  df <- data.frame(y = factor(c(1, 0)), m1 = c(0.5, 0.5))

  expect_error(classif_model_compare(df, "y", "invalid_model"))

  df$y_num <- as.numeric(df$y)
  expect_error(classif_model_compare(df, "y_num", "m1"))
})
