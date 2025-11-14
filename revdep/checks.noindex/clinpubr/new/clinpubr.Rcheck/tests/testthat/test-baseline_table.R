library(withr)
library(survival)

test_that("get_var_types correctly classifies variables", {
  data(cancer, package = "survival")

  with_tempdir({
    set.seed(1)
    res <- get_var_types(cancer, strata = "sex", save_qqplots = TRUE)

    expect_s3_class(res, "var_types")
    expect_true(dir.exists("qqplots"))
    expect_equal(length(list.files("qqplots")), 14)

    expect_snapshot(res)
  })
})

test_that("baseline_table generates correct output files 2", {
  skip_if_not_installed("tableone")
  with_tempdir({
    set.seed(1)
    var_types <- get_var_types(mtcars, strata = "vs") # Automatically infer variable types
    baseline_table(mtcars, var_types = var_types, contDigits = 1, seed = 1, save_table = TRUE,
                   filename = "baseline.csv")

    expect_snapshot(read.csv("baseline.csv", check.names = FALSE))
    expect_snapshot(read.csv("baseline_missing.csv", check.names = FALSE))
  })
})

test_that("baseline_table generates correct output files", {
  skip_if_not_installed("tableone")
  skip_if_not_installed("rstatix")
  data(cancer, package = "survival")
  cancer$ph.ecog_cat <- factor(cancer$ph.ecog, levels = c(0:3), labels = c("0", "1", ">=2", ">=2"))

  with_tempdir({
    set.seed(1)
    var_types <- get_var_types(cancer, strata = "ph.ecog_cat")
    baseline_table(cancer, var_types = var_types, contDigits = 1, seed = 1, save_table = TRUE,
                   filename = "test_output.csv")

    expect_true(file.exists("test_output.csv"))
    expect_true(file.exists("test_output_missing.csv"))
    expect_true(file.exists("test_output_pairwise.csv"))

    expect_snapshot(read.csv("test_output.csv", check.names = FALSE))
    expect_snapshot(read.csv("test_output_missing.csv", check.names = FALSE))
    expect_snapshot(read.csv("test_output_pairwise.csv", check.names = FALSE))
  })
})

test_that("alpha_by_n calculates appropriate thresholds", {
  expect_equal(alpha_by_n(50), 0.05)
  set.seed(1)
  expect_snapshot(alpha_by_n(500))
})
