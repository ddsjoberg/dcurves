library(dplyr)

# Test get_samples function

test_that("get_samples returns correct samples", {
  set.seed(1)
  # Basic case
  expect_equal(get_samples(1:5, n_samples = 3), paste(c(1, 4, 3), collapse = "\n"))
  # Unique only
  expect_equal(get_samples(c(1, 1, 2, 3), unique_only = TRUE), paste(1:3, collapse = "\n"))
  # Collapse separator
  expect_equal(get_samples(1:3, collapse = ", "), "1, 2, 3")
  # Handle NAs
  expect_equal(get_samples(c(NA, 1, 2)), paste(1:2, collapse = "\n"))
})

# Test subject_view function

test_that("subject_view works with numeric value_col", {
  df <- data.frame(
    subject = rep(c("A", "B"), each = 5),
    value = c(rnorm(5), rnorm(5, mean = 10)),
    unit = rep(c("mg", "g"), each = 5)
  )
  result <- subject_view(df, subject_col = "subject", info_cols = "unit", value_col = "value")
  # Check basic structure
  expect_s3_class(result, "data.frame")
  expect_named(result, c("subject", "target_subject", "count", "nvalid", "mean", "sd", "median", "unit"))
  # Check count
  expect_equal(result$count, c(5, 5))
})

test_that("subject_view works with non-numeric value_col", {
  df <- data.frame(
    subject = rep(c("A", "B"), each = 5),
    value = rep(c("high", "low"), each = 5),
    unit = rep(c("mg", "g"), each = 5)
  )
  result <- subject_view(df, subject_col = "subject", info_cols = "unit")
  # Check structure without numeric stats
  expect_named(result, c("subject", "target_subject", "count", "unit"))
  # Check count
  expect_equal(result$count, c(5, 5))
})

test_that("subject_view handles edge cases", {
  # Empty data frame
  expect_error(subject_view(data.frame(), subject_col = "subject", info_cols = "unit"))
  # All NA values
  df <- data.frame(subject = "A", value = NA, unit = NA)
  result <- subject_view(df, subject_col = "subject", info_cols = "unit", value_col = "value")
  expect_null(result$nvalid)
})
