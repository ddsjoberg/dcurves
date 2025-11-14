library(survival)
library(withr)

test_that("max_missing_rates calculates correctly", {
  # Test with example data
  data(cancer)
  mmr <- max_missing_rates(cancer)
  expect_true(is.numeric(mmr$row))
  expect_true(is.numeric(mmr$col))

  # Test with all NAs
  df_na <- data.frame(a = rep(NA, 5), b = rep(NA, 5))
  mmr_na <- max_missing_rates(df_na)
  expect_equal(mmr_na$row, 1)
  expect_equal(mmr_na$col, 1)

  # Test with no NAs
  df_no_na <- data.frame(a = 1:5, b = 6:10)
  mmr_no_na <- max_missing_rates(df_no_na)
  expect_equal(mmr_no_na$row, 0)
  expect_equal(mmr_no_na$col, 0)
})

test_that("get_valid_subset returns correct subset", {
  set.seed(1) # For reproducibility
  data(cancer)

  # Basic test with example parameters
  subset1 <- get_valid_subset(cancer, row_na_ratio = 0.2, col_na_ratio = 0.1, row_priority = 1)
  expect_true(is.data.frame(subset1))
  expect_true(nrow(subset1) <= nrow(cancer))
  expect_true(ncol(subset1) <= ncol(cancer))

  # Check max missing rates after subset
  mmr_subset1 <- max_missing_rates(subset1)
  expect_true(mmr_subset1$row <= 0.2)
  expect_true(mmr_subset1$col <= 0.1)

  # Test return_index = TRUE
  indices <- get_valid_subset(cancer, row_na_ratio = 0.2, col_na_ratio = 0.1, row_priority = 1, return_index = TRUE)
  expect_true(is.list(indices))
  expect_true(all(indices$rows %in% seq_len(nrow(cancer))))
  expect_true(all(indices$cols %in% seq_len(ncol(cancer))))

  # Edge case: all rows/columns exceed NA ratio (should return empty)
  df_high_na <- data.frame(matrix(NA, nrow = 10, ncol = 5))
  subset_empty <- get_valid_subset(df_high_na, row_na_ratio = 0.1, col_na_ratio = 0.1)
  # Check that either rows or columns are empty, and missing rates are within limits
  expect_true(nrow(subset_empty) == 0 || ncol(subset_empty) == 0)

  # Edge case: no rows/columns exceed NA ratio (should return original)
  df_low_na <- data.frame(matrix(rnorm(50), nrow = 10, ncol = 5)) # No NAs
  subset_full <- get_valid_subset(df_low_na, row_na_ratio = 0.5, col_na_ratio = 0.5)
  expect_equal(dim(subset_full), dim(df_low_na))
})
