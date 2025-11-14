test_that("mad_outlier correctly identifies outliers", {
  # Basic functionality test with known outlier
  x <- c(1, 2, 3, 4, 5, 100)
  expect_equal(mad_outlier(x), c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))

  # Test with no outliers
  x <- 1:10
  expect_equal(all(mad_outlier(x) == FALSE), TRUE)

  # Test with custom constant
  x <- c(1, 2, 3, 4, 5, 10)
  expect_equal(mad_outlier(x, constant = 1.4826 * 10), c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
})

test_that("mad_outlier handles edge cases correctly", {
  # Test with single value
  x <- 5
  expect_equal(mad_outlier(x), FALSE)

  # Test with all identical values
  x <- rep(5, 10)
  expect_equal(all(mad_outlier(x) == FALSE), TRUE)

  # Test with NA values
  x <- c(1, 2, NA, 4, 5, 100)
  expect_equal(mad_outlier(x), c(FALSE, FALSE, NA, FALSE, FALSE, TRUE))
})
