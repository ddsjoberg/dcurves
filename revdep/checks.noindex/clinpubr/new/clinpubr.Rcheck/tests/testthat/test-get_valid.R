# Test basic functionality with different modes
test_that("get_valid returns correct values for different modes", {
  x <- c(NA, 1, 2, NA, 3, NA, 4)
  expect_equal(get_valid(x, mode = "first"), 1)
  expect_equal(get_valid(x, mode = "last"), 4)
  expect_equal(get_valid(x, mode = "mid"), 2)
})

# Test disjoint parameter
test_that("get_valid respects disjoint parameter", {
  x <- c(NA, 1, 2, 3, NA)
  # With disjoint = TRUE, mid should only return value if length > 2
  expect_equal(get_valid(x, mode = "mid", disjoint = TRUE), 2)
  # With shorter vector, disjoint should return NA for mid
  x_short <- c(NA, 1, NA)
  expect_equal(get_valid(x_short, mode = "mid", disjoint = TRUE), NA)
  # Ensure first and last are different when disjoint = TRUE
  x_disjoint <- c(NA, 1, 2, NA, 3)
  expect_true(get_valid(x_disjoint, mode = "first", disjoint = TRUE) != get_valid(x_disjoint, mode = "last", disjoint = TRUE))
})

# Test edge cases
test_that("get_valid handles edge cases", {
  # All NAs
  x_all_na <- rep(NA, 5)
  expect_equal(get_valid(x_all_na), NA)
  # Single valid value
  x_single <- c(NA, 5, NA)
  expect_equal(get_valid(x_single, mode = "first"), 5)
  expect_equal(get_valid(x_single, mode = "last"), 5)
  expect_equal(get_valid(x_single, mode = "mid"), 5)
  # Disjoint with single valid value should return NA for non-first modes
  expect_equal(get_valid(x_single, mode = "last", disjoint = TRUE), NA)
  expect_equal(get_valid(x_single, mode = "mid", disjoint = TRUE), NA)
})