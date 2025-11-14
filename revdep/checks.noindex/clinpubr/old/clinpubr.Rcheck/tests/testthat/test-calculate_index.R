test_that("Basic condition calculation", {
  df <- data.frame(x = c(1, 3, 5), y = c(2, NA, 4))
  result <- calculate_index(df, x > 2, y < 3)
  expect_equal(result, c(1, 1, 1))
})

test_that("Different weights and NA replacement", {
  df <- data.frame(a = c(NA, 2, 3), b = c(1, NA, 5))
  res <- calculate_index(df, a, b > 3,
    .weight = c(2, 3),
    .na_replace = c(0, 1)
  )
  expect_equal(res, c(0, 5, 9))
})

test_that("Parameter length validation", {
  df <- data.frame(x = 1:3)
  expect_error(calculate_index(df, x > 1, .weight = 1:2))
  expect_error(calculate_index(df, .weight = 1:3))
  expect_error(calculate_index(df, x > 1, .na_replace = 1:2))
})

test_that("Edge case handling", {
  # Test empty input
  expect_error(calculate_index(data.frame()))

  # Test all NA handling
  df <- data.frame(col1 = c(NA, NA), col2 = c(NA, 1))
  res <- calculate_index(df, col1 > 0, col2 < 1, .na_replace = 2)
  expect_equal(res, c(4, 2))
})
