test_that("extract_num handles basic cases", {
  x <- c("1.2(XXX)", "5-8POS", "NS", "FULL", "5.5", "4.2")
  expect_equal(extract_num(x), c(1.2, 5, NA, NA, 5.5, 4.2))

  res <- extract_num(c("3-5", "6"), res_type = "first", multimatch2na = TRUE)
  expect_equal(res, c(NA, 6))

  expect_equal(extract_num(c("0.5kg", "2.0lbs"), leq_1 = TRUE), c(0.5, NA))

  expect_equal(extract_num(c("NS", "5"), zero_regexp = "NS"), c(0, 5))
})

test_that("range mode works correctly", {
  x <- c("3-5cm", "7.2mm", "1|3kg")

  expect_equal(extract_num(x, "range", allow_neg = FALSE), c(4, 7.2, 2))

  res <- extract_num(c("FULL", "5"), max_regexp = "FULL", max_quantile = 1)
  expect_equal(res, c(5, 5))
})
