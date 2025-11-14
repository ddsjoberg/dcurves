# Test numerical Excel dates
test_that("numerical Excel dates convert correctly", {
  expect_equal(to_date("44197"), as.Date("2021-01-01"))
})

# Test character date formats
test_that("character dates convert correctly", {
  dates <- c("2020-01-01", "2020/01/01", "20200101", "2020.01.01")
  expected <- rep(as.Date("2020-01-01"), 4)
  expect_equal(to_date(dates), expected)
})

# Test mixed input types
test_that("mixed types convert correctly", {
  mixed <- c(44197, "2020-01-01", "2020/01/01")
  expected <- as.Date(c("2021-01-01", "2020-01-01", "2020-01-01"))
  expect_equal(to_date(mixed), expected)
})

# Test NA handling
test_that("invalid inputs return NA", {
  invalid <- c("2020-13-01", "not_a_date", 123456789)
  result <- to_date(invalid)
  expect_true(all(is.na(result)))
})

# Test failure reporting
test_that("fails to process prints messages", {
  expect_message(to_date(c("invalid_date", "2020-01-01"), verbose = TRUE),
                 "cannot process:invalid_date")
})

# Test non-Excel numerical dates
test_that("non-Excel numerical dates convert correctly", {
  expect_equal(to_date(18262, from_excel = FALSE), as.Date(18262, origin = "1970-01-01"))
})
