test_that("multi-column mode works", {
  dat <- data.frame(
    Q1 = c("A", "B", "C"),
    Q2.A = c(TRUE, TRUE, FALSE),
    Q2.B = c(TRUE, FALSE, TRUE),
    Q2.C = c(FALSE, TRUE, FALSE)
  )
  result <- answer_check(dat, c("A", "TFT"), multi_column = TRUE)
  expect_equal(result[, 1], c(TRUE, FALSE, FALSE))
  expect_equal(result[, 2], c(FALSE, TRUE, FALSE))
})

test_that("single-column mode works", {
  dat <- data.frame(
    Q1 = c("A", "B", "C"),
    Q2 = c("AD", "AE", "ABF")
  )
  result <- answer_check(dat, c("A", "AE"))
  expect_equal(result[, 1], c(TRUE, FALSE, FALSE))
  expect_equal(result[, 2], c(FALSE, TRUE, FALSE))
})

test_that("width not equal", {
  dat <- data.frame(Q1 = c("A", "B"))
  expect_error(answer_check(dat, c("A", "B")), "width not equal")
})

test_that("NA handled to be false", {
  dat <- data.frame(
    Q1 = c(NA, "B", "A")
  )
  result <- answer_check(dat, c("A"))
  expect_equal(result[, 1], c(FALSE, FALSE, TRUE))
})