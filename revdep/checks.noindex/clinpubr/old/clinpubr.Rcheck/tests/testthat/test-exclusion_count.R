# Sample data for testing
cohort <- data.frame(
  id = 1:6,
  age = c(17, 25, 30, 40, 50, 60),
  sex = c("M", "F", "F", "M", "F", "M"),
  value = c(1, NA, 3, 4, 5, NA),
  dementia = c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)
)

test_that("exclusion_count works with specified criteria names", {
  result <- exclusion_count(
    cohort,
    age < 18,
    is.na(value),
    dementia == TRUE,
    .criteria_names = c(
      "Age < 18 years",
      "Missing value",
      "History of dementia"
    )
  )

  expected <- data.frame(
    Criteria = c(
      "Initial N",
      "Age < 18 years",
      "Missing value",
      "History of dementia",
      "Final N"
    ),
    N = c(6, 1, 2, 1, 2),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)
})

test_that("exclusion_count works with default criteria names", {
  result <- exclusion_count(
    cohort,
    age < 18,
    is.na(value)
  )

  expected <- data.frame(
    Criteria = c(
      "Initial N",
      "age < 18",
      "is.na(value)",
      "Final N"
    ),
    N = c(6, 1, 2, 3),
    stringsAsFactors = FALSE
  )
  expect_equal(result, expected)
})

test_that("exclusion_count handles NA in criteria correctly", {
  df_na <- data.frame(x = c(1, 2, NA, 4))
  expect_warning(result <- exclusion_count(df_na, x > 2, .criteria_names = "X > 2"))
  expected <- data.frame(
    Criteria = c("Initial N", "X > 2", "Final N"),
    N = c(4, 2, 2),
    stringsAsFactors = FALSE
  )
  expect_equal(result, expected)
})

test_that("exclusion_count throws error for no criteria", {
  expect_error(
    exclusion_count(cohort),
    "At least one exclusion criterion must be provided."
  )
})

test_that("exclusion_count throws error for criteria name length mismatch", {
  expect_error(
    exclusion_count(cohort, age < 18, .criteria_names = c("a", "b")),
    "`.criteria_names` must have the same length as the number of criteria."
  )
})
