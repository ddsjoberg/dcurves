test_that("Basic non-numeric detection", {
  input <- c("12.3", "45..6", "78a9", NA, "\uFF11\uFF12", "987", "45..6")

  # Default parameters
  expect_equal(check_nonnum(input), c("45..6", "78a9", "\uFF11\uFF12"))

  # return_idx = TRUE
  expect_equal(check_nonnum(input, return_idx = TRUE)$idx, c(2, 3, 5, 7))

  # show_unique = FALSE
  expect_equal(check_nonnum(input, show_unique = FALSE), c("45..6", "78a9", "\uFF11\uFF12", "45..6"))
})

test_that("Parameter combinations", {
  input <- c("a", "a", "b", "c")

  # return_idx
  res <- check_nonnum(input, return_idx = TRUE, show_unique = FALSE)
  expect_type(res, "list")
  expect_named(res, c("value", "idx"))

  # show_unique
  expect_length(check_nonnum(input, show_unique = TRUE), 3)

  # random_sample
  expect_length(check_nonnum(input, show_unique = TRUE, max_count = 2, random_sample = TRUE), 2)
})

test_that("Edge cases handling", {
  # Empty input
  expect_length(check_nonnum(character(0)), 0)

  # All NA
  expect_length(check_nonnum(c(NA, NA_character_)), 0)

  # All valid
  expect_length(check_nonnum(c("1.2", "3")), 0)
})

test_that("df_view_nonnum returns correct non-numeric entries", {
  # Create test data frame with non-numeric values
  df <- data.frame(
    col1 = c("12.3", "45..6", "78a9", NA, "987"),
    col2 = c("valid", "12.3a", "\uFF11\uFF12", "ok", "45..6"),
    stringsAsFactors = FALSE
  )

  # Test default parameters (max_count=20, random_sample=FALSE)
  result <- df_view_nonnum(df)
  expect_s3_class(result, "data.frame")
  # Include all non-numeric entries (valid, ok are non-numeric)
  # Expect non-numeric entries followed by NAs (due to other columns having more entries)
  expect_equal(result$col1, c("45..6", "78a9", NA, NA, NA))  # NA is excluded
  expect_equal(result$col2, c("valid", "12.3a", "\uFF11\uFF12", "ok", "45..6"))

  # Test max_count=2
  result <- df_view_nonnum(df, max_count = 2)
  expect_equal(nrow(result), 2)
  expect_equal(result$col1, c("45..6", "78a9"))  # First 2 entries
  # First 2 non-numeric entries in col2
  expect_equal(result$col2, c("valid", "12.3a"))  # First 2 entries

  # Test random_sample=TRUE with set.seed for consistency
  set.seed(1)
  result_random <- df_view_nonnum(df, max_count = 2, random_sample = TRUE)
  set.seed(1)
  expected_sample <- sample(c("12.3a", "\uFF11\uFF12", "45..6"), 2)
  # Include all non-numeric values in sampling pool
  expect_true(all(result_random$col2 %in% c("valid", "12.3a", "\uFF11\uFF12", "ok", "45..6")))
})


test_that("df_view_nonnum returns correct non-numeric entries for long df", {
  df <- data.frame(
    subject = c("A", "A", "A", "A", "B", "B", "B", "B", "B"),
    val = c("12.3", "45..6", "78a9", NA, "valid", "12.3a", "\uFF11\uFF12", "ok", "45..6")
  )

  # Test default parameters (max_count=20, random_sample=FALSE)
  result <- df_view_nonnum(df, long_df = TRUE)
  expect_s3_class(result, "data.frame")
  # Include all non-numeric entries (valid, ok are non-numeric)
  # Expect non-numeric entries followed by NAs (due to other columns having more entries)
  expect_equal(result$A, c("45..6", "78a9", NA, NA, NA))  # NA is excluded
  expect_equal(result$B, c("valid", "12.3a", "\uFF11\uFF12", "ok", "45..6"))

  # Test max_count=2
  result <- df_view_nonnum(df, max_count = 2, long_df = TRUE, subject_col = "subject", value_col = "val")
  expect_equal(nrow(result), 2)
  expect_equal(result$A, c("45..6", "78a9"))  # First 2 entries
  # First 2 non-numeric entries in col2
  expect_equal(result$B, c("valid", "12.3a"))  # First 2 entries

  # Test random_sample=TRUE with set.seed for consistency
  set.seed(1)
  result_random <- df_view_nonnum(df, max_count = 2, random_sample = TRUE, long_df = TRUE)
  set.seed(1)
  expected_sample <- sample(c("12.3a", "\uFF11\uFF12", "45..6"), 2)
  # Include all non-numeric values in sampling pool
  expect_true(all(result_random$B %in% c("valid", "12.3a", "\uFF11\uFF12", "ok", "45..6")))
})

test_that("df_view_nonnum handles edge cases", {
  # Empty data frame
  expect_length(df_view_nonnum(data.frame()), 0)

  # Data frame with no non-numeric values
  df_clean <- data.frame(col1 = c("12.3", "45.6"), col2 = c("78.9", "101.1"))
  result <- df_view_nonnum(df_clean)
  expect_length(result, 0)

  # max_count = NULL (use all rows)
  df <- data.frame(col = c("a", "b", "c", "d"))
  result <- df_view_nonnum(df, max_count = NULL)
  expect_equal(nrow(result), 4)
})
