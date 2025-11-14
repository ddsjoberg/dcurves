test_that("value_initial_cleaning correctly cleans numerical strings", {
  # Test case 1: Fullwidth to halfwidth conversion
  expect_equal(
    value_initial_cleaning("\uFF11\uFF12\uFF13"),
    "123"
  )

  # Test case 2: Multiple dots to single dot
  expect_equal(
    value_initial_cleaning("11..23"),
    "11.23"
  )

  # Test case 4: Mixed cases (actual fullwidth characters)
  expect_equal(
    value_initial_cleaning(c("\uFF11\uFF12..\uFF13", "..45", "  67  ")),
    c("12.3", ".45", "67")
  )

  # Test case 5: Empty string to NA
  expect_equal(
    value_initial_cleaning(""),
    NA_character_
  )
})

test_that("char_initial_cleaning correctly cleans strings", {
  # Test case 1: Fullwidth to halfwidth conversion
  expect_equal(
    char_initial_cleaning("\uFF11\uFF12\uFF13"),
    "123"
  )

  # Test case 2: Multiple dots to single dot
  expect_equal(
    char_initial_cleaning(c("\uFF11\uFF12.\uFF13", "..45", "  hello \t world  ")),
    c("12.3", "..45", "hello world")
  )

  # Test case 5: Empty string to NA
  expect_equal(
    char_initial_cleaning(""),
    NA_character_
  )
})