set.seed(1)

# Test combine_multichoice function

test_that("combine_multichoice handles backward compatibility with vector input", {
  df <- data.frame(
    q1 = c(TRUE, FALSE, TRUE, NA),
    q2 = c(FALSE, FALSE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  result <- combine_multichoice(df,
    quest_cols = c("q1", "q2"),
    remove_prefix = FALSE
  )
  expect_equal(result$combined_multichoice, c("q1", "", "q1,q2", "q2"))
})

test_that("combine_multichoice creates multiple columns with named list", {
  df <- data.frame(
    a1 = c(TRUE, FALSE, TRUE), a2 = c(FALSE, TRUE, TRUE),
    b1 = c(TRUE, TRUE, FALSE), b2 = c(FALSE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  result <- combine_multichoice(df,
    quest_cols = list(groupA = c("a1", "a2"), groupB = c("b1", "b2")),
    remove_prefix = FALSE
  )
  expect_equal(result$groupA, c("a1", "a2", "a1,a2"))
  expect_equal(result$groupB, c("b1", "b1", "b2"))
  expect_false(any(c("a1", "a2", "b1", "b2") %in% colnames(result)))
})

test_that("remove_prefix removes common prefixes from column names", {
  df <- data.frame(
    Q1_a = c(TRUE, FALSE, TRUE), Q1_b = c(FALSE, TRUE, TRUE),
    Q2_x = c(TRUE, TRUE, FALSE), Q2_y = c(FALSE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  result <- combine_multichoice(
    df,
    quest_cols = list(group1 = c("Q1_a", "Q1_b"), group2 = c("Q2_x", "Q2_y"))
  )
  expect_equal(result$group1, c("a", "b", "a,b"))
  expect_equal(result$group2, c("x", "x", "y"))
})

test_that("remove_prefix handles no common prefix scenario", {
  df <- data.frame(
    apple = c(TRUE, FALSE), banana = c(FALSE, TRUE),
    cherry = c(TRUE, TRUE), stringsAsFactors = FALSE
  )
  result <- combine_multichoice(
    df,
    quest_cols = list(fruits = c("apple", "banana", "cherry"))
  )
  # Should use original names when no common prefix
  expect_equal(result$fruits, c("apple,cherry", "banana,cherry"))
})

test_that("remove_prefix preserves original names when trimming results in empty strings", {
  df <- data.frame(
    ID1 = c(TRUE, FALSE), ID2 = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  result <- combine_multichoice(
    df,
    quest_cols = list(ids = c("ID1", "ID2"))
  )
  # Common prefix is "ID", trimming gives "1" and "2"
  expect_equal(result$ids, c("1", "2"))
})

test_that("combine_multichoice errors on unnamed list elements", {
  df <- data.frame(q1 = c(TRUE, FALSE), q2 = c(FALSE, TRUE))
  expect_error(
    combine_multichoice(df, quest_cols = list(c("q1", "q2"))),
    "All elements of quest_cols must be named"
  )
})

test_that("combine_multichoice errors on missing columns in any group", {
  df <- data.frame(a1 = c(TRUE, FALSE), b1 = c(FALSE, TRUE))
  expect_error(
    combine_multichoice(df, quest_cols = list(groupA = c("a1", "a2"), groupB = c("b1", "b2"))),
    "The following columns are not present in the data frame: a2, b2"
  )
})

test_that("combine_multichoice errors on non-logical columns in any group", {
  df <- data.frame(a1 = c(TRUE, FALSE), a2 = c(1, 2), b1 = c(FALSE, TRUE), b2 = c(3, 4))
  expect_error(
    combine_multichoice(df, quest_cols = list(groupA = c("a1", "a2"), groupB = c("b1", "b2"))),
    "The following columns are not logical vectors: a2, b2"
  )
})

test_that("combine_multichoice removes original columns when remove_cols=TRUE", {
  df <- data.frame(a1 = c(TRUE, FALSE), a2 = c(FALSE, TRUE), b1 = c(TRUE, FALSE))
  result <- combine_multichoice(df, quest_cols = list(groupA = c("a1", "a2")), remove_cols = TRUE)
  expect_false(any(c("a1", "a2") %in% colnames(result)))
  expect_true("groupA" %in% colnames(result))
  expect_true("b1" %in% colnames(result)) # Unrelated column should remain
})

test_that("combine_multichoice keeps original columns when remove_cols=FALSE", {
  df <- data.frame(a1 = c(TRUE, FALSE), a2 = c(FALSE, TRUE))
  result <- combine_multichoice(df, quest_cols = list(groupA = c("a1", "a2")), remove_cols = FALSE)
  expect_true(all(c("a1", "a2", "groupA") %in% colnames(result)))
})

test_that("split_multichoice handles basic splitting", {
  df <- data.frame(q1 = c("ab", "c da", "b a", NA), q2 = c("a b", "a c", "d", "ab"))
  result <- split_multichoice(df, quest_cols = c("q1", "q2"))
  expect_snapshot(result)
})

test_that("split_multichoice with remove_space=FALSE", {
  df <- data.frame(q1 = c("a b", "c d", "b a"))
  result <- split_multichoice(df, quest_cols = "q1", remove_space = FALSE)
  expect_snapshot(result)
})

test_that("split_multichoice with custom link character", {
  df <- data.frame(q1 = c("ab", "cd"))
  result <- split_multichoice(df, quest_cols = "q1", link = "-")
  expect_snapshot(result)
})

test_that("split_multichoice retains original columns when remove_cols=FALSE", {
  df <- data.frame(q1 = c("ab", "cd"))
  result <- split_multichoice(df, quest_cols = "q1", remove_cols = FALSE)
  expect_snapshot(colnames(result))
})

test_that("split_multichoice handles empty strings and NAs", {
  df <- data.frame(q1 = c(NA, "", "ab"))
  result <- split_multichoice(df, quest_cols = "q1")
  expect_snapshot(result)
})
