library(rlang)
library(dplyr)

# Test create_formula ---------------------------------------------------
test_that("create_formula generates correct formulas", {
  # Basic formula
  expect_equal(
    as.character(create_formula("y", "x")),
    as.character(y ~ x)
  )

  # With time (Cox model)
  expect_equal(
    as.character(create_formula("y", "x", time = "t")),
    as.character(Surv(t, y) ~ x)
  )

  # With group_var and interaction
  expect_equal(
    as.character(create_formula("y", "x", group_var = "g", interaction = TRUE)),
    as.character(y ~ x * g)
  )

  # With rcs knots
  expect_equal(
    as.character(create_formula("y", "x", rcs_knots = 3)),
    as.character(y ~ rcs(x, 3))
  )
})

# Test to_factor --------------------------------------------------------
test_that("to_factor converts numeric to factor appropriately", {
  # Numeric with >5 unique values
  x <- 1:10
  expect_true(is.factor(to_factor(x)))
  expect_equal(length(levels(to_factor(x))), 2)

  # Numeric with <=5 unique values
  x <- 1:3
  expect_true(is.factor(to_factor(x)))
  expect_equal(levels(to_factor(x)), c("1", "2", "3"))

  # NA handling
  x <- c(1, 2, NA)
  expect_equal(levels(to_factor(x, na_as_level = TRUE)), c("1", "2", "NA"))
})

# Test remove_conflict --------------------------------------------------
test_that("remove_conflict handles variable conflicts", {
  # Overlapping variables
  expect_equal(remove_conflict(c("a", "b"), c("b", "c")), "a")
  # No overlap
  expect_equal(remove_conflict(c("a", "b"), c("c", "d")), c("a", "b"))
  # Silent mode
  expect_silent(remove_conflict(c("a", "b"), c("b"), silent = TRUE))
})

# Test .calculate_order (internal) --------------------------------------
test_that(".calculate_order determines element order", {
  vec_list <- list(c("a", "b"), c("b", "a"), c("a", "c"))
  expect_equal(.calculate_order("a", "b", vec_list), 0)  # 1 before, 1 after
  expect_equal(.calculate_order("a", "c", vec_list), 1)  # 1 before, 0 after
})