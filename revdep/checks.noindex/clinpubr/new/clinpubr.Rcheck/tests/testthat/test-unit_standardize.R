# Test data setup
test_data <- data.frame(
  subject = c("a", "a", "a", "b", "b", "b", "c", "c", "c"),
  value = c(1, 2, 3, 10, 20, 30, 100, 200, 300),
  unit = c(NA, "mg", "mg", "mL", "dL", "dL", "mm", "cm", "cm"),
  stringsAsFactors = FALSE
)

# Test 1: Explicit target unit with NA conversion
test_that("NA unit converts to target with coefficient", {
  change_rules <- list(
    list(subject = "a", target_unit = "mg", units2change = c(NA), coeffs = c(10))
  )
  result <- unit_standardize(test_data, "subject", "value", "unit", change_rules)
  expect_equal(result$unit[result$subject == "a"], rep("mg", 3))
  expect_equal(result$value[result$subject == "a"], c(10, 2, 3)) # 1*10, 2*1, 3*1
})

# Test 2: Automatic target unit selection (most common)
test_that("Auto-selects most common unit", {
  change_rules <- list(
    list(subject = "b") # No target_unit specified
  )
  result <- unit_standardize(test_data, "subject", "value", "unit", change_rules)
  expect_equal(result$unit[result$subject == "b"], rep("dL", 3))
  expect_equal(result$value[result$subject == "b"], c(10, 20, 30)) # No conversion needed
})

# Test 3: Multiple units with custom coefficients
test_that("Converts multiple units with coefficients", {
  change_rules <- list(
    list(subject = "c", target_unit = "cm", units2change = c("mm"), coeffs = c(0.1))
  )
  result <- unit_standardize(test_data, "subject", "value", "unit", change_rules)
  expect_equal(result$unit[result$subject == "c"], rep("cm", 3))
  expect_equal(result$value[result$subject == "c"], c(10, 200, 300)) # 100*0.1, 200*1, 300*1
})

# Test 4: Error when coeffs length mismatch
test_that("Errors on coeffs/units2change length mismatch", {
  change_rules <- list(
    list(subject = "a", target_unit = "mg", units2change = c(NA, "g"), coeffs = c(10))
  )
  expect_error(
    unit_standardize(test_data, "subject", "value", "unit", change_rules),
    "`coeffs` should have the same length as `units2change`!"
  )
})

# Test 5: Complete list of unit conversions
test_that("Errors on target_unit not in units2change", {
  df <- data.frame(
    subject = c("a", "a", "b", "b", "b", "c", "c"), value = c(1, 2, 3, 4, 5, 6, 7),
    unit = c(NA, "x", "x", "x", "y", "z1", "z2")
  )
  change_rules <- list(
    list(subject = "a", target_unit = "x", units2change = c(NA), coeffs = c(20)),
    list(subject = "b"),
    list(subject = "c", target_unit = "z2")
  )
  res <- unit_standardize(df,
    subject_col = "subject", value_col = "value", unit_col = "unit",
    change_rules = change_rules
  )
  expect_equal(res$unit, c("x", "x", "x", "x", "x", "z2", "z2"))
  expect_equal(res$value, c(20, 2, 3, 4, 5, 6, 7))
})

# Test unit_standardize with data frame change_rules (Example 2)
test_that("unit_standardize works with data frame change_rules from unit_view", {
  set.seed(1)
  df <- data.frame(subject = sample(c("a", "b"), 1000, replace = T), value = runif(1000))
  df$unit <- NA
  df$unit[df$subject == "a"] <- sample(c("mg/L", "g/l", "g/L"), sum(df$subject == "a"), replace = T)
  df$value[df$subject == "a" & df$unit == "mg/L"] <- df$value[df$subject == "a" & df$unit == "mg/L"] * 1000
  df$unit[df$subject == "b"] <- sample(c(NA, "m.g", "mg"), sum(df$subject == "b"),
    prob = c(0.3, 0.05, 0.65), replace = T
  )
  df$value[df$subject == "b" & df$unit %in% "mg"] <- df$value[df$subject == "b" & df$unit %in% "mg"] * 1000
  df$value[df$subject == "b" & is.na(df$unit)] <- df$value[df$subject == "b" & is.na(df$unit)] *
    sample(c(1, 1000), size = sum(df$subject == "b" & is.na(df$unit)), replace = T)

  unit_table <- unit_view(df = df, subject_col = "subject", value_col = "value", unit_col = "unit", save_table = FALSE)
  unit_table$label <- c("t", NA, 1e-3, NA, NA, "r") # Label units
  result <- unit_standardize(
    df = df, subject_col = "subject", value_col = "value",
    unit_col = "unit", change_rules = unit_table
  )
  # Check unit_view output after standardization has fewer conflicts
  post_view <- unit_view(
    df = result, subject_col = "subject", value_col = "value", unit_col = "unit",
    save_table = FALSE, conflicts_only = FALSE
  )
  expect_lt(nrow(post_view), nrow(unit_table))
})

# Test unit_view with different quantiles and conflicts_only
test_that("unit_view generates correct output", {
  df <- data.frame(subject = c("a", "a", "b"), value = c(1, 2, 3), unit = c("x", "y", "x"))
  # Test quantiles
  res <- unit_view(df,
    subject_col = "subject", value_col = "value", unit_col = "unit",
    quantiles = c(0.25, 0.75), save_table = FALSE, conflicts_only = FALSE
  )
  expect_true(all(c("q_0.25", "q_0.75") %in% colnames(res)))
  # Test conflicts_only=TRUE
  res_conflicts <- unit_view(df,
    subject_col = "subject", value_col = "value", unit_col = "unit",
    conflicts_only = TRUE, save_table = FALSE
  )
  expect_equal(nrow(res_conflicts), 2) # Subject a has 2 units
  # Test CSV generation
  tmp_file <- tempfile(fileext = ".csv")
  unit_view(df,
    subject_col = "subject", value_col = "value", unit_col = "unit",
    save_table = TRUE, filename = tmp_file
  )
  expect_true(file.exists(tmp_file))
  file.remove(tmp_file)
})

# Test edge cases for unit_standardize
test_that("unit_standardize handles edge cases", {
  # Multiple target units error
  df <- data.frame(subject = "a", value = 1, unit = "x")
  change_rules <- list(list(subject = "a", target_unit = c("x", "y")))
  expect_error(unit_standardize(df, "subject", "value", "unit", change_rules), "too many targets!")
  # Mismatched coeffs and units2change
  change_rules <- list(list(subject = "a", target_unit = "x", units2change = c("z", "y"), coeffs = 1))
  expect_error(
    unit_standardize(df, "subject", "value", "unit", change_rules),
    "`coeffs` should have the same length as `units2change`!"
  )
  # Remove units (set to NA)
  df <- data.frame(subject = c("a", "a"), value = c(1, 100), unit = c("bad", "very bad"))
  change_rules <- list(list(subject = "a", units2change = "bad", coeffs = 1, units2remove = "very bad"))
  expect_error(
    unit_standardize(df, "subject", "value", "unit", change_rules),
    "`target_unit` is not specified and cannot be inferred from the data!"
  )
})
