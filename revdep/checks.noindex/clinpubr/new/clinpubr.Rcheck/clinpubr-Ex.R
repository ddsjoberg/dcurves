pkgname <- "clinpubr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('clinpubr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_lists")
### * add_lists

flush(stderr()); flush(stdout())

### Name: add_lists
### Title: Adding lists element-wise
### Aliases: add_lists

### ** Examples

l1 <- list(a = 1, b = 2)
l2 <- list(a = 3, b = 4, c = 5)
add_lists(l1, l2)



cleanEx()
nameEx("answer_check")
### * answer_check

flush(stderr()); flush(stdout())

### Name: answer_check
### Title: Check answers of multiple choice questions
### Aliases: answer_check

### ** Examples

dat <- data.frame(Q1 = c("A", "B", "C"), Q2 = c("AD", "AE", "ABF"))
seq <- c("A", "AE")
answer_check(dat, seq)
dat <- data.frame(
  Q1 = c("A", "B", "C"), Q2.A = c(TRUE, TRUE, FALSE),
  Q2.B = c(TRUE, FALSE, TRUE), Q2.C = c(FALSE, TRUE, FALSE)
)
seq <- c("A", "TFT")
answer_check(dat, seq, multi_column = TRUE)



cleanEx()
nameEx("baseline_table")
### * baseline_table

flush(stderr()); flush(stdout())

### Name: baseline_table
### Title: Create a baseline table for a dataset.
### Aliases: baseline_table

### ** Examples

withr::with_tempdir(
  {
    data(cancer, package = "survival")
    var_types <- get_var_types(cancer, strata = "sex")
    baseline_table(cancer, var_types = var_types, filename = "baseline.csv")

    # baseline table with pairwise comparison
    cancer$ph.ecog_cat <- factor(cancer$ph.ecog,
      levels = c(0:3),
      labels = c("0", "1", ">=2", ">=2")
    )
    var_types <- get_var_types(cancer, strata = "ph.ecog_cat")
    baseline_table(cancer, var_types = var_types, filename = "baselineV2.csv")
    print(paste0("files saved to: ", getwd()))
  },
  clean = FALSE
)



cleanEx()
nameEx("break_at")
### * break_at

flush(stderr()); flush(stdout())

### Name: break_at
### Title: Generate breaks for histogram
### Aliases: break_at

### ** Examples

break_at(xlim = c(0, 10), breaks = 12, ref_val = 3.12)



cleanEx()
nameEx("calculate_index")
### * calculate_index

flush(stderr()); flush(stdout())

### Name: calculate_index
### Title: Calculate index based on conditions
### Aliases: calculate_index

### ** Examples

df <- data.frame(x = c(1, 2, 3, 4, 5), y = c(1, 2, NA, 4, NA))
calculate_index(df, x > 3, y < 3, .weight = c(1, 2), .na_replace = 0)



cleanEx()
nameEx("check_nonnum")
### * check_nonnum

flush(stderr()); flush(stdout())

### Name: check_nonnum
### Title: Check elements that are not numeric
### Aliases: check_nonnum

### ** Examples

check_nonnum(c("\uFF11\uFF12\uFF13", "11..23", "3.14", "2.131", "35.2."))



cleanEx()
nameEx("classif_model_compare")
### * classif_model_compare

flush(stderr()); flush(stdout())

### Name: classif_model_compare
### Title: Performance comparison of classification models
### Aliases: classif_model_compare

### ** Examples

data(cancer, package = "survival")
df <- kidney
df$dead <- ifelse(df$time <= 100 & df$status == 0, NA, df$time <= 100)
df <- na.omit(df[, -c(1:3)])

model0 <- glm(dead ~ age + frail, family = binomial(), data = df)
model <- glm(dead ~ ., family = binomial(), data = df)
df$base_pred <- predict(model0, type = "response")
df$full_pred <- predict(model, type = "response")

classif_model_compare(df, "dead", c("base_pred", "full_pred"), save_output = FALSE)



cleanEx()
nameEx("combine_files")
### * combine_files

flush(stderr()); flush(stdout())

### Name: combine_files
### Title: combine multiple data files into a single data frame
### Aliases: combine_files

### ** Examples

library(withr)
with_tempdir({
  write.csv(data.frame(x = 1:3, y = 4:6), "file1.csv", row.names = FALSE)
  write.csv(data.frame(x = 7:9, y = 10:12), "file2.csv", row.names = FALSE)
  dat <- combine_files(pattern = "file")
})
print(dat)



cleanEx()
nameEx("combine_multichoice")
### * combine_multichoice

flush(stderr()); flush(stdout())

### Name: combine_multichoice
### Title: Combine multi-choice columns into one
### Aliases: combine_multichoice

### ** Examples

# Single group (backward compatibility)
df <- data.frame(q1 = c(TRUE, FALSE, TRUE), q2 = c(FALSE, TRUE, TRUE))
combine_multichoice(df, quest_cols = c("q1", "q2"))

# Multiple groups with named list
df <- data.frame(
  a1 = c(TRUE, FALSE, TRUE), a2 = c(FALSE, TRUE, TRUE),
  b1 = c(TRUE, TRUE, FALSE), b2 = c(FALSE, FALSE, TRUE)
)
combine_multichoice(df, quest_cols = list(groupA = c("a1", "a2"), groupB = c("b1", "b2")))



cleanEx()
nameEx("common_prefix")
### * common_prefix

flush(stderr()); flush(stdout())

### Name: common_prefix
### Title: Get common prefix of a string vector
### Aliases: common_prefix

### ** Examples

common_prefix(c("Q1_a", "Q1_b", "Q1_c"))



cleanEx()
nameEx("cut_by")
### * cut_by

flush(stderr()); flush(stdout())

### Name: cut_by
### Title: Convert Numeric to Factor
### Aliases: cut_by

### ** Examples

set.seed(123)
cut_by(rnorm(100), c(0, 1, 2))
cut_by(rnorm(100), c(1 / 3, 2 / 3), breaks_as_quantiles = TRUE, label_type = "LMH")



cleanEx()
nameEx("df_view_nonnum")
### * df_view_nonnum

flush(stderr()); flush(stdout())

### Name: df_view_nonnum
### Title: Show non-numeric elements in a data frame
### Aliases: df_view_nonnum

### ** Examples

df <- data.frame(
  x = c("1", "2", "3..3", "4", "6a"),
  y = c("1", "ss", "aa.a", "4", "xx"),
  z = c("1", "2", "3", "4", "6")
)
df_view_nonnum(df)



cleanEx()
nameEx("exclusion_count")
### * exclusion_count

flush(stderr()); flush(stdout())

### Name: exclusion_count
### Title: Count the number of excluded samples at each step
### Aliases: exclusion_count

### ** Examples

cohort <- data.frame(
  age = c(17, 25, 30, NA, 50, 60),
  sex = c("M", "F", "F", "M", "F", "M"),
  value = c(1, NA, 3, 4, 5, NA),
  dementia = c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)
)
exclusion_count(
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



cleanEx()
nameEx("extract_num")
### * extract_num

flush(stderr()); flush(stdout())

### Name: extract_num
### Title: Extract numbers from string.
### Aliases: extract_num

### ** Examples

x <- c("1.2(XXX)", "5-8POS", "NS", "FULL", "5.5", "4.2")
extract_num(x)
extract_num(x,
  res_type = "first", multimatch2na = TRUE, zero_regexp = "NEG|NS",
  max_regexp = "FULL"
)
extract_num(x, res_type = "range", allow_neg = FALSE, zero_regexp = "NEG|NS", max_regexp = "FULL")



cleanEx()
nameEx("fill_with_last")
### * fill_with_last

flush(stderr()); flush(stdout())

### Name: fill_with_last
### Title: Fill NA values with the last valid value
### Aliases: fill_with_last

### ** Examples

fill_with_last(c(1, 2, NA, 4, NA, 6))



cleanEx()
nameEx("filter_rcs_predictors")
### * filter_rcs_predictors

flush(stderr()); flush(stdout())

### Name: filter_rcs_predictors
### Title: Filter predictors for RCS
### Aliases: filter_rcs_predictors

### ** Examples

filter_rcs_predictors(mtcars)



cleanEx()
nameEx("first_mode")
### * first_mode

flush(stderr()); flush(stdout())

### Name: first_mode
### Title: Calculate the first mode
### Aliases: first_mode

### ** Examples

first_mode(c(1, 1, 2, 2, 3, 3, 3, NA, NA, NA))



cleanEx()
nameEx("format_pval")
### * format_pval

flush(stderr()); flush(stdout())

### Name: format_pval
### Title: Format p-value for publication
### Aliases: format_pval

### ** Examples

format_pval(c(0.001, 0.0001, 0.05, 0.1123456))
format_pval(c(0.001, 0.0001, 0.05, 0.1123456), text_ahead = "p value")



cleanEx()
nameEx("formula_add_covs")
### * formula_add_covs

flush(stderr()); flush(stdout())

### Name: formula_add_covs
### Title: Add covariates to a formula
### Aliases: formula_add_covs

### ** Examples

formula_add_covs("y ~ a + b", c("c", "d"))



cleanEx()
nameEx("get_samples")
### * get_samples

flush(stderr()); flush(stdout())

### Name: get_samples
### Title: Generate a sample of values from a vector and collapse them.
### Aliases: get_samples

### ** Examples

get_samples(c(1, 2, 3, 4, 5))
get_samples(c(1, 2, 3, 4, 5), n_samples = 2)
get_samples(c(1, 2, 3, 3, 3), n_samples = 2, unique_only = TRUE)
get_samples(c(1, 2, 3, 4, 5), collapse = ", ")



cleanEx()
nameEx("get_valid")
### * get_valid

flush(stderr()); flush(stdout())

### Name: get_valid
### Title: Get one valid value from vector.
### Aliases: get_valid

### ** Examples

get_valid(c(NA, 1, 2, NA, 3, NA, 4))
get_valid(c(NA, 1, NA), mode = "last", disjoint = TRUE)



cleanEx()
nameEx("get_valid_subset")
### * get_valid_subset

flush(stderr()); flush(stdout())

### Name: get_valid_subset
### Title: Get the subset that satisfies the missing rate condition.
### Aliases: get_valid_subset

### ** Examples

data(cancer, package = "survival")
dim(cancer)
max_missing_rates(cancer)

cancer_valid <- get_valid_subset(cancer, row_na_ratio = 0.2, col_na_ratio = 0.1, row_priority = 1)
dim(cancer_valid)
max_missing_rates(cancer_valid)



cleanEx()
nameEx("get_var_types")
### * get_var_types

flush(stderr()); flush(stdout())

### Name: get_var_types
### Title: Get variable types for baseline table
### Aliases: get_var_types

### ** Examples

data(cancer, package = "survival")
get_var_types(cancer, strata = "sex") # set save_qqplots = TRUE to check the QQ plots

var_types <- get_var_types(cancer, strata = "sex")
# for some reason we want the variable "pat.karno" ro be considered normal.
var_types$nonnormal_vars <- setdiff(var_types$nonnormal_vars, "pat.karno")



cleanEx()
nameEx("importance_plot")
### * importance_plot

flush(stderr()); flush(stdout())

### Name: importance_plot
### Title: Importance plot
### Aliases: importance_plot

### ** Examples

set.seed(1)
dummy_importance <- runif(20)^5
names(dummy_importance) <- paste0("var", 1:20)
importance_plot(dummy_importance, top_n = 15, split_at = 10, save_plot = FALSE)



cleanEx()
nameEx("indicate_duplicates")
### * indicate_duplicates

flush(stderr()); flush(stdout())

### Name: indicate_duplicates
### Title: Determine duplicate elements including their first occurrence.
### Aliases: indicate_duplicates

### ** Examples

indicate_duplicates(c(1, 2, NA, NA, 1))
indicate_duplicates(c(1, 2, 3, 4, 4))

# Useful to check duplicates in data frames.
df <- data.frame(
  id = c(1, 2, 1, 2, 3), year = c(2010, 2011, 2010, 2010, 2011),
  value = c(1, 2, 3, 4, 5)
)
df[indicate_duplicates(df[, c("id", "year")]), ]



cleanEx()
nameEx("interaction_p_value")
### * interaction_p_value

flush(stderr()); flush(stdout())

### Name: interaction_p_value
### Title: Calculate interaction p-value
### Aliases: interaction_p_value

### ** Examples

data(cancer, package = "survival")
interaction_p_value(
  data = cancer, y = "status", predictor = "age", group_var = "sex",
  time = "time", rcs_knots = 4
)



cleanEx()
nameEx("interaction_plot")
### * interaction_plot

flush(stderr()); flush(stdout())

### Name: interaction_plot
### Title: Plot interactions
### Aliases: interaction_plot

### ** Examples

data(cancer, package = "survival")
interaction_plot(cancer,
  y = "status", time = "time", predictor = "age", group_var = "sex",
  save_plot = FALSE
)
interaction_plot(cancer,
  y = "status", predictor = "age", group_var = "sex",
  save_plot = FALSE
)
interaction_plot(cancer,
  y = "wt.loss", predictor = "age", group_var = "sex",
  save_plot = FALSE
)



cleanEx()
nameEx("interaction_scan")
### * interaction_scan

flush(stderr()); flush(stdout())

### Name: interaction_scan
### Title: Scan for interactions between variables
### Aliases: interaction_scan

### ** Examples

data(cancer, package = "survival")
interaction_scan(cancer, y = "status", time = "time", save_table = FALSE)



cleanEx()
nameEx("mad_outlier")
### * mad_outlier

flush(stderr()); flush(stdout())

### Name: mad_outlier
### Title: Mark possible outliers with MAD.
### Aliases: mad_outlier

### ** Examples

x <- c(1, 2, 3, 4, 5, 100)
mad_outlier(x)



cleanEx()
nameEx("max_missing_rates")
### * max_missing_rates

flush(stderr()); flush(stdout())

### Name: max_missing_rates
### Title: Get the maximum missing rate of rows and columns.
### Aliases: max_missing_rates

### ** Examples

data(cancer, package = "survival")
max_missing_rates(cancer)



cleanEx()
nameEx("merge_ordered_vectors")
### * merge_ordered_vectors

flush(stderr()); flush(stdout())

### Name: merge_ordered_vectors
### Title: Merging vectors while maintaining order
### Aliases: merge_ordered_vectors

### ** Examples

merge_ordered_vectors(list(c(1, 3, 4, 5, 7, 10), c(2, 5, 6, 7, 8), c(1, 7, 5, 10)))



cleanEx()
nameEx("na2false")
### * na2false

flush(stderr()); flush(stdout())

### Name: na2false
### Title: Replace NA values with FALSE
### Aliases: na2false

### ** Examples

na2false(c(TRUE, FALSE, NA, TRUE, NA))
na2false(c(1, 2, NA))



cleanEx()
nameEx("na_max")
### * na_max

flush(stderr()); flush(stdout())

### Name: na_max
### Title: Safe min and max functions that return NA if all values are NA
### Aliases: na_max na_min

### ** Examples

na_max(c(1, 2, 3, NA))
na_min(c(NA, NA, NA))



cleanEx()
nameEx("predictor_effect_plot")
### * predictor_effect_plot

flush(stderr()); flush(stdout())

### Name: predictor_effect_plot
### Title: Plot the effect of a predictor variable
### Aliases: predictor_effect_plot

### ** Examples

data(cancer, package = "survival")
cancer$dead <- cancer$status == 2
cancer <- cancer[!is.na(cancer$inst), ]
predictor_effect_plot(
  data = cancer,
  x = "age",
  y = "dead",
  method = "linear",
  covars = "ph.karno",
  add_hist = FALSE,
  trans = "log2",
  save_plot = FALSE,
  cluster = "inst"
)



cleanEx()
nameEx("qq_show")
### * qq_show

flush(stderr()); flush(stdout())

### Name: qq_show
### Title: QQ plot
### Aliases: qq_show

### ** Examples

qq_show(rnorm(100))



cleanEx()
nameEx("rcs_plot")
### * rcs_plot

flush(stderr()); flush(stdout())

### Name: rcs_plot
### Title: Plot restricted cubic spline
### Aliases: rcs_plot

### ** Examples

data(cancer, package = "survival")
# coxph model with time assigned
rcs_plot(cancer, x = "age", y = "status", time = "time", covars = "ph.karno", save_plot = FALSE)

# logistic model with time not assigned
cancer$dead <- cancer$status == 2
rcs_plot(cancer, x = "age", y = "dead", covars = "ph.karno", save_plot = FALSE)



cleanEx()
nameEx("regression_basic_results")
### * regression_basic_results

flush(stderr()); flush(stdout())

### Name: regression_basic_results
### Title: Basic results of logistic or Cox regression.
### Aliases: regression_basic_results

### ** Examples

data(cancer, package = "survival")
# coxph model with time assigned
regression_basic_results(cancer,
  x = "age", y = "status", time = "time",
  model_covs = list(Crude = c(), Model1 = c("ph.karno"), Model2 = c("ph.karno", "sex")),
  save_output = FALSE,
  ggtheme = survminer::theme_survminer(font.legend = c(14, "plain", "black")) # theme for KM
)

# logistic model with time not assigned
cancer$dead <- cancer$status == 2
regression_basic_results(cancer,
  x = "age", y = "dead", ref_levels = c("Q3", "High"),
  model_covs = list(Crude = c(), Model1 = c("ph.karno"), Model2 = c("ph.karno", "sex")),
  save_output = FALSE
)



cleanEx()
nameEx("regression_fit")
### * regression_fit

flush(stderr()); flush(stdout())

### Name: regression_fit
### Title: Obtain regression results
### Aliases: regression_fit

### ** Examples

data(cancer, package = "survival")
regression_fit(data = cancer, y = "status", predictor = "age", time = "time", rcs_knots = 4)



cleanEx()
nameEx("regression_forest")
### * regression_forest

flush(stderr()); flush(stdout())

### Name: regression_forest
### Title: Forest plot of regression results
### Aliases: regression_forest

### ** Examples

data(cancer, package = "survival")
cancer$ph.ecog_cat <- factor(cancer$ph.ecog, levels = c(0:3), labels = c("0", "1", ">=2", ">=2"))
regression_forest(cancer,
  model_vars = c("age", "sex", "wt.loss", "ph.ecog_cat", "meal.cal"), y = "status", time = "time",
  as_univariate = TRUE, save_plot = FALSE
)

regression_forest(cancer,
  model_vars = c("age", "sex", "wt.loss", "ph.ecog_cat", "meal.cal"), y = "status", time = "time",
  show_vars = c("age", "sex", "ph.ecog_cat", "meal.cal"), save_plot = FALSE
)

regression_forest(cancer,
  model_vars = list(
    M0 = c("age"),
    M1 = c("age", "sex", "wt.loss", "ph.ecog_cat", "meal.cal"),
    M2 = c("age", "sex", "wt.loss", "ph.ecog_cat", "meal.cal", "pat.karno")
  ),
  y = "status", time = "time",
  show_vars = c("age", "sex", "ph.ecog_cat", "meal.cal"), save_plot = FALSE
)



cleanEx()
nameEx("regression_scan")
### * regression_scan

flush(stderr()); flush(stdout())

### Name: regression_scan
### Title: Scan for significant regression predictors
### Aliases: regression_scan

### ** Examples

data(cancer, package = "survival")
regression_scan(cancer, y = "status", time = "time", save_table = FALSE)



cleanEx()
nameEx("replace_elements")
### * replace_elements

flush(stderr()); flush(stdout())

### Name: replace_elements
### Title: Replacing elements in a vector
### Aliases: replace_elements

### ** Examples

replace_elements(c("a", "x", "1", NA, "a"), c("a", "b", NA), c("A", "B", "XX"))



cleanEx()
nameEx("split_multichoice")
### * split_multichoice

flush(stderr()); flush(stdout())

### Name: split_multichoice
### Title: Split multi-choice data into columns
### Aliases: split_multichoice

### ** Examples

df <- data.frame(q1 = c("ab", "c da", "b a", NA), q2 = c("a b", "a c", "d", "ab"))
split_multichoice(df, quest_cols = c("q1", "q2"))



cleanEx()
nameEx("str_match_replace")
### * str_match_replace

flush(stderr()); flush(stdout())

### Name: str_match_replace
### Title: Match string and replace with corresponding value
### Aliases: str_match_replace

### ** Examples

ori_names <- c("xx (mg/dl)", "b*x", "Covid-19")
modified_names <- c("v1", "v2", "v3")
x <- c("v1.v2", "v3.yy", "v4")
str_match_replace(x, modified_names, ori_names)



cleanEx()
nameEx("subgroup_forest")
### * subgroup_forest

flush(stderr()); flush(stdout())

### Name: subgroup_forest
### Title: Create subgroup forest plot.
### Aliases: subgroup_forest

### ** Examples

data(cancer, package = "survival")
# coxph model with time assigned
subgroup_forest(cancer,
  subgroup_vars = c("age", "sex", "wt.loss"), x = "ph.ecog", y = "status",
  time = "time", covars = "ph.karno", ticks_at = c(1, 2), save_plot = FALSE
)

# logistic model with time not assigned
cancer$dead <- cancer$status == 2
subgroup_forest(cancer,
  subgroup_vars = c("age", "sex", "wt.loss"), x = "ph.ecog", y = "dead",
  covars = "ph.karno", ticks_at = c(1, 2), save_plot = FALSE
)

cancer$ph.ecog_cat <- factor(cancer$ph.ecog, levels = c(0:3), labels = c("0", "1", ">=2", ">=2"))
subgroup_forest(cancer,
  subgroup_vars = c("sex", "wt.loss"), x = "ph.ecog_cat", y = "dead",
  covars = "ph.karno", ticks_at = c(1, 2), save_plot = FALSE
)



cleanEx()
nameEx("subject_view")
### * subject_view

flush(stderr()); flush(stdout())

### Name: subject_view
### Title: Get an overview of different subjects in data.
### Aliases: subject_view

### ** Examples

df <- data.frame(subject = sample(c("a", "b"), 1000, replace = TRUE), value = runif(1000))
df$unit <- NA
df$unit[df$subject == "a"] <- sample(c("mg/L", "g/l", "g/L"),
  sum(df$subject == "a"),
  replace = TRUE
)
df$value[df$subject == "a" & df$unit == "mg/L"] <-
  df$value[df$subject == "a" & df$unit == "mg/L"] * 1000
df$unit[df$subject == "b"] <- sample(c(NA, "g", "mg"), sum(df$subject == "b"), replace = TRUE)
df$value[df$subject == "b" & df$unit %in% "mg"] <-
  df$value[df$subject == "b" & df$unit %in% "mg"] * 1000
df$value[df$subject == "b" & is.na(df$unit)] <- df$value[df$subject == "b" & is.na(df$unit)] *
  sample(c(1, 1000), size = sum(df$subject == "b" & is.na(df$unit)), replace = TRUE)
subject_view(
  df = df, subject_col = "subject", info_cols = c("value", "unit"), value_col = "value",
  save_table = FALSE
)



cleanEx()
nameEx("test_normality")
### * test_normality

flush(stderr()); flush(stdout())

### Name: test_normality
### Title: Test normality of a numeric variable
### Aliases: test_normality

### ** Examples

# Test normal data
normal_data <- rnorm(100)
test_normality(normal_data)

# Test non-normal data
skewed_data <- rexp(100)
test_normality(skewed_data)



cleanEx()
nameEx("to_date")
### * to_date

flush(stderr()); flush(stdout())

### Name: to_date
### Title: Convert numerical or character date to date.
### Aliases: to_date

### ** Examples

to_date(c(43562, "2020-01-01", "2020/01/01", "20200101", "2020.01.01"))



cleanEx()
nameEx("unit_standardize")
### * unit_standardize

flush(stderr()); flush(stdout())

### Name: unit_standardize
### Title: Standardize units of numeric data.
### Aliases: unit_standardize

### ** Examples

# Example 1: Using the list as change_rules is more convenient for small datasets.
df <- data.frame(
  subject = c("a", "a", "b", "b", "b", "c", "c"), value = c(1, 2, 3, 4, 5, 6, 7),
  unit = c(NA, "x", "x", "x", "y", "a", "b")
)
change_rules <- list(
  list(subject = "a", target_unit = "x", units2change = c(NA), coeffs = c(20)),
  list(subject = "b"),
  list(subject = "c", target_unit = "b")
)
unit_standardize(df,
  subject_col = "subject", value_col = "value", unit_col = "unit",
  change_rules = change_rules
)

# Example 2: Using the labeled result from `unit_view()` as the input
# is more robust for large datasets.
df <- data.frame(subject = sample(c("a", "b"), 1000, replace = TRUE), value = runif(1000))
df$unit <- NA
df$unit[df$subject == "a"] <- sample(c("mg/L", "g/l", "g/L"),
  sum(df$subject == "a"),
  replace = TRUE
)
df$value[df$subject == "a" & df$unit == "mg/L"] <-
  df$value[df$subject == "a" & df$unit == "mg/L"] * 1000
df$unit[df$subject == "b"] <- sample(c(NA, "m.g", "mg"), sum(df$subject == "b"),
  prob = c(0.3, 0.05, 0.65), replace = TRUE
)
df$value[df$subject == "b" & df$unit %in% "mg"] <-
  df$value[df$subject == "b" & df$unit %in% "mg"] * 1000
df$value[df$subject == "b" & is.na(df$unit)] <- df$value[df$subject == "b" & is.na(df$unit)] *
  sample(c(1, 1000), size = sum(df$subject == "b" & is.na(df$unit)), replace = TRUE)

unit_table <- unit_view(
  df = df, subject_col = "subject",
  value_col = "value", unit_col = "unit", save_table = FALSE
)
unit_table$label <- c("t", NA, 1e-3, NA, NA, "r") # labeling the units

df_standardized <- unit_standardize(
  df = df, subject_col = "subject", value_col = "value",
  unit_col = "unit", change_rules = unit_table
)
unit_view(
  df = df_standardized, subject_col = "subject", value_col = "value", unit_col = "unit",
  save_table = FALSE, conflicts_only = FALSE
)



cleanEx()
nameEx("unit_view")
### * unit_view

flush(stderr()); flush(stdout())

### Name: unit_view
### Title: Generate a table of conflicting units.
### Aliases: unit_view

### ** Examples

df <- data.frame(subject = sample(c("a", "b"), 1000, replace = TRUE), value = runif(1000))
df$unit <- NA
df$unit[df$subject == "a"] <- sample(c("mg/L", "g/l", "g/L"),
  sum(df$subject == "a"),
  replace = TRUE
)
df$value[df$subject == "a" & df$unit == "mg/L"] <-
  df$value[df$subject == "a" & df$unit == "mg/L"] * 1000
df$unit[df$subject == "b"] <- sample(c(NA, "g", "mg"), sum(df$subject == "b"), replace = TRUE)
df$value[df$subject == "b" & df$unit %in% "mg"] <-
  df$value[df$subject == "b" & df$unit %in% "mg"] * 1000
df$value[df$subject == "b" & is.na(df$unit)] <- df$value[df$subject == "b" & is.na(df$unit)] *
  sample(c(1, 1000), size = sum(df$subject == "b" & is.na(df$unit)), replace = TRUE)
unit_view(
  df = df, subject_col = "subject",
  value_col = "value", unit_col = "unit", save_table = FALSE
)



cleanEx()
nameEx("unmake_names")
### * unmake_names

flush(stderr()); flush(stdout())

### Name: unmake_names
### Title: Unmake names
### Aliases: unmake_names

### ** Examples

ori_names <- c("xx (mg/dl)", "b*x", "Covid-19")
x <- c(make.names(ori_names), "aa")
unmake_names(x, ori_names)



cleanEx()
nameEx("value_initial_cleaning")
### * value_initial_cleaning

flush(stderr()); flush(stdout())

### Name: value_initial_cleaning
### Title: Preliminarily cleaning string vectors
### Aliases: value_initial_cleaning char_initial_cleaning

### ** Examples

x <- c("\uFF11\uFF12\uFF13", "11..23", "\uff41\uff42\uff41\uff4e\uff44\uff4f\uff4e", 
       "hello world ")
value_initial_cleaning(x)
char_initial_cleaning(x)



cleanEx()
nameEx("vec2code")
### * vec2code

flush(stderr()); flush(stdout())

### Name: vec2code
### Title: Generate code from string vector Generate the code that can be
###   used to generate the string vector.
### Aliases: vec2code

### ** Examples

vec2code(colnames(mtcars))



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
