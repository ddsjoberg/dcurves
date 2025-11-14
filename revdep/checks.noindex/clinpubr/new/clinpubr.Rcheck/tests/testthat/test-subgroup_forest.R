library(vdiffr)
library(withr)

# Set seed for reproducibility
set.seed(1)

test_that("subgroup_forest generates correct Cox regression plot", {
  withr::with_tempdir({
    data(cancer, package = "survival")
    p <- subgroup_forest(cancer,
      subgroup_vars = c("age", "sex", "wt.loss"), x = "ph.ecog", y = "status",
      time = "time", covars = "ph.karno", ticks_at = c(1, 2), save_plot = FALSE
    )
    expect_s3_class(p, "gtable")
    # vdiffr::expect_doppelganger("cox_subgroup_forest", p)
  })
})


test_that("subgroup_forest generates correct linear regression plot", {
  withr::with_tempdir({
    data(cancer, package = "survival")
    p <- subgroup_forest(cancer,
      subgroup_vars = c("age", "sex"), x = "ph.ecog", y = "wt.loss",
      covars = "ph.karno", save_plot = FALSE
    )
    expect_s3_class(p, "gtable")
    # vdiffr::expect_doppelganger("linear_subgroup_forest", p)
  })
})


test_that("subgroup_forest handles factor predictor correctly", {
  withr::with_tempdir({
    data(cancer, package = "survival")
    cancer$dead <- cancer$status == 2
    cancer$ph.ecog_cat <- factor(cancer$ph.ecog, levels = c(0:3), labels = c("0", "1", ">=2", ">=2"))
    p <- subgroup_forest(cancer,
      subgroup_vars = c("sex", "wt.loss"), x = "ph.ecog_cat", y = "dead",
      covars = "ph.karno", ticks_at = c(1, 2), save_plot = FALSE
    )
    expect_s3_class(p, "gtable")
    # vdiffr::expect_doppelganger("factor_predictor_subgroup_forest", p)
  })
})

test_that("subgroup_forest standardize x correctly", {
  withr::with_tempdir({
    data(cancer, package = "survival")
    p <- subgroup_forest(cancer,
      subgroup_vars = c("age", "sex"), x = "ph.ecog", y = "wt.loss",
      covars = "ph.karno", save_plot = FALSE, standardize_x = TRUE
    )
    expect_s3_class(p, "gtable")
    # vdiffr::expect_doppelganger("standardized_subgroup_forest", p)
  })
})