test_that("cut_by handles quantile breaks", {
  set.seed(1)
  x <- rnorm(100)
  res <- cut_by(x, breaks = c(0.5), breaks_as_quantiles = TRUE, label_type = "LMH")
  expect_equal(levels(res), c("Low", "High"))
  set.seed(123)
  x <- rnorm(100)

  res <- cut_by(x, c(0.3, 0.7), breaks_as_quantiles = TRUE, label_type = "LMH")
  expect_equal(levels(res), c("Low", "Medium", "High"))

  res <- cut_by(x, c(0.4), breaks_as_quantiles = TRUE, label_type = "combined")
  expect_equal(levels(res), c("Low [-2.31,-0.223)", "High [-0.223,2.19]"))

  res <- cut_by(x, c(0.23), labels = c("A", "b"))
  expect_snapshot(res)
})

test_that("cut_by enforces LMH labels when group is specified", {
  set.seed(1)
  x <- c(rnorm(100, 0, 1), rnorm(100, 5, 1))
  group <- rep(c("A", "B"), each = 100)

  # Test warning when label_type is not LMH
  expect_warning(
    result <- cut_by(x,
      breaks = c(1 / 3, 2 / 3), breaks_as_quantiles = TRUE,
      group = group, label_type = "ori"
    ),
    "`label_type` is set to `LMH` since `group` is specified",
    fixed = TRUE
  )
  expect_equal(levels(result), c("Low", "Medium", "High"))
})

test_that("cut_by with group calculates quantiles by group", {
  set.seed(1)
  x <- c(rnorm(100, 0, 1), rnorm(100, 5, 1))
  group <- rep(c("A", "B"), each = 100)

  expect_warning(
    result <- cut_by(x, breaks = c(1 / 3, 2 / 3), breaks_as_quantiles = TRUE, group = group),
    "`label_type` is set to `LMH` since `group` is specified",
    fixed = TRUE
  )

  expect_equal(levels(result), c("Low", "Medium", "High"))

  # Check within-group distributions
  expect_equal(mean(result[group == "A"] == "Low"), 1 / 3, tolerance = 0.1)
  expect_equal(mean(result[group == "B"] == "Low"), 1 / 3, tolerance = 0.1)
})

test_that("cut_by handles group with identical values", {
  set.seed(1)
  x <- c(rep(1, 10), rep(2, 90))
  expect_warning(result <- cut_by(x, breaks = c(1 / 3, 2 / 3), breaks_as_quantiles = TRUE),
    "Empty levels are dropped: [2.00000001,2.00000002]",
    fixed = TRUE
  )
  expect_true(sum(result == "[2,2.00000001)") == 90)

  set.seed(1)
  x <- rep(5, 100)
  group <- rep(c("A", "B"), each = 50)
  result <- suppressWarnings(cut_by(x, breaks = c(0.5), breaks_as_quantiles = TRUE, group = group))
  expect_true(all(result == "Low"))
})

test_that("boundary conditions handled", {
  expect_error(cut_by(1:10, c(-1, 2)))
})
