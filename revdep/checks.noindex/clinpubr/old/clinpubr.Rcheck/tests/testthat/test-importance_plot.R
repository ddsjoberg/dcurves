library(ggplot2)
library(withr)

test_that("importance_plot works with top_n and split_at", {
  set.seed(1)
  dummy_importance <- runif(20)^5
  names(dummy_importance) <- paste0("var", 1:20)

  p <- importance_plot(dummy_importance, top_n = 15, split_at = 10, save_plot = FALSE)
  expect_s3_class(p, "gg")
  expect_length(p$layers, 3) # geom_bar + geom_text + geom_vline when split_at and labels are present
  expect_snapshot(p$data)
  vdiffr::expect_doppelganger("test_plot.png", p)

  with_tempdir({
    importance_plot(dummy_importance, filename = "test_plot.png", save_plot = TRUE)
    expect_true(file.exists("test_plot.png"))
  })
})

test_that("importance_plot works with top_n = NULL", {
  set.seed(1)
  dummy_small <- runif(5)^5
  names(dummy_small) <- paste0("var", 1:5)
  p <- importance_plot(dummy_small, top_n = NULL, save_plot = FALSE)
  expect_equal(length(levels(p$data$name)), 5)
  expect_false("..." %in% p$data$name)
})

test_that("labels are formatted correctly", {
  set.seed(1)
  dummy_labels <- runif(3)^5
  names(dummy_labels) <- c("A", "B", "C")

  p <- importance_plot(dummy_labels, show_labels = TRUE, nsmall = 2, save_plot = FALSE)
  label_text <- p$layers[[2]]$data$label
  expect_true(all(grepl("\\.\\d{2}", label_text)))
})
