test_that("autoplot() works", {
  expect_error(
    dca_binary <-
      dca(cancer ~ cancerpredmarker, data = df_binary) %>%
      autoplot(),
    NA
  )
})

