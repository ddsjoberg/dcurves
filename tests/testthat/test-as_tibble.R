test_that("as_tibble() works", {
  expect_error(
    dca_binary <-
      dca(cancer ~ cancerpredmarker, data = df_binary) %>%
      as_tibble(),
    NA
  )
})
