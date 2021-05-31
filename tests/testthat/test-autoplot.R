test_that("autoplot() works", {
  expect_error(
    dca_binary <-
      dca(cancer ~ cancerpredmarker, data = df_binary) %>%
      autoplot(),
    NA
  )

  expect_error(
    dca_binary <-
      dca(cancer ~ cancerpredmarker, data = df_binary) %>%
      autoplot(smooth = TRUE),
    NA
  )

  expect_error(
    dca_binary <-
      dca(cancer ~ cancerpredmarker, data = df_binary) %>%
      net_interventions_avoided() %>%
      autoplot(),
    NA
  )

  expect_error(
    dca_binary <-
      dca(cancer ~ cancerpredmarker, data = df_binary) %>%
      net_interventions_avoided() %>%
      autoplot(smooth = TRUE),
    NA
  )

  expect_error(
    dca_binary <-
      dca(cancer ~ cancerpredmarker, data = df_binary) %>%
      net_interventions_avoided() %>%
      autoplot(type = "net_benefit"),
    NA
  )
})
