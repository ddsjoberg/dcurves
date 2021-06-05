test_that("plot() works", {
  expect_error(
    dca_binary <-
      dca(cancer ~ cancerpredmarker, data = df_binary) %>%
      plot(),
    NA
  )

  expect_error(
    dca_binary <-
      dca(cancer ~ cancerpredmarker, data = df_binary) %>%
      plot(smooth = TRUE),
    NA
  )

  expect_error(
    dca_binary <-
      dca(cancer ~ cancerpredmarker, data = df_binary) %>%
      net_intervention_avoided() %>%
      plot(),
    NA
  )

  expect_error(
    dca_binary <-
      dca(cancer ~ cancerpredmarker, data = df_binary) %>%
      net_intervention_avoided() %>%
      plot(smooth = TRUE),
    NA
  )

  expect_error(
    dca_binary <-
      dca(cancer ~ cancerpredmarker, data = df_binary) %>%
      net_intervention_avoided() %>%
      plot(type = "net_benefit"),
    NA
  )

  expect_error(
    dca(cancer ~ cancerpredmarker, data = df_binary) %>%
      net_intervention_avoided() %>%
      plot(type = "net_intervention_avoided", show_ggplot_code = TRUE),
    NA
  )

  expect_error(
    dca(cancer ~ cancerpredmarker, data = df_binary) %>%
      plot(type = "net_intervention_avoided", show_ggplot_code = TRUE)
  )
})
