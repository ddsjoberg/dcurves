test_that("net_intervention_avoided() works", {
  expect_error(
    dca_binary1 <-
      dca(cancer ~ cancerpredmarker, data = df_binary) %>%
      standardized_net_benefit() %>%
      plot(),
    NA
  )

  expect_error(
    dca(cancer ~ cancerpredmarker,
        data = df_binary,
        harm = list(cancerpredmarker = 0.5)) %>%
      standardized_net_benefit()
  )

  expect_error(
    dca(cancer ~ cancerpredmarker,
        data = df_binary) %>%
      plot(type = "standardized_net_benefit")
  )

  expect_error(
    standardized_net_benefit(mtcars)
  )
})
