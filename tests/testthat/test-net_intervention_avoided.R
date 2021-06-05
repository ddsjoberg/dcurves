test_that("net_intervention_avoided() works", {
  expect_error(
    dca_binary <-
      dca(cancer ~ cancerpredmarker, data = df_binary) %>%
      net_intervention_avoided(),
    NA
  )

  expect_error(
    dca_binary <-
      dca(cancer ~ cancerpredmarker, data = df_binary) %>%
      net_intervention_avoided(),
    NA
  )

  expect_error(
      net_intervention_avoided(mtcars)
  )
})
