test_that("net_intervention_avoided() works", {
  expect_error(
    dca_binary1 <-
      dca(cancer ~ cancerpredmarker, data = df_binary) %>%
      net_intervention_avoided(),
    NA
  )
  expect_equal(
    dca_binary1 %>%
      as_tibble() %>%
      dplyr::filter(threshold == 0.25) %>%
      dplyr::pull(net_intervention_avoided),
    c(0.0, 44.0, 63.2),
    tolerance = 1e-5
  )

  expect_error(
      net_intervention_avoided(mtcars)
  )
})
