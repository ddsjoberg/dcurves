test_that("dca() works", {
  expect_error(
    dca_binary1 <- dca(cancer ~ cancerpredmarker, data = df_binary),
    NA
  )
  expect_equal(
    dca_binary1 %>%
      as_tibble() %>%
      dplyr::filter(threshold == 0.25) %>%
      dplyr::pull(net_benefit),
    c(-0.1466667, 0.0000000, 0.0640000),
    tolerance = 1e-5
  )


  expect_error(
    dca_binary2 <- dca(cancer ~ age, data = df_binary, as_probability = "age"),
    NA
  )
  expect_equal(
    dca_binary2 %>%
      as_tibble() %>%
      dplyr::filter(threshold == 0.25) %>%
      dplyr::pull(net_benefit),
    c(-0.14666667, 0.00000000, 0.02444444),
    tolerance = 1e-5
  )

  expect_error(
    dca_at_threshold_zero <- dca(cancer ~ famhistory, data = df_binary, thresholds = 0),
    NA
  )
  expect_equal(
    as_tibble(dca_at_threshold_zero) %>% dplyr::pull(net_benefit),
    c(0.140, 0, 0.032)
  )

  expect_error(
    dca_surv1 <- dca(Surv(ttcancer, cancer) ~ cancerpredmarker, data = df_surv, time = 1),
    NA
  )
  expect_equal(
    dca_surv1 %>%
      as_tibble() %>%
      dplyr::filter(threshold == 0.25) %>%
      dplyr::pull(net_benefit),
    c(-0.13695058, 0.00000000, 0.05695371),
    tolerance = 1e-5
  )

  expect_error(
    dca_surv2 <- dca(Surv(ttcancer, cancer) ~ age, data = df_surv, time = 1, as_probability = "age"),
    NA
  )
  expect_equal(
    dca_surv2 %>%
      as_tibble() %>%
      dplyr::filter(threshold == 0.25) %>%
      dplyr::pull(net_benefit),
    c(-0.1369506, 0.0000000, -0.1424773),
    tolerance = 1e-5
  )

  expect_error(
    dca_case_control <-
      dca(casecontrol ~ cancerpredmarker, data = df_case_control, prevalence = 0.15),
    NA
  )
  expect_equal(
    dca_case_control %>%
      as_tibble() %>%
      dplyr::filter(threshold == 0.25) %>%
      dplyr::pull(net_benefit),
    c(-0.13333333, 0.00000000, 0.01495671),
    tolerance = 1e-5
  )

  expect_error(
    dca(Surv(ttcancer, cancer_cr) ~ cancerpredmarker,
      data = df_surv,
      time = 1.5,
      thresholds = 1:50 / 100
    ),
    NA
  )
})

test_that("dca() errors print with bad inputs", {
  expect_error(
    dca_binary <- dca(formula = letters, data = df_binary)
  )
  expect_error(
    dca_binary <- dca(cancer ~ cancerpredmarker + all, data = df_binary %>% dplyr::mutate(all = 1L))
  )
  expect_error(
    dca_binary <- dca(cancer ~ cancerpredmarker, data = letters)
  )
  expect_error(
    dca_binary <- dca(cancer ~ age, data = df_binary)
  )
  expect_error(
    dca_binary <- dca(cancerpredmarker ~ cancer, data = df_binary)
  )
  expect_error(
    dca_surv <- dca(Surv(ttcancer, cancer) ~ cancerpredmarker, data = df_surv),
  )
  expect_error(
    dca_surv <- dca(Surv(ttcancer, cancer) ~ cancerpredmarker, data = df_surv, time = 1000)
  )
})
