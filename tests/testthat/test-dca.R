test_that("dca() works", {
  expect_error(
    dca_binary <- dca(cancer ~ cancerpredmarker, data = df_binary),
    NA
  )
  expect_error(
    dca_binary <- dca(cancer ~ age, data = df_binary, as_probability = "age"),
    NA
  )

  expect_error(
    dca_surv <- dca(Surv(ttcancer, cancer) ~ cancerpredmarker, data = df_surv, time = 1),
    NA
  )
  expect_error(
    dca_surv <- dca(Surv(ttcancer, cancer) ~ age, data = df_surv, time = 1, as_probability = "age"),
    NA
  )

  expect_error(
    dca_surv <- dca(casecontrol ~ cancerpredmarker, data = df_case_control, prevalence = 0.15),
    NA
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
