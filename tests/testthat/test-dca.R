test_that("dca() works", {
  expect_error(
    dca_binary <- dca(cancer ~ cancerpredmarker, data = df_binary),
    NA
  )

  expect_error(
    dca_surv <- dca(Surv(ttcancer, cancer) ~ cancerpredmarker, data = df_surv, time = 1),
    NA
  )
})
