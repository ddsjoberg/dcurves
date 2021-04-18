test_that("dca() works", {
  dca(cancer ~ cancerpredmarker, data = df_dca)
})
