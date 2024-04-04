test_that("test_consequences() works", {
  # errors should be triggered
  expect_error(
    test_consequences(formula = cancer ~ cancerpredmarker, data = letters)
  )
  expect_error(
    test_consequences(formula = cancer ~ cancerpredmarker, data = df_binary, label = letters)
  )
  expect_error(
    test_consequences(formula = letters, data = mtcars)
  )

  expect_error(
    df <- test_consequences(formula = cancer ~ cancerpredmarker + famhistory, data = df_binary,
                            thresholds = 0.5),
    NA
  )

  tbl <- with(df_binary, table(famhistory, cancer))
  a <- tbl[2, 2]; b <- tbl[2, 1]; c <- tbl[1, 2]; d <- tbl[1, 1]
  expect_equal(
    df %>%
      dplyr::filter(variable == "famhistory") %>%
      dplyr::select(-c(variable, label)) %>%
      tidyr::pivot_longer(cols = dplyr::everything()) %>%
      tibble::deframe() %>%
      as.list(),
    list(n = nrow(df_binary),
         threshold = 0.5,
         pos_rate = (a + c) / (a + b + c + d),
         neg_rate  = (b + d) / (a + b + c + d),
         test_pos_rate  = (a + b) / (a + b + c + d),
         test_neg_rate  = (c + d) / (a + b + c + d),
         tp_rate  = a / (a + b + c + d),
         fp_rate  = b / (a + b + c + d),
         fn_rate  = c / (a + b + c + d),
         tn_rate  = d / (a + b + c + d),
         ppv  = a / (a + b),
         npv  = d / (c + d),
         sens  = a / (a + c),
         spec  = d / (b + d),
         lr_pos  = (a / (a + c)) / (1 - (d / (b + d))),
         lr_neg  = (1 - (a / (a + c))) / (d / (b + d)))
  )

  expect_no_error({
    if ("future.apply" %in% rownames(utils::installed.packages())) {
      future::plan("multisession", workers = 2)
      test_consequences(formula = cancer ~ cancerpredmarker + marker,
                        data = df_binary, parallel = TRUE)
    }
  })
})
