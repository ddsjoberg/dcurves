test_that("predictor_effect_plot works with categorical predictors (logistic)", {
  data <- mtcars
  data$am <- as.factor(data$am)
  data$vs <- as.factor(data$vs)

  p <- predictor_effect_plot(
    data = data,
    x = "am",
    y = "vs",
    method = "categorical",
    save_plot = FALSE
  )

  expect_s3_class(p, "ggplot")
  # Check that it creates a pointrange plot
  expect_true("GeomCrossbar" %in% sapply(p$layers, function(l) class(l$geom)[1]))
})

test_that("predictor_effect_plot works with categorical predictors (cox)", {
  data <- survival::cancer
  data$sex <- as.factor(data$sex)
  
  p <- predictor_effect_plot(
    data = data,
    x = "sex",
    y = "status",
    time = "time",
    method = "categorical",
    save_plot = FALSE
  )
  
  expect_s3_class(p, "ggplot")
  expect_true("GeomCrossbar" %in% sapply(p$layers, function(l) class(l$geom)[1]))
})


test_that("predictor_effect_plot works with linear numeric predictors (logistic)", {
  p <- predictor_effect_plot(
    data = mtcars,
    x = "wt",
    y = "vs",
    method = "linear",
    save_plot = FALSE
  )

  expect_s3_class(p, "ggplot")
  # Check that it creates a line plot
  expect_true("GeomLine" %in% sapply(p$layers, function(l) class(l$geom)[1]))
})

test_that("predictor_effect_plot works with linear numeric predictors (cox)", {
  p <- predictor_effect_plot(
    data = survival::cancer,
    x = "age",
    y = "status",
    time = "time",
    method = "linear",
    save_plot = FALSE
  )
  
  expect_s3_class(p, "ggplot")
  expect_true("GeomLine" %in% sapply(p$layers, function(l) class(l$geom)[1]))
})


test_that("rcs_plot wrapper works as expected (equivalent to method='rcs')", {
  # Using the wrapper
  p_wrapper <- rcs_plot(
    data = survival::cancer,
    x = "age",
    y = "status",
    time = "time",
    covars = "ph.karno",
    save_plot = FALSE
  )

  # Using the main function directly
  p_main <- predictor_effect_plot(
    data = survival::cancer,
    x = "age",
    y = "status",
    time = "time",
    covars = "ph.karno",
    method = "rcs",
    save_plot = FALSE,
  )

  expect_s3_class(p_wrapper, "ggplot")
  expect_s3_class(p_main, "ggplot")

  # The plots should be identical
  expect_equal(p_wrapper$data, p_main$data)
  expect_equal(length(p_wrapper$layers), length(p_main$layers))
  
  # A simple check on labels
  expect_equal(p_wrapper$labels$y, p_main$labels$y)
})

test_that("auto method correctly identifies predictor type", {
  # Factor -> categorical
  data_cat <- mtcars
  data_cat$cyl <- as.factor(data_cat$cyl)
  p_cat <- predictor_effect_plot(
    data = data_cat,
    x = "cyl",
    y = "vs",
    method = "auto",
    save_plot = FALSE
  )
  expect_true("GeomCrossbar" %in% sapply(p_cat$layers, function(l) class(l$geom)[1]))
})

test_that("Function handles missing data", {
   data <- mtcars
   data$mpg[1:5] <- NA
   expect_warning(
     predictor_effect_plot(data, x = "mpg", y = "vs", save_plot = FALSE),
     "5 incomplete cases excluded."
   )
})


test_that("predictor_effect_plot produces expected images (cox, rcs)", {
  withr::with_tempdir({
    data <- survival::cancer
    p <- predictor_effect_plot(
      data = data,
      x = "age",
      y = "status",
      time = "time",
      method = "rcs",
      covars = "ph.karno",
      save_plot = TRUE
    )

    expect_s3_class(p, "ggplot")
    vdiffr::expect_doppelganger("predictor effect cox rcs age", p)
    # when save_plot=TRUE a file should be written
    expect_true(any(grepl(".png$", list.files())))
  })
})

test_that("predictor_effect_plot produces expected images (logistic, linear)", {
  withr::with_tempdir({
    data <- survival::cancer
    data$dead <- data$status == 2
    data=data[!is.na(data$inst),]

    p_log <- predictor_effect_plot(
      data = data,
      x = "age",
      y = "dead",
      method = "linear",
      covars = "ph.karno",
      save_plot = FALSE
    )
    expect_s3_class(p_log, "ggplot")
    vdiffr::expect_doppelganger("predictor effect logistic linear age", p_log)

    p_log2 <- predictor_effect_plot(
      data = data,
      x = "age",
      y = "dead",
      method = "linear",
      covars = "ph.karno",
      add_hist = FALSE,
      trans = "log2",
      save_plot = FALSE
    )
    expect_s3_class(p_log2, "ggplot")
    vdiffr::expect_doppelganger("predictor effect logistic linear age trans", p_log)

    p_log3 <- predictor_effect_plot(
      data = data,
      x = "age",
      y = "dead",
      method = "linear",
      covars = "ph.karno",
      add_hist = FALSE,
      trans = "log2",
      save_plot = FALSE,
      cluster = "inst"
    )
    expect_s3_class(p_log3, "ggplot")
    vdiffr::expect_doppelganger("predictor effect logistic trans cluster", p_log)

    p_cat <- predictor_effect_plot(
      data = data,
      x = "sex",
      y = "dead",
      method = "categorical",
      save_plot = FALSE
    )
    expect_s3_class(p_cat, "ggplot")
    vdiffr::expect_doppelganger("predictor effect logistic categorical sex", p_cat)
  })
})
