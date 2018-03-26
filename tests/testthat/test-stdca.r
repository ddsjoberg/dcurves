library(MASS)
library(survival)

data.set <- Melanoma
data.set$diedcancer = ifelse(data.set$status==1, 1, 0)


test_that("Simple case runs without error, testing survival, failure, and expected type", {
  expect_error(
    stdca(data=data.set, outcome="diedcancer", ttoutcome="time", timepoint=545,
          predictors="thickness", probability=FALSE, xstop=.25 , graph = F)
    , NA
  )
})
