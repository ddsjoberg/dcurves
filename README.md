
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis build status](https://travis-ci.org/ddsjoberg/dca.svg?branch=master)](https://travis-ci.org/ddsjoberg/dca) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/ddsjoberg/dca?branch=master&svg=true)](https://ci.appveyor.com/project/ddsjoberg/dca) [![Coverage status](https://codecov.io/gh/ddsjoberg/dca/branch/master/graph/badge.svg)](https://codecov.io/github/ddsjoberg/dca?branch=master) <!-- [![CRAN status](https://www.r-pkg.org/badges/version/dca)](https://cran.r-project.org/package=dca) -->

Decision Curve Analysis (dca)
=============================

Diagnostic and prognostic models are typically evaluated with measures of accuracy that do not address clinical consequences. Decision-analytic techniques allow assessment of clinical outcomes but often require collection of additional information and may be cumbersome to apply to models that yield a continuous result. Decision curve analysis is a method for evaluating and comparing prediction models that incorporates clinical consequences, requires only the data set on which the models are tested, and can be applied to models that have either continuous or dichotomous results.

Installation
------------

You can install dca from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ddsjoberg/dca")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
library(MASS)
library(dca)
data.set <- birthwt
model = glm(low ~ age + lwt, family=binomial(link="logit"), data=data.set)
data.set$predlow = predict(model, type="response")
results = dca(data=data.set, outcome="low", predictors=c("age", "lwt"), probability=c("FALSE", "FALSE"))
#> [1] "age converted to a probability with logistic regression. Due to linearity assumption, miscalibration may occur."
#> [1] "lwt converted to a probability with logistic regression. Due to linearity assumption, miscalibration may occur."
```

<img src="man/figures/README-example-1.png" width="100%" />
