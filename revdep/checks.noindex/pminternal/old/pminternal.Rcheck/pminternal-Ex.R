pkgname <- "pminternal"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('pminternal')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("boot_optimism")
### * boot_optimism

flush(stderr()); flush(stdout())

### Name: boot_optimism
### Title: Calculate optimism and bias-corrected scores via bootstrap
###   resampling
### Aliases: boot_optimism

### ** Examples

library(pminternal)
set.seed(456)
# simulate data with two predictors that interact
dat <- pmcalibration::sim_dat(N = 1000, a1 = -2, a3 = -.3)
mean(dat$y)
dat$LP <- NULL # remove linear predictor

# fit a (misspecified) logistic regression model
model_fun <- function(data, ...){
  glm(y ~ x1 + x2, data=data, family="binomial")
}

pred_fun <- function(model, data, ...){
  predict(model, newdata=data, type="response")
}

boot_optimism(data=dat, outcome="y", model_fun=model_fun, pred_fun=pred_fun,
              method="boot", B=20) # B set to 20 for example but should be >= 200




cleanEx()
nameEx("cal_plot")
### * cal_plot

flush(stderr()); flush(stdout())

### Name: cal_plot
### Title: Plot apparent and bias-corrected calibration curves
### Aliases: cal_plot

### ** Examples

library(pminternal)
set.seed(456)
# simulate data with two predictors that interact
dat <- pmcalibration::sim_dat(N = 2000, a1 = -2, a3 = -.3)
mean(dat$y)
dat$LP <- NULL # remove linear predictor

# fit a (misspecified) logistic regression model
m1 <- glm(y ~ x1 + x2, data=dat, family="binomial")

# to get a plot of bias-corrected calibration we need
# to specify 'eval' argument via 'calib_args'
# this argument specifies at what points to evalulate the
# calibration curve for plotting. The example below uses
# 100 equally spaced points between the min and max
# original prediction.

p <- predict(m1, type="response")
p100 <- seq(min(p), max(p), length.out=100)

m1_iv <- validate(m1, method="cv_optimism", B=10,
                  calib_args = list(eval=p100))
# calib_ags can be used to set other calibration curve
# settings: see pmcalibration::pmcalibration

cal_plot(m1_iv)




cleanEx()
nameEx("calibration_stability")
### * calibration_stability

flush(stderr()); flush(stdout())

### Name: calibration_stability
### Title: Plot calibration stability across bootstrap replicates
### Aliases: calibration_stability

### ** Examples




cleanEx()
nameEx("classification_stability")
### * classification_stability

flush(stderr()); flush(stdout())

### Name: classification_stability
### Title: Classification instability plot
### Aliases: classification_stability

### ** Examples

set.seed(456)
# simulate data with two predictors that interact
dat <- pmcalibration::sim_dat(N = 2000, a1 = -2, a3 = -.3)
mean(dat$y)
dat$LP <- NULL # remove linear predictor

# fit a (misspecified) logistic regression model
m1 <- glm(y ~ ., data=dat, family="binomial")

# internal validation of m1 via bootstrap optimism with 10 resamples
# B = 10 for example but should be >= 200 in practice
m1_iv <- validate(m1, method="boot_optimism", B=10)

classification_stability(m1_iv, threshold=.2)




cleanEx()
nameEx("crossval")
### * crossval

flush(stderr()); flush(stdout())

### Name: crossval
### Title: Calculate bias-corrected scores via cross-validation
### Aliases: crossval

### ** Examples

library(pminternal)
set.seed(456)
# simulate data with two predictors that interact
dat <- pmcalibration::sim_dat(N = 1000, a1 = -2, a3 = -.3)
mean(dat$y)
dat$LP <- NULL # remove linear predictor

# fit a (misspecified) logistic regression model
#m1 <- glm(y ~ x1 + x2, data=dat, family="binomial")

model_fun <- function(data, ...){
  glm(y ~ x1 + x2, data=data, family="binomial")
}

pred_fun <- function(model, data, ...){
  predict(model, newdata=data, type="response")
}

# CV Corrected = Apparent - CV Optimism
# CV Average = average score in held out fold
crossval(data=dat, outcome="y", model_fun=model_fun, pred_fun=pred_fun, k=10)




cleanEx()
nameEx("dcurve_stability")
### * dcurve_stability

flush(stderr()); flush(stdout())

### Name: dcurve_stability
### Title: Plot decision curve stability across bootstrap replicates
### Aliases: dcurve_stability

### ** Examples




cleanEx()
nameEx("mape_stability")
### * mape_stability

flush(stderr()); flush(stdout())

### Name: mape_stability
### Title: Mean absolute predictor error (MAPE) stability plot
### Aliases: mape_stability

### ** Examples

set.seed(456)
# simulate data with two predictors that interact
dat <- pmcalibration::sim_dat(N = 2000, a1 = -2, a3 = -.3)
mean(dat$y)
dat$LP <- NULL # remove linear predictor

# fit a (misspecified) logistic regression model
m1 <- glm(y ~ ., data=dat, family="binomial")

# internal validation of m1 via bootstrap optimism with 10 resamples
# B = 10 for example but should be >= 200 in practice
m1_iv <- validate(m1, method="boot_optimism", B=10)

mape_stability(m1_iv)




cleanEx()
nameEx("prediction_stability")
### * prediction_stability

flush(stderr()); flush(stdout())

### Name: prediction_stability
### Title: Plot prediction stability across bootstrap replicates
### Aliases: prediction_stability

### ** Examples

set.seed(456)
# simulate data with two predictors that interact
dat <- pmcalibration::sim_dat(N = 2000, a1 = -2, a3 = -.3)
mean(dat$y)
dat$LP <- NULL # remove linear predictor

# fit a (misspecified) logistic regression model
m1 <- glm(y ~ ., data=dat, family="binomial")

# internal validation of m1 via bootstrap optimism with 10 resamples
# B = 10 for example but should be >= 200 in practice
m1_iv <- validate(m1, method="boot_optimism", B=10)

prediction_stability(m1_iv)




cleanEx()
nameEx("score_binary")
### * score_binary

flush(stderr()); flush(stdout())

### Name: score_binary
### Title: Score predictions for binary events
### Aliases: score_binary

### ** Examples

p <- runif(100)
y <- rbinom(length(p), 1, p)
score_binary(y = y, p = p)



cleanEx()
nameEx("summary.internal_validate")
### * summary.internal_validate

flush(stderr()); flush(stdout())

### Name: summary.internal_validate
### Title: Summarize a internal_validate object
### Aliases: summary.internal_validate

### ** Examples

library(pminternal)
set.seed(456)
# simulate data with two predictors that interact
dat <- pmcalibration::sim_dat(N = 2000, a1 = -2, a3 = -.3)
mean(dat$y)
dat$LP <- NULL # remove linear predictor

# fit a (misspecified) logistic regression model
m1 <- glm(y ~ ., data=dat, family="binomial")

# internal validation of m1 via bootstrap optimism with 10 resamples
# B = 10 for example but should be >= 200 in practice
m1_iv <- validate(m1, method="boot_optimism", B=10)
summary(m1_iv)




cleanEx()
nameEx("validate")
### * validate

flush(stderr()); flush(stdout())

### Name: validate
### Title: Get bias-corrected performance measures via bootstrapping or
###   cross-validation
### Aliases: validate

### ** Examples

library(pminternal)
set.seed(456)
# simulate data with two predictors that interact
dat <- pmcalibration::sim_dat(N = 2000, a1 = -2, a3 = -.3)
mean(dat$y)
dat$LP <- NULL # remove linear predictor

# fit a (misspecified) logistic regression model
m1 <- glm(y ~ ., data=dat, family="binomial")

# internal validation of m1 via bootstrap optimism with 10 resamples
# B = 10 for example but should be >= 200 in practice
m1_iv <- validate(m1, method="boot_optimism", B=10)
m1_iv




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
