## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(dcurves)
library(gtsummary); library(dplyr); library(tidyr)

## -----------------------------------------------------------------------------
mod <- glm(cancer ~ famhistory, df_binary, family = binomial)
tbl <- tbl_regression(mod, exponentiate = TRUE)
tbl

## -----------------------------------------------------------------------------
dca(cancer ~ famhistory, df_binary) %>%
  plot(smooth = TRUE)

## -----------------------------------------------------------------------------
dca(cancer ~ famhistory, df_binary, thresholds = seq(0, 0.35, by = 0.01)) %>%
  plot(smooth = TRUE)

## -----------------------------------------------------------------------------
glm(cancer ~ marker + age + famhistory, df_binary, family = binomial) %>%
  broom::augment(newdata = df_binary, type.predict = "response")

## -----------------------------------------------------------------------------
dca(cancer ~ famhistory + cancerpredmarker, 
    data = df_binary, 
    thresholds = seq(0, 0.35, by = 0.01)) %>%
  plot(smooth = TRUE)

## -----------------------------------------------------------------------------
df_binary_updated <-
  df_binary %>%
  mutate(
    # Use the coefficients from the Brown model
    logodds_Brown = 0.75 * (famhistory) + 0.26 * (age) - 17.5,
    # Convert to predicted probability
    phat_Brown = exp(logodds_Brown) / (1 + exp(logodds_Brown))
  )

# Run the decision curve
dca(cancer ~ phat_Brown, 
    data = df_binary_updated, 
    thresholds = seq(0, 0.35, by = 0.01),
    label = list(phat_Brown = "Brown Model")) %>%
  plot(smooth = TRUE)

## -----------------------------------------------------------------------------
df_binary_updated <-
  df_binary_updated %>%
  mutate(
    # Create a variable for the strategy of treating only high risk patients
    # This will be 1 for treat and 0 for don't treat
    high_risk = ifelse(risk_group == "high", 1, 0),
    # Treat based on Joint Approach
    joint = ifelse(risk_group == "high" | cancerpredmarker > 0.15, 1, 0),
    # Treat based on Conditional Approach
    conditional = ifelse(risk_group == "high" | 
                           (risk_group == "intermediate" & cancerpredmarker > 0.15), 1, 0)
  )

## -----------------------------------------------------------------------------
dca(cancer ~ high_risk + joint + conditional, 
    data = df_binary_updated, 
    thresholds = seq(0, 0.35, by = 0.01)) %>%
  plot(smooth = TRUE)

## -----------------------------------------------------------------------------
# the harm of measuring the marker is stored in a scalar
harm_marker <- 0.0333

# in the conditional test, only patients at intermediate risk have their marker measured
intermediate_risk <- df_binary_updated$risk_group == "intermediate"

# harm of the conditional approach is proportion of patients who have the marker measured multiplied by the harm
harm_conditional <- mean(intermediate_risk) * harm_marker

# Run the decision curve 
dca_with_harms <-
  dca(cancer ~ high_risk + joint + conditional, 
    data = df_binary_updated, 
    harm = list(joint = harm_marker, conditional = harm_conditional),
    thresholds = seq(0, 0.35, by = 0.01))
plot(dca_with_harms, smooth = TRUE)

## -----------------------------------------------------------------------------
dca_with_harms %>%
  as_tibble() %>%
  filter(threshold %in% seq(0.05, 0.35, by = 0.05)) %>%
  select(variable, threshold, net_benefit) %>%
  pivot_wider(id_cols = threshold, 
              names_from = variable,
              values_from = net_benefit)

## -----------------------------------------------------------------------------
dca(cancer ~ marker, 
    data = df_binary, 
    as_probability = "marker") %>%
  net_intervention_avoided() %>%
  plot(smooth = TRUE)

## -----------------------------------------------------------------------------
library(survival)
df_surv %>% head()

## -----------------------------------------------------------------------------
# build Cox model
cox_model <- coxph(Surv(ttcancer, cancer) ~ age + famhistory + marker, data = df_surv)

# show summary of model results
tbl_regression(cox_model, exponentiate = TRUE)

# add model prediction to our data frame
df_surv_updated <-
  broom::augment(
    cox_model, 
    newdata = df_surv %>% mutate(ttcancer =  1.5),
    type.predict = "expected"
  ) %>%
    mutate(
      pr_failure18 = 1 - exp(-.fitted)
    ) %>%
    select(-.fitted, -.se.fit)

## -----------------------------------------------------------------------------
dca(Surv(ttcancer, cancer) ~ pr_failure18, 
    data = df_surv_updated,
    time = 1.5,
    thresholds = 1:50 / 100) %>%
  plot(smooth = TRUE)

## -----------------------------------------------------------------------------
dca(Surv(ttcancer, cancer_cr) ~ pr_failure18, 
    data = df_surv_updated,
    time = 1.5,
    thresholds = 1:50 / 100) %>%
  plot(smooth = TRUE)

## -----------------------------------------------------------------------------
dca(casecontrol ~ cancerpredmarker, 
    data = df_case_control,
    prevalence = 0.15) %>%
  plot(smooth = TRUE)

