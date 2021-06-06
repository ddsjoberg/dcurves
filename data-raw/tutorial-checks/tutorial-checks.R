library(dcurves)

# import data
df_tutorial <-
  here::here("data-raw", "tutorial-checks", "dca.csv") %>%
  readr::read_csv()

# page 6
dca(cancer ~ famhistory,
    data = df_tutorial,
    thresholds = 1:35 / 100)

# page 8
dca(cancer ~ famhistory + cancerpredmarker,
    data = df_tutorial,
    thresholds = 1:35 / 100)

# page 18
dca(cancer ~ marker,
    data = df_tutorial,
    thresholds = 5:35 / 100,
    as_probability = "marker") %>%
  net_intervention_avoided()

# page 22
df_tutorial2 <-
  survival::coxph(Surv(ttcancer, cancer) ~ age + famhistory + marker, data = df_tutorial) %>%
  broom::augment(
    newdata = df_surv %>% dplyr::mutate(ttcancer =  1.5),
    type.predict = "expected"
  ) %>%
  dplyr::mutate(
    pr_failure18 = 1 - exp(-.fitted)
  ) %>%
  dplyr::select(pr_failure18) %>%
  dplyr::bind_cols(df_tutorial)

dca(Surv(ttcancer, cancer) ~ pr_failure18,
    data = df_tutorial2,
    time = 1.5,
    thresholds = 1:50 / 100)


