## code to prepare `DATASET` dataset goes here
set.seed(20210530)

df_dca_full <- readr::read_csv(here::here("data-raw", "dca.csv"))

# binary endpoint data
df_binary <- dplyr::select(df_dca_full, -ttcancer, -casecontrol)
df_surv <-
  df_dca_full %>%
  dplyr::mutate(
    cancer_cr =
      dplyr::case_when(
        cancer == 1 ~ "diagnosed with cancer",
        runif(dplyr::n()) < 0.9 ~ "censor",
        TRUE  ~ "dead other causes"
      ) %>%
      factor(levels = c("censor", "diagnosed with cancer", "dead other causes"))
  ) %>%
  dplyr::select(-dead, -casecontrol)
df_surv %>%
  dplyr::select(cancer, cancer_cr) %>%
  gtsummary::tbl_cross()


df_case_control <- dplyr::select(df_dca_full, patientid, casecontrol,
                                  dplyr::everything(), -cancer, -dead, -ttcancer)

usethis::use_data(df_binary, df_surv, df_case_control, overwrite = TRUE)
