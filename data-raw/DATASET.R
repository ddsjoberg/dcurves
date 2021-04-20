## code to prepare `DATASET` dataset goes here

df_dca_full <- readr::read_csv(here::here("data-raw", "dca.csv"))

# binary endpoint data
df_binary <- dplyr::select(df_dca_full, -ttcancer, -casecontrol)
df_surv <- dplyr::select(df_dca_full, -dead, -casecontrol)
df_case_control <- dplyr::select(df_dca_full, patientid, casecontrol,
                                  dplyr::everything(), -cancer, -dead, -ttcancer)

usethis::use_data(df_binary, df_surv, df_case_control, overwrite = TRUE)
