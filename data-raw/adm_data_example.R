adm_data_example <-
  read_tidybrookes_csv(
    file = tidybrookes_example("adm.csv"),
    col_types = "adm"
  ) %>%
  adm_rename

usethis::use_data(adm_data_example, overwrite = TRUE)
