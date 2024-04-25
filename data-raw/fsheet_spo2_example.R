fsheet_raw_test <-
  read_tidybrookes_csv(
    file = tidybrookes_example("fsheet.csv"),
    col_types = "fsheet"
  ) %>%
  fsheet_rename


fsheet_def <- list() %>%
  fsheet_add(
    symbol = "spo2",
    title = "Oxygen saturations",
    names = c("SpO2"),
    search_pattern = c("spo2", "sats"),
    search_exclude = c(),
    range_discard_below = 50)

fsheet_spo2_example <- fsheet_extract(fsheet_raw_test, fsheet_def)

usethis::use_data(fsheet_spo2_example, overwrite = TRUE)
