fsheet_raw_test <-
  read_tidybrookes_csv(
    file = tidybrookes_example("fsheet.csv"),
    col_types = "fsheet"
  ) %>%
  fsheet_rename

fsheet_def <- list() %>%
  fsheet_add(
    symbol = "weight",
    title = "Weight",
    names = c("Weight"),
    search_pattern = c("weight"),
    search_exclude = c(),
    unit_rescale_fn = case_when(TRUE ~ value_as_number * 28.35 / 1000),
    unit_relabel_fn = case_when(TRUE ~ "kg"))

fsheet_data_example <- fsheet_extract(fsheet_raw_test, fsheet_def)
