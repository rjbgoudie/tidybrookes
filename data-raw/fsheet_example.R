fsheet_raw_test <-
  read_tidybrookes_csv(
    file = tidybrookes_example("fsheet.csv"),
    col_types = "fsheet"
  ) %>%
  fsheet_rename


fsheet_def <- list() %>%
  fsheet_add(
    symbol = "news2",
    title = "NEWS2",
    names = c("NEWS2 score"),
    search_pattern = c("news2", "news"),
    search_exclude = c(),
    silently_exclude_na_when = TRUE,
    silently_exclude_when =
      (value_original == " " |
         value_original == "E" # some kind of typo
      ),
    value_as_number_fn =
      case_when(value_original == "3mm" ~ 3,
                TRUE ~ value_as_number),
    expect_after =
      (value_as_number %in% 0:17)) %>%
  fsheet_add(
    symbol = "spo2",
    title = "Oxygen saturations",
    names = c("SpO2"),
    search_pattern = c("spo2", "sats"),
    search_exclude = c(),
    range_discard_below = 50) %>%
  fsheet_add(
    symbol = "weight",
    title = "Weight",
    names = c("Weight"),
    search_pattern = c("weight"),
    search_exclude = c(),
    unit_rescale_fn = case_when(TRUE ~ value_as_number * 28.35 / 1000),
    unit_relabel_fn = case_when(TRUE ~ "kg"))

fsheet_example <- fsheet_extract(fsheet_raw_test, fsheet_def)

usethis::use_data(fsheet_example, overwrite = TRUE)
