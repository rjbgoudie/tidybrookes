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
      (value_as_number %in% 0:17))

fsheet_news2_example <- fsheet_extract(fsheet_raw_test, fsheet_def)

usethis::use_data(fsheet_news2_example, overwrite = TRUE)
