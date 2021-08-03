test_that("chooses within visit", {
  fsheet_raw_test <-
    read_tidybrookes_csv(
      file = tidybrookes_example("fsheet.csv"),
      col_types = "fsheet"
      ) %>%
    fsheet_rename %>%
    filter(person_id == "AA")

  fsheet_def <- list() %>%
    fsheet_add(
      symbol = "weight",
      title = "Weight",
      names = c("Weight"),
      search_pattern = c("weight"),
      search_exclude = c(),
      unit_rescale_fn = case_when(TRUE ~ value_as_number * 28.35 / 1000),
      unit_relabel_fn = case_when(TRUE ~ "kg"))

  fsheet_data_test <- fsheet_extract(fsheet_raw_test, fsheet_def)

  demo_adm_raw <-
    read_tidybrookes_csv(
      file = tidybrookes_example("adm.csv"),
      col_types = "adm"
      ) %>%
    adm_rename %>%
    filter(person_id == "AA")

  joined <- demo_adm_raw %>%
    fsheet_first_during(fsheet_data_test,
      during = "during_visit")

  expect_equal(
    joined$weight_first_during_visit,
    4125 * 28.35 / 1000)
  expect_equal(
    joined$weight_first_during_visit_datetime,
    ymd_hms("2021-01-03 17:00:00", tz = "Europe/London"))
})

test_that("chooses first within visit", {
  fsheet_raw_test <-
    read_tidybrookes_csv(
      file = tidybrookes_example("fsheet.csv"),
      col_types = "fsheet"
      ) %>%
    fsheet_rename %>%
    filter(person_id == "CC")

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

  fsheet_data_test <- fsheet_extract(fsheet_raw_test, fsheet_def)

  demo_adm_raw <-
    read_tidybrookes_csv(
      file = tidybrookes_example("adm.csv"),
      col_types = "adm"
      ) %>%
    adm_rename %>%
    filter(person_id == "CC")

  joined <- demo_adm_raw %>%
    fsheet_first_during(fsheet_data_test,
      during = "during_visit")

  expect_equal(
    joined$news2_first_during_visit,
    5)
  expect_equal(
    joined$news2_first_during_visit_datetime,
    ymd_hms("2021-01-01 08:59:00", tz = "Europe/London"))
})



test_that("two person ", {
  fsheet_raw_test <-
    read_tidybrookes_csv(
      file = tidybrookes_example("fsheet.csv"),
      col_types = "fsheet") %>%
    fsheet_rename %>%
    filter(person_id %in% c("AA", "BB"))

  fsheet_def <- list() %>%
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

  fsheet_data_test <- fsheet_extract(fsheet_raw_test, fsheet_def)

  demo_adm_raw <-
    read_tidybrookes_csv(
      file = tidybrookes_example("adm.csv"),
      col_types = "adm"
      ) %>%
    adm_rename %>%
    filter(person_id %in% c("AA", "BB"))

  joined <- demo_adm_raw %>%
    fsheet_first_during(fsheet_data_test,
      during = "during_visit")

  expect_equal(
    joined$weight_first_during_visit,
    c(4125 * 28.35 / 1000, NA_real_))
  expect_identical(
    joined$weight_first_during_visit_datetime,
    c(ymd_hms("2021-01-03 17:00:00", tz = "Europe/London"),
      ymd_hms(NA, tz = "Europe/London")))

  expect_equal(
    joined$spo2_first_during_visit,
    c(NA_real_, 93))
  expect_identical(
    joined$spo2_first_during_visit_datetime,
    c(ymd_hms(NA, tz = "Europe/London"),
      ymd_hms("2021-01-21 08:59:00", tz = "Europe/London")))
})


test_that("median within visit", {
  fsheet_raw_test <-
    read_tidybrookes_csv(
      file = tidybrookes_example("fsheet.csv"),
      col_types = "fsheet"
      ) %>%
    fsheet_rename %>%
    filter(person_id == "AA")

  fsheet_def <- list() %>%
    fsheet_add(
      symbol = "weight",
      title = "Weight",
      names = c("Weight"),
      search_pattern = c("weight"),
      search_exclude = c(),
      unit_rescale_fn = case_when(TRUE ~ value_as_number * 28.35 / 1000),
      unit_relabel_fn = case_when(TRUE ~ "kg"))

  fsheet_data_test <- fsheet_extract(fsheet_raw_test, fsheet_def)

  demo_adm_raw <-
    read_tidybrookes_csv(
      file = tidybrookes_example("adm.csv"),
      col_types = "adm"
      ) %>%
    adm_rename %>%
    filter(person_id == "AA")

  joined <- demo_adm_raw %>%
    fsheet_summarise_during(
      fsheet_data_test,
      during = "during_visit",
      type = "summarise",
      formula = tibble(value_as_number = median(value_as_number),
                       datetime = NA),
      names_suffix = "median")

  expect_equal(
    joined$weight_median_during_visit,
    median(c(4125 * 28.35 / 1000)))
  expect_equal(
    joined$weight_median_during_visit_datetime,
    NA)
})

test_that("median and max within visit, with multiple results", {
  fsheet_raw_test <-
    read_tidybrookes_csv(
      file = tidybrookes_example("fsheet.csv"),
      col_types = "fsheet"
    ) %>%
    fsheet_rename %>%
    filter(person_id == "CC")

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

  fsheet_data_test <- fsheet_extract(fsheet_raw_test, fsheet_def)

  demo_adm_raw <-
    read_tidybrookes_csv(
      file = tidybrookes_example("adm.csv"),
      col_types = "adm"
      ) %>%
    adm_rename %>%
    filter(person_id == "CC")

  joined <- demo_adm_raw %>%
    fsheet_summarise_during(
      fsheet_data_test,
      during = "during_visit",
      type = "summarise",
      formula = tibble(value_as_number = median(value_as_number),
                       datetime = NA),
      names_suffix = "median"
    )

  expect_equal(
    joined$news2_median_during_visit,
    median(c(10, 12, 5)))
  expect_equal(
    joined$news2_median_during_visit_datetime,
    NA)


  # test max during visit
  joined2 <- demo_adm_raw %>%
    fsheet_max_during(fsheet_data_test,
                      during = "during_visit")

  expect_equal(
    joined2$news2_max_during_visit,
    max(c(10, 12, 5)))
  expect_equal(
    joined2$news2_max_during_visit_datetime,
    ymd_hms("2021-01-04 09:00:00", tz = "Europe/London"))

  # test max during initial 24h of visit
  joined3 <- demo_adm_raw %>%
    fsheet_max_during(fsheet_data_test,
                      during = "during_visit_initial_24h")

  expect_equal(
    joined3$news2_max_during_visit_initial_24h,
    max(c(5)))
  expect_equal(
    joined3$news2_max_during_visit_initial_24h_datetime,
    ymd_hms("2021-01-01 08:59:00", tz = "Europe/London"))
})

test_that("fsheet_all_during retains patients without fsheet data", {
  # DD has no NEWS2 data
  fsheet_raw_test <-
    read_tidybrookes_csv(
      file = tidybrookes_example("fsheet.csv"),
      col_types = "fsheet"
      ) %>%
    fsheet_rename %>%
    filter(person_id %in% c("CC", "DD"))

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

  fsheet_data_test <- fsheet_extract(fsheet_raw_test, fsheet_def)

  demo_adm_raw <-
    read_tidybrookes_csv(
      file = tidybrookes_example("adm.csv"),
      col_types = "adm"
      ) %>%
    adm_rename %>%
    filter(person_id %in% c("CC", "DD"))

  joined <- demo_adm_raw %>%
    fsheet_all_during(
      fsheet_data_test,
      during = "during_visit")


  expect_equal(joined$symbol, c(rep("news2", 3), NA))
  expect_equal(
    joined$value_as_number,
    c(c(5, 10, 12), NA))
  expect_equal(
    joined$datetime,
    c(ymd_hms("2021-01-01 08:59:00", tz = "Europe/London"),
      ymd_hms("2021-01-03 17:00:00", tz = "Europe/London"),
      ymd_hms("2021-01-04 09:00:00", tz = "Europe/London"),
      ymd_hms(NA, tz = "Europe/London")))
})

test_that("character fsheet data handled", {
  fsheet_raw_test <-
    read_tidybrookes_csv(
      file = tidybrookes_example("fsheet.csv"),
      col_types = "fsheet"
    ) %>%
    fsheet_rename %>%
    filter(person_id %in% "EE")

  fsheet_def <- list() %>%
    fsheet_add(
      symbol = "acvpu",
      title = "ACVPU",
      names = c("NEWS2/MEOWS: ACVPU"),
      search_pattern = c("acvpu"),
      search_exclude = c(),
      type = "character",
      expect_after =
        (value_as_character %in% c("A", "C", "V", "P", "U")))

    fsheet_data_test <- fsheet_extract(fsheet_raw_test, fsheet_def)

    demo_adm_raw <-
      read_tidybrookes_csv(
        file = tidybrookes_example("adm.csv"),
        col_types = "adm"
      ) %>%
      adm_rename %>%
      filter(person_id %in% c("EE"))

  joined <- demo_adm_raw %>%
        fsheet_all_during(
          fsheet_data_test,
          during = "during_visit")

  expect_equal(joined$symbol, c("acvpu"))
  expect_equal(joined$value_as_number, c(NA_real_))
  expect_equal(joined$value_as_character, c("A"))
})

test_that("mixed numeric and character fsheet data handled", {
  fsheet_raw_test <-
    read_tidybrookes_csv(
      file = tidybrookes_example("fsheet.csv"),
      col_types = "fsheet"
    ) %>%
    fsheet_rename %>%
    filter(person_id %in% "FF")

  fsheet_def <- list() %>%
    fsheet_add(
      symbol = "acvpu",
      title = "ACVPU",
      names = c("NEWS2/MEOWS: ACVPU"),
      search_pattern = c("acvpu"),
      search_exclude = c(),
      type = "character",
      expect_after =
        (value_as_character %in% c("A", "C", "V", "P", "U"))) %>%
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

  fsheet_data_test <- fsheet_extract(fsheet_raw_test, fsheet_def)

  demo_adm_raw <-
    read_tidybrookes_csv(
      file = tidybrookes_example("adm.csv"),
      col_types = "adm"
    ) %>%
    adm_rename %>%
    filter(person_id %in% c("FF"))

  joined <- demo_adm_raw %>%
    fsheet_all_during(
      fsheet_data_test,
      during = "during_visit")

  expect_equal(joined$symbol, c("news2", "acvpu"))
  expect_equal(joined$value_as_number, c(10, NA_real_))
  expect_equal(joined$value_as_character, c(NA_character_, "P"))
})
