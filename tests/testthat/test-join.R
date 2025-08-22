dt <- lubridate::ymd_hms

test_that("chooses within visit", {
  fsheet_raw_test <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_fsheet.csv"),
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

  suppressMessages({
    fsheet_data_test <- fsheet_extract(fsheet_raw_test, fsheet_def)
  })

  demo_adm_raw <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_adm.csv"),
      col_types = "adm"
    ) %>%
    adm_rename %>%
    filter(person_id == "AA")

  suppressMessages({
    joined <- demo_adm_raw %>%
      first_during(fsheet_data_test,
                   datetime = measurement_datetime,
                   during = "during_visit")
  })

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
      file = tidybrookes_example("test_fsheet.csv"),
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

  suppressMessages({
    fsheet_data_test <- fsheet_extract(fsheet_raw_test, fsheet_def)
  })

  demo_adm_raw <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_adm.csv"),
      col_types = "adm"
    ) %>%
    adm_rename %>%
    filter(person_id == "CC")

  suppressMessages({
    joined <- demo_adm_raw %>%
      first_during(fsheet_data_test,
                   datetime = measurement_datetime,
                   during = "during_visit")
  })

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
      file = tidybrookes_example("test_fsheet.csv"),
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

  suppressMessages({
    fsheet_data_test <- fsheet_extract(fsheet_raw_test, fsheet_def)
  })

  demo_adm_raw <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_adm.csv"),
      col_types = "adm"
    ) %>%
    adm_rename %>%
    filter(person_id %in% c("AA", "BB"))

  suppressMessages({
    joined <- demo_adm_raw %>%
      first_during(fsheet_data_test,
                   datetime = measurement_datetime,
                   during = "during_visit")
  })
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
      file = tidybrookes_example("test_fsheet.csv"),
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

  suppressMessages({
    fsheet_data_test <- fsheet_extract(fsheet_raw_test, fsheet_def)
  })

  demo_adm_raw <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_adm.csv"),
      col_types = "adm"
    ) %>%
    adm_rename %>%
    filter(person_id == "AA")

  suppressMessages({
    joined <- demo_adm_raw %>%
      summarise_during(
        fsheet_data_test,
        datetime = measurement_datetime,
        during = "during_visit",
        type = "summarise",
        formula = tibble(value_as_number = median(value_as_number),
                         datetime = NA),
        names_suffix = "median")
  })

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
      file = tidybrookes_example("test_fsheet.csv"),
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

  suppressMessages({
    fsheet_data_test <- fsheet_extract(fsheet_raw_test, fsheet_def)
  })

  demo_adm_raw <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_adm.csv"),
      col_types = "adm"
    ) %>%
    adm_rename %>%
    filter(person_id == "CC")

  suppressMessages({
    joined <- demo_adm_raw %>%
      summarise_during(
        fsheet_data_test,
        datetime = measurement_datetime,
        during = "during_visit",
        type = "summarise",
        formula = tibble(value_as_number = median(value_as_number),
                         datetime = NA),
        names_suffix = "median"
      )
  })

  expect_equal(
    joined$news2_median_during_visit,
    median(c(10, 12, 5)))
  expect_equal(
    joined$news2_median_during_visit_datetime,
    NA)


  suppressMessages({
    # test max during visit
    joined2 <- demo_adm_raw %>%
      max_during(fsheet_data_test,
                 datetime = measurement_datetime,
                 during = "during_visit")
  })
  expect_equal(
    joined2$news2_max_during_visit,
    max(c(10, 12, 5)))
  expect_equal(
    joined2$news2_max_during_visit_datetime,
    ymd_hms("2021-01-04 09:00:00", tz = "Europe/London"))

  suppressMessages({
  # test max during initial 24h of visit
  joined3 <- demo_adm_raw %>%
    max_during(fsheet_data_test,
               datetime = measurement_datetime,
               during = "during_visit_initial_24h")
  })

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
      file = tidybrookes_example("test_fsheet.csv"),
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

    suppressMessages({
      fsheet_data_test <- fsheet_extract(fsheet_raw_test, fsheet_def)
    })

    demo_adm_raw <-
      read_tidybrookes_csv(
        file = tidybrookes_example("test_adm.csv"),
        col_types = "adm"
      ) %>%
      adm_rename %>%
      filter(person_id %in% c("CC", "DD"))

    suppressMessages({
      joined <- demo_adm_raw %>%
        all_during(
          fsheet_data_test,
          datetime = measurement_datetime,
          during = "during_visit")
    })

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
      file = tidybrookes_example("test_fsheet.csv"),
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

  suppressMessages({
    fsheet_data_test <- fsheet_extract(fsheet_raw_test, fsheet_def)
  })

  demo_adm_raw <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_adm.csv"),
      col_types = "adm"
    ) %>%
    adm_rename %>%
    filter(person_id %in% c("EE"))

  suppressMessages({
    joined <- demo_adm_raw %>%
      all_during(
        fsheet_data_test,
        datetime = measurement_datetime,
        during = "during_visit")
  })

  expect_equal(joined$symbol, c("acvpu"))
  expect_equal(joined$value_as_number, c(NA_real_))
  expect_equal(joined$value_as_character, c("A"))
})

test_that("mixed numeric and character fsheet data handled", {
  fsheet_raw_test <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_fsheet.csv"),
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

  suppressMessages({
    fsheet_data_test <- fsheet_extract(fsheet_raw_test, fsheet_def)
  })

  demo_adm_raw <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_adm.csv"),
      col_types = "adm"
    ) %>%
    adm_rename %>%
    filter(person_id %in% c("FF"))

  suppressMessages({
    joined <- demo_adm_raw %>%
      all_during(
        fsheet_data_test,
        datetime = measurement_datetime,
        during = "during_visit")
  })

  expect_equal(joined$symbol, c("news2", "acvpu"))
  expect_equal(joined$value_as_number, c(10, NA_real_))
  expect_equal(joined$value_as_character, c(NA_character_, "P"))
})


test_that("first_during_after_event works", {
  adm <- tibble::tribble(
    ~person_id,     ~visit_start_datetime,       ~visit_end_datetime, ~visit_id,
          "AA", dt("2000-01-01 09:00:00"), dt("2021-01-02 09:00:00"),        1L
  )

  fsheet_a <- tibble::tribble(
    ~person_id, ~symbol,     ~measurement_datetime, ~value_as_number,
          "AA",     "a", dt("2000-01-01 08:00:00"),                10,
          "AA",     "a", dt("2000-01-01 09:15:00"),                11, # first
          "AA",     "a", dt("2000-01-01 09:30:00"),                12
  )

  fsheet_b <- tibble::tribble(
    ~person_id, ~symbol,     ~measurement_datetime, ~value_as_number,
          "AA",     "b", dt("2000-01-01 10:59:00"),                1,
          "AA",     "b", dt("2000-01-01 08:59:00"),                2,
          "AA",     "b", dt("2000-01-01 09:05:00"),                3,
          "AA",     "b", dt("2000-01-01 09:25:00"),                4, # this
          "AA",     "b", dt("2000-01-01 09:35:00"),                5,
  )


  expect_equal({
    suppressMessages({
      adm %>%
        first_during(fsheet_a,
                     datetime = measurement_datetime,
                     during = "during_visit") %>%
        first_during_after_event(fsheet_b,
                                 datetime = measurement_datetime,
                                 during = "during_visit",
                                 event_datetime = a_first_during_visit_datetime,
                                 names_suffix = "first_a")
    })
  },
  tibble(
    person_id = c("AA"),
    visit_start_datetime = dt("2000-01-01 09:00:00"),
    visit_end_datetime = dt("2021-01-02 09:00:00"),
    visit_id = c(1L),
    a_first_during_visit_datetime = dt("2000-01-01 09:15:00"),
    a_first_during_visit = c(11),
    b_first_after_first_a_during_visit_datetime = dt("2000-01-01 09:25:00"),
    b_first_after_first_a_during_visit = c(4)
  ))
})



test_that("last_during_before_event works", {
  adm <- tibble::tribble(
    ~person_id,     ~visit_start_datetime,       ~visit_end_datetime, ~visit_id,
          "AA", dt("2000-01-01 09:00:00"), dt("2021-01-02 09:00:00"),        1L
  )

  fsheet_a <- tibble::tribble(
    ~person_id, ~symbol,     ~measurement_datetime, ~value_as_number,
          "AA",     "a", dt("2000-01-01 08:00:00"),                10,
          "AA",     "a", dt("2000-01-01 09:15:00"),                11, # first
          "AA",     "a", dt("2000-01-01 09:30:00"),                12
  )

  fsheet_b <- tibble::tribble(
    ~person_id, ~symbol,     ~measurement_datetime, ~value_as_number,
          "AA",     "b", dt("2000-01-01 10:59:00"),                1,
          "AA",     "b", dt("2000-01-01 08:59:00"),                2,
          "AA",     "b", dt("2000-01-01 09:05:00"),                3, # this
          "AA",     "b", dt("2000-01-01 09:25:00"),                4,
          "AA",     "b", dt("2000-01-01 09:35:00"),                5,
  )

  expect_equal({
    suppressMessages({
      adm %>%
        first_during(fsheet_a,
                     datetime = measurement_datetime,
                     during = "during_visit") %>%
        last_during_before_event(fsheet_b,
                                 datetime = measurement_datetime,
                                 during = "during_visit",
                                 event_datetime = a_first_during_visit_datetime,
                                 names_suffix = "first_a")
    })
  },
  tibble(
    person_id = c("AA"),
    visit_start_datetime = dt("2000-01-01 09:00:00"),
    visit_end_datetime = dt("2021-01-02 09:00:00"),
    visit_id = c(1L),
    a_first_during_visit_datetime = dt("2000-01-01 09:15:00"),
    a_first_during_visit = c(11),
    b_last_before_first_a_during_visit_datetime = dt("2000-01-01 09:05:00"),
    b_last_before_first_a_during_visit = c(3)
  ))
})



test_that("all_during flags if all datetime are midnight if posixct", {
  fsheet_raw_test <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_fsheet.csv"),
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

  suppressMessages({
    fsheet_data_test <- fsheet_extract(fsheet_raw_test, fsheet_def) |>
      mutate(measurement_datetime =
               lubridate::floor_date(measurement_datetime, "day"))
  })

  demo_adm_raw <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_adm.csv"),
      col_types = "adm"
    ) %>%
    adm_rename %>%
    filter(person_id == "AA")

  expect_message({
    demo_adm_raw %>%
      all_during(fsheet_data_test,
                 datetime = measurement_datetime,
                 during = "during_visit")
  },
  "midnight")
})


test_that("all_during does not flag if all datetime are midnight if Date", {
  fsheet_raw_test <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_fsheet.csv"),
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

  suppressMessages({
    fsheet_data_test <- fsheet_extract(fsheet_raw_test, fsheet_def) |>
      mutate(measurement_datetime =
               as.Date(lubridate::floor_date(measurement_datetime, "day")))
  })

  demo_adm_raw <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_adm.csv"),
      col_types = "adm"
    ) %>%
    adm_rename %>%
    filter(person_id == "AA")

  expect_no_message({
    demo_adm_raw %>%
      all_during(fsheet_data_test,
                 datetime = measurement_datetime,
                 during = "during_visit")
  })
})

