test_that("all_during basics", {

  suppressMessages({
    joined <- adm_data_example |>
      all_during(fsheet_news2_example,
                 datetime = measurement_datetime,
                 during = "during_visit") |>
      filter(person_id == "AA", visit_id == 1)
  })

  expect_equal(nrow(joined), 3)
  expect_equal(joined$value_as_number, c(3, 3, 4))
})

test_that("all_during handles shuffled data", {
  adm_shuffled <- adm_data_example |>
    dplyr::sample_frac(1)

  fsheet_shuffled <- fsheet_news2_example |>
    dplyr::sample_frac(1)

  suppressMessages({
    joined <- adm_shuffled |>
      all_during(fsheet_shuffled,
                 datetime = measurement_datetime,
                 during = "during_visit") |>
      filter(person_id == "AA", visit_id == 1)
  })

  expect_equal(nrow(joined), 3)
  expect_equal(joined$value_as_number, c(3, 3, 4))
})

test_that("all_during returns no visit if nothing during_visit", {

  suppressMessages({
    joined <- adm_data_example |>
      all_during(fsheet_news2_example,
                 datetime = measurement_datetime,
                 during = "during_visit") |>
      filter(person_id == "CC", visit_id == 5)
  })

  expect_equal(nrow(joined), 0)
})

test_that("all_during returns visit if nothing during_visit but left joining", {
  suppressMessages({
    joined <- adm_data_example |>
      all_during(fsheet_news2_example,
                 datetime = measurement_datetime,
                 during = "during_visit",
                 join = "left") |>
      filter(person_id == "CC", visit_id == 5)
  })

  expect_equal(nrow(joined), 1)
})

test_that("all_during returns nothing if no overlap", {
  # move visits to 10 years later so no overlap
  adm_data_example_adjusted <-
    adm_data_example |>
    mutate(visit_start_datetime = visit_start_datetime + dyears(10),
           visit_end_datetime = visit_end_datetime + dyears(10))

  suppressMessages({
    joined <- adm_data_example_adjusted |>
      all_during(fsheet_news2_example,
                 datetime = measurement_datetime,
                 during = "during_visit")
  })

  expect_equal(
    nrow(joined),
    0L
  )
})




test_that("first_during works correctly", {
  suppressMessages({
  joined <- adm_data_example %>%
    first_during(fsheet_news2_example,
                 datetime = measurement_datetime,
                 during = "during_visit") |>
    arrange(person_id, visit_id)
  })
  expect_equal(
    joined$news2_first_during_visit,
    c(3, 3, 3, 5, NA)
  )
  expect_identical(
    joined$news2_first_during_visit_datetime,
    c(ymd_hms("2021-01-01 09:00:00", tz = "Europe/London"),
      ymd_hms("2021-01-05 11:30:00", tz = "Europe/London"),
      ymd_hms("2021-01-21 09:30:00", tz = "Europe/London"),
      ymd_hms("2021-01-01 08:59:00", tz = "Europe/London"),
      lubridate::NA_POSIXct_
      )
  )
})

test_that("last_during works correctly", {
  suppressMessages({
    joined <- adm_data_example %>%
      last_during(fsheet_news2_example,
                  datetime = measurement_datetime,
                  during = "during_visit") |>
      arrange(person_id, visit_id)
  })
  expect_equal(
    joined$news2_last_during_visit,
    c(4, 3, 3, 6, NA)
  )
  expect_identical(
    joined$news2_last_during_visit_datetime,
    c(ymd_hms("2021-01-01 12:30:00", tz = "Europe/London"),
      ymd_hms("2021-01-06 10:30:00", tz = "Europe/London"),
      ymd_hms("2021-01-23 09:00:00", tz = "Europe/London"),
      ymd_hms("2021-01-04 09:00:00", tz = "Europe/London"),
      lubridate::NA_POSIXct_
    )
  )
})

test_that("first_during and last_during work correctly together", {
  suppressMessages({
    joined <- adm_data_example %>%
      first_during(fsheet_news2_example,
                   datetime = measurement_datetime,
                   during = "during_visit") %>%
      last_during(fsheet_news2_example,
                  datetime = measurement_datetime,
                  during = "during_visit") |>
      arrange(person_id, visit_id)
  })

  expect_equal(
    joined$news2_first_during_visit,
    c(3, 3, 3, 5, NA)
  )
  expect_identical(
    joined$news2_first_during_visit_datetime,
    c(ymd_hms("2021-01-01 09:00:00", tz = "Europe/London"),
      ymd_hms("2021-01-05 11:30:00", tz = "Europe/London"),
      ymd_hms("2021-01-21 09:30:00", tz = "Europe/London"),
      ymd_hms("2021-01-01 08:59:00", tz = "Europe/London"),
      lubridate::NA_POSIXct_
    )
  )
  expect_equal(
    joined$news2_last_during_visit,
    c(4, 3, 3, 6, NA)
  )
  expect_identical(
    joined$news2_last_during_visit_datetime,
    c(ymd_hms("2021-01-01 12:30:00", tz = "Europe/London"),
      ymd_hms("2021-01-06 10:30:00", tz = "Europe/London"),
      ymd_hms("2021-01-23 09:00:00", tz = "Europe/London"),
      ymd_hms("2021-01-04 09:00:00", tz = "Europe/London"),
      lubridate::NA_POSIXct_
    )
  )
})

test_that("first_during with multiple symbols", {
  suppressMessages({
    joined <- adm_data_example %>%
      first_during(fsheet_example,
                   datetime = measurement_datetime,
                   during = "during_visit") |>
      arrange(person_id, visit_id)
  })

  expect_equal(
    joined$news2_first_during_visit,
    c(3, 3, 3, 5, NA)
  )
  expect_identical(
    joined$news2_first_during_visit_datetime,
    c(ymd_hms("2021-01-01 09:00:00", tz = "Europe/London"),
      ymd_hms("2021-01-05 11:30:00", tz = "Europe/London"),
      ymd_hms("2021-01-21 09:30:00", tz = "Europe/London"),
      ymd_hms("2021-01-01 08:59:00", tz = "Europe/London"),
      lubridate::NA_POSIXct_
    )
  )

  expect_equal(
    joined$spo2_first_during_visit,
    c(96, 96, 93, NA, NA)
  )
  expect_identical(
    joined$spo2_first_during_visit_datetime,
    c(ymd_hms("2021-01-01 10:59:00", tz = "Europe/London"),
      ymd_hms("2021-01-05 11:30:00", tz = "Europe/London"),
      ymd_hms("2021-01-21 09:15:00", tz = "Europe/London"),
      lubridate::NA_POSIXct_,
      lubridate::NA_POSIXct_
    )
  )
})

test_that("nearest_to_event_during", {
  suppressMessages({
    adm_aa <- adm_data_example |>
      filter(person_id == "AA")
    fsheet_news2_aa <- fsheet_news2_example |>
      filter(person_id == "AA")
    fsheet_spo2_aa <- fsheet_spo2_example |>
      filter(person_id == "AA")

    first_news2 <- adm_aa %>%
      first_during(fsheet_news2_aa,
                   datetime = measurement_datetime,
                   during = "during_visit") |>
        select(person_id,
               visit_id,
               visit_start_datetime,
               visit_end_datetime,
               contains("spo2"),
               contains("news2"))
    #'
    # extract the NEAREST (either before or after) SpO2 measurement that to
    # the first NEWS2 record during each hospital visit
    joined <- first_news2 %>%
      nearest_to_event_during(fsheet_spo2_aa,
                              datetime = measurement_datetime,
                              during = "during_visit",
                              event_datetime = news2_first_during_visit_datetime,
                              names_suffix = "first_news2") %>%
      select(person_id,
             visit_id,
             contains("spo2"),
             contains("news2"))
  })

  expect_equal(
    joined$spo2_nearest_to_first_news2_during_visit,
    c(96, 96)
  )
  expect_identical(
    joined$spo2_nearest_to_first_news2_during_visit_datetime,
    c(ymd_hms("2021-01-01 10:59:00", tz = "Europe/London"),
      ymd_hms("2021-01-05 11:30:00", tz = "Europe/London")
    )
  )
})

test_that("nearest_to_event_during during 14_days_before_visit_until_visit_end", {
  suppressMessages({
    adm_aa <- adm_data_example |>
      filter(person_id == "AA")
    fsheet_news2_aa <- fsheet_news2_example |>
      filter(person_id == "AA")
    fsheet_spo2_aa <- fsheet_spo2_example |>
      filter(person_id == "AA")

    first_news2 <- adm_aa %>%
      first_during(fsheet_news2_aa,
                   datetime = measurement_datetime,
                   during = "during_visit") |>
      select(person_id,
             visit_id,
             visit_start_datetime,
             visit_end_datetime,
             contains("spo2"),
             contains("news2"))

    # add measurement before visit_start_datetime that is nearer than
    # first measurement during visit
    just_before_visit_datetime <- ymd_hms("2021-01-01 08:30:00", tz = "Europe/London")
    fsheet_spo2_aa <-
      bind_rows(
        fsheet_spo2_aa,
        tibble(
          person_id = "AA",
          symbol = "spo2",
          measurement_datetime = just_before_visit_datetime,
          value_as_number = 10,
          type = "numeric"
        )
      )

    joined <- first_news2 %>%
      nearest_to_event_during(fsheet_spo2_aa,
                              datetime = measurement_datetime,
                              during = "14_days_before_visit_until_visit_end",
                              event_datetime = news2_first_during_visit_datetime,
                              names_suffix = "first_news2") %>%
      select(person_id,
             visit_id,
             contains("spo2"),
             contains("news2"))
  })

  expect_equal(
    joined$spo2_nearest_to_first_news2_14_days_before_visit_until_visit_end,
    c(10, 96)
  )
  expect_identical(
    joined$spo2_nearest_to_first_news2_14_days_before_visit_until_visit_end_datetime,
    c(just_before_visit_datetime,
      ymd_hms("2021-01-05 11:30:00", tz = "Europe/London")
    )
  )
})

test_that("nearest_visit_start_during()", {
  suppressMessages({
    adm_aa <- adm_data_example |>
      filter(person_id == "AA")

    fsheet_spo2_aa <- fsheet_spo2_example |>
      filter(person_id == "AA")

    # add measurement before visit_start_datetime that is nearer than
    # first measurement during visit
    just_before_visit_datetime <- ymd_hms("2021-01-01 08:30:00", tz = "Europe/London")
    fsheet_spo2_aa <-
      bind_rows(
        fsheet_spo2_aa,
        tibble(
          person_id = "AA",
          symbol = "spo2",
          measurement_datetime = just_before_visit_datetime,
          value_as_number = 10,
          type = "numeric"
        )
      )

    joined <- adm_aa %>%
      nearest_visit_start_during(fsheet_spo2_aa,
                                 datetime = measurement_datetime,
                                 during = "during_visit")
  })

  expect_equal(
    joined$spo2_nearest_visit_start_during_visit,
    c(96, 96)
  )
  expect_identical(
    joined$spo2_nearest_visit_start_during_visit_datetime,
    c(ymd_hms("2021-01-01 10:59:00", tz = "Europe/London"),
      ymd_hms("2021-01-05 11:30:00", tz = "Europe/London")
    )
  )
})

test_that("nearest_visit_start_during before_visit_end", {
  suppressMessages({
    adm_aa <- adm_data_example |>
      filter(person_id == "AA")

    fsheet_spo2_aa <- fsheet_spo2_example |>
      filter(person_id == "AA")

    # add measurement before visit_start_datetime that is nearer than
    # first measurement during visit
    just_before_visit_datetime <- ymd_hms("2021-01-01 08:30:00", tz = "Europe/London")
    fsheet_spo2_aa <-
      bind_rows(
        fsheet_spo2_aa,
        tibble(
          person_id = "AA",
          symbol = "spo2",
          measurement_datetime = just_before_visit_datetime,
          value_as_number = 10,
          type = "numeric"
        )
      )

    joined <- adm_aa %>%
      nearest_visit_start_during(fsheet_spo2_aa,
                                 datetime = measurement_datetime,
                                 during = "before_visit_end")
  })

  expect_equal(
    joined$spo2_nearest_visit_start_before_visit_end,
    c(10, 96)
  )
  expect_identical(
    joined$spo2_nearest_visit_start_before_visit_end_datetime,
    c(just_before_visit_datetime,
      ymd_hms("2021-01-05 11:30:00", tz = "Europe/London")
    )
  )
})



test_that("max_during during_visit", {
  suppressMessages({
    adm_aa <- adm_data_example |>
      filter(person_id == "AA")

    fsheet_spo2_aa <- fsheet_spo2_example |>
      filter(person_id == "AA")

    # add measurement before visit_start_datetime that is nearer than
    # first measurement during visit
    just_before_visit_datetime <- ymd_hms("2021-01-01 08:30:00", tz = "Europe/London")
    fsheet_spo2_aa <-
      bind_rows(
        fsheet_spo2_aa,
        tibble(
          person_id = "AA",
          symbol = "spo2",
          measurement_datetime = just_before_visit_datetime,
          value_as_number = 10,
          type = "numeric"
        )
      )

    joined <- adm_aa %>%
      max_during(fsheet_spo2_aa,
                 datetime = measurement_datetime,
                 during = "during_visit")
  })

  expect_equal(
    joined$spo2_max_during_visit,
    c(96, 96)
  )
  expect_identical(
    joined$spo2_max_during_visit_datetime,
    c(ymd_hms("2021-01-01 10:59:00", tz = "Europe/London"),
      ymd_hms("2021-01-05 11:30:00", tz = "Europe/London")
    )
  )
})


test_that("min_during during_visit", {
  suppressMessages({
    adm_aa <- adm_data_example |>
      filter(person_id == "AA")

    fsheet_spo2_aa <- fsheet_spo2_example |>
      filter(person_id == "AA")

    # add measurement before visit_start_datetime that is nearer than
    # first measurement during visit
    just_before_visit_datetime <- ymd_hms("2021-01-01 08:30:00", tz = "Europe/London")
    fsheet_spo2_aa <-
      bind_rows(
        fsheet_spo2_aa,
        tibble(
          person_id = "AA",
          symbol = "spo2",
          measurement_datetime = just_before_visit_datetime,
          value_as_number = 10,
          type = "numeric"
        )
      )

    joined <- adm_aa %>%
      min_during(fsheet_spo2_aa,
                 datetime = measurement_datetime,
                 during = "during_visit")
  })

  expect_equal(
    joined$spo2_min_during_visit,
    c(90, 90)
  )
  expect_identical(
    joined$spo2_min_during_visit_datetime,
    c(ymd_hms("2021-01-02 08:59:00", tz = "Europe/London"),
      ymd_hms("2021-01-05 16:30:00", tz = "Europe/London")
    )
  )
})


test_that("max_during and min_during during_visit", {
  suppressMessages({
    adm_aa <- adm_data_example |>
      filter(person_id == "AA")

    fsheet_spo2_aa <- fsheet_spo2_example |>
      filter(person_id == "AA")

    # add measurement before visit_start_datetime that is nearer than
    # first measurement during visit
    just_before_visit_datetime <- ymd_hms("2021-01-01 08:30:00", tz = "Europe/London")
    fsheet_spo2_aa <-
      bind_rows(
        fsheet_spo2_aa,
        tibble(
          person_id = "AA",
          symbol = "spo2",
          measurement_datetime = just_before_visit_datetime,
          value_as_number = 10,
          type = "numeric"
        )
      )

    joined <- adm_aa %>%
      min_during(fsheet_spo2_aa,
                 datetime = measurement_datetime,
                 during = "during_visit") %>%
      max_during(fsheet_spo2_aa,
                 datetime = measurement_datetime,
                 during = "during_visit")
  })

  expect_equal(
    joined$spo2_min_during_visit,
    c(90, 90)
  )
  expect_identical(
    joined$spo2_min_during_visit_datetime,
    c(ymd_hms("2021-01-02 08:59:00", tz = "Europe/London"),
      ymd_hms("2021-01-05 16:30:00", tz = "Europe/London")
    )
  )

  expect_equal(
    joined$spo2_max_during_visit,
    c(96, 96)
  )
  expect_identical(
    joined$spo2_max_during_visit_datetime,
    c(ymd_hms("2021-01-01 10:59:00", tz = "Europe/London"),
      ymd_hms("2021-01-05 11:30:00", tz = "Europe/London")
    )
  )
})
