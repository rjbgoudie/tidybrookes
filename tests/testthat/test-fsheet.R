
test_that("Ignores irrelevant rows", {
  fsheet_raw_test <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_fsheet.csv"),
      col_types = "fsheet") %>%
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

  expect_equal(fsheet_data_test$unit, c("kg", "kg", "kg"))
  expect_equal(
    fsheet_data_test$value_as_number,
    c(4025 * 28.35 / 1000,
      4125 * 28.35 / 1000,
      4035 * 28.35 / 1000
      ))
})

test_that("discard values below", {
  fsheet_raw_test <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_fsheet.csv"),
      col_types = "fsheet") %>%
    fsheet_rename %>%
    filter(person_id == "BB")

  fsheet_def <- list() %>%
    fsheet_add(
      symbol = "spo2",
      title = "Oxygen saturations",
      names = c("SpO2"),
      search_pattern = c("spo2", "sats"),
      search_exclude = c(),
      range_discard_below = 50)

  suppressMessages({
    fsheet_data_test <- fsheet_extract(fsheet_raw_test, fsheet_def)
  })

  expect_identical(fsheet_data_test$unit, NA_character_)
  expect_identical(fsheet_data_test$censoring, NA_character_)
  expect_identical(fsheet_data_test$symbol, "spo2")
  expect_equal(fsheet_data_test$value_as_number, 93)
})

test_that("missing measurement date", {
  fsheet_raw_test <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_fsheet.csv"),
      col_types = "fsheet") %>%
    fsheet_rename %>%
    filter(person_id == "DD")

  fsheet_def <- list() %>%
    fsheet_add(
      symbol = "spo2",
      title = "Oxygen saturations",
      names = c("SpO2"),
      search_pattern = c("spo2", "sats"),
      search_exclude = c(),
      range_discard_below = 50)

  suppressMessages({
    fsheet_data_test <- fsheet_extract(fsheet_raw_test, fsheet_def)
  })

  expect_identical(fsheet_data_test$value_as_character, NA_character_)
  expect_identical(fsheet_data_test$value_as_number, 93)
  expect_identical(fsheet_data_test$symbol, "spo2")
  expect_equal(fsheet_data_test$measurement_datetime, ymd_hms(NA, tz = "Europe/London"))
})
