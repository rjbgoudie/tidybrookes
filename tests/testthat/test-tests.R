
test_that("Ignores irrelevant row", {
  tests_raw_demo <-
    read_tidybrookes_csv(
      file = tidybrookes_example("tests.csv"),
      col_types = "tests") %>%
    tests_rename

  tests_def <- list() %>%
    tests_add(
    symbol = "magnesium",
    title = "Magnesium",
    names_cuh = c("MAGNESIUM"),
    names_external = "EXT MAGNESIUM",
    search_pattern = c("magnesium", "mg"),
    search_exclude = c("ETOH MG/100ML",
                       "RETIRED-ETOH MG/100ML",
                       "RIST 0.2MG/ML",
                       "RIST 0.77MG/ML",
                       "RISTOCETIN 0.5MG/ML",
                       "RISTOCETIN 1.5MG/ML"),
    silently_exclude_na_when = FALSE,
    censoring_fn = case_when(value == "<0.29" ~ "left", # very low
                             TRUE ~ NA_character_),
    value_fn = case_when(value == "<0.29" ~ 0.29,
                         TRUE ~ value_numeric),
    expect_before = (unit == "mmol/L"),
    range_mainly_low = 0.4,
    range_mainly_high = 1.2,
    range_discard_below = 0.32,
    range_discard_above = 1.8)

  tests_data_demo <- tests_extract(tests_raw_demo, tests_def)

  expect_identical(tests_data_demo$unit, "mmol/L")
  expect_equal(tests_data_demo$value, 1.0)
})
