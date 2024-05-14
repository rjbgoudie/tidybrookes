test_that("Ignores irrelevant row", {
  tests_raw_demo <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_tests.csv"),
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
      censoring_fn = case_when(value_original == "<0.29" ~ "left", # very low
                               TRUE ~ NA_character_),
      value_as_number_fn = case_when(value_original == "<0.29" ~ 0.29,
                                     TRUE ~ value_as_number),
      expect_before = (unit == "mmol/L"),
      range_mainly_low = 0.4,
      range_mainly_high = 1.2,
      range_discard_below = 0.32,
      range_discard_above = 1.8)

  suppressMessages({
    tests_data_demo <- tests_extract(tests_raw_demo, tests_def)
  })

  expect_identical(tests_data_demo$unit, "mmol/L")
  expect_equal(tests_data_demo$value_as_number, 1.0)
})

test_that("Tests handle character values", {
  tests_raw_demo <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_tests.csv"),
      col_types = "tests") %>%
    tests_rename %>%
    filter(person_id == "BB")

  tests_def <- list() %>%
    tests_add(
      symbol = "blood_specimen_type_bg",
      title = "Blood specimen type",
      names_cuh = "POC BLOOD SPECIMEN TYPE",
      names_external = NA,
      search_pattern = c("Specimen type"),
      search_exclude = c(),
      type = "character",
      silently_exclude_na_when = FALSE,
      value_as_character_fn = case_when(
        value_original == "Arterial blood" ~ "Arterial blood",
        value_original == "Arterial Blood" ~ "Arterial blood",
        value_original == "Blood" ~ "Blood",
        value_original == "Capillary blood" ~ "Capillary blood",
        value_original == "Capillary Blood" ~ "Capillary blood",
        value_original == "Mixed blood" ~ "Mixed blood",
        value_original == "Venous blood" ~ "Venous blood",
        value_original == "Venous Blood" ~ "Venous blood"),
      expect_before =
        (value_original %in% c("Arterial blood", "Venous blood", "Arterial Blood",
                               "Blood", "Mixed blood", "Capillary blood",
                               "Venous Blood", "Capillary Blood")),
      expect_after =
        (value_as_character %in% c("Arterial blood", "Venous blood", "Blood",
                                   "Mixed blood", "Capillary blood")))

  suppressMessages({
    tests_data_demo <- tests_extract(tests_raw_demo, tests_def)
  })

  expect_identical(tests_data_demo$value_as_character, "Arterial blood")
})

test_that("Tests handle unexpected character values", {
  # CC has unexpected value in BLOOD SPECIMEN TYPE
  tests_raw_demo <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_tests.csv"),
      col_types = "tests") %>%
    tests_rename %>%
    filter(person_id %in% c("BB", "CC"))

    tests_def <- list() %>%
      tests_add(
        symbol = "blood_specimen_type_bg",
        title = "Blood specimen type",
        names_cuh = "POC BLOOD SPECIMEN TYPE",
        names_external = NA,
        search_pattern = c("Specimen type"),
        search_exclude = c(),
        type = "character",
        silently_exclude_na_when = FALSE,
        value_as_character_fn = case_when(
          value_original == "Arterial blood" ~ "Arterial blood",
          value_original == "Arterial Blood" ~ "Arterial blood",
          value_original == "Blood" ~ "Blood",
          value_original == "Capillary blood" ~ "Capillary blood",
          value_original == "Capillary Blood" ~ "Capillary blood",
          value_original == "Mixed blood" ~ "Mixed blood",
          value_original == "Venous blood" ~ "Venous blood",
          value_original == "Venous Blood" ~ "Venous blood"),
        expect_before =
          (value_original %in% c("Arterial blood", "Venous blood", "Arterial Blood",
                                 "Blood", "Mixed blood", "Capillary blood",
                                 "Venous Blood", "Capillary Blood")),
        expect_after =
          (value_as_character %in% c("Arterial blood", "Venous blood", "Blood",
                                     "Mixed blood", "Capillary blood")))

    # supress warnings, since this gives warning on both expect_before and
    # expect_after, but currently not sure how to test this
    # suppressWarnings({
    #   expect_warning(tests_extract(tests_raw_demo, tests_def),
    #                  regexp = "expect_after|expect_before")
    # })

})
