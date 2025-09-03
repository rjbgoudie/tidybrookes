
test_that("fsheet rename basics", {
  fsheet_colnames <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_fsheet.csv"),
      col_types = "fsheet") |>
    fsheet_rename() |>
    colnames()


  expect_identical(
    fsheet_colnames,
    c("person_id",
      "name",
      "value",
      "comment",
      "measurement_datetime",
      "data_id",
      "measurement_id",
      "line_id",
      "template",
      "form"))
})


test_that("fsheet rename basics", {
  fsheet_colnames <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_fsheet.csv"),
      col_types = "fsheet") |>
    fsheet_rename(names = c(override_person_id = "STUDY_SUBJECT_DIGEST",
                            default_rename("fsheet"))) |>
    colnames()

  expect_identical(
    fsheet_colnames,
    c("override_person_id",
      "name",
      "value",
      "comment",
      "measurement_datetime",
      "data_id",
      "measurement_id",
      "line_id",
      "template",
      "form"))
})
