test_that("duckdb load adm", {
  con <- duckdb_tidybrookes_connect()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  duckdb_read_tidybrookes_csv(
    con,
    table = "adm",
    file = tidybrookes_example("test_adm.csv"))

  demo_adm_raw <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_adm.csv"),
      col_types = "adm"
    )

  duckdb_demo_adm_raw <- dplyr::tbl(con, "adm_raw_unrenamed") |>
    select(-adm_id) |>
    collect()

  expect_identical(duckdb_demo_adm_raw, demo_adm_raw)
})

test_that("duckdb load tests", {
  con <- duckdb_tidybrookes_connect()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  duckdb_read_tidybrookes_csv(
    con,
    table = "tests",
    file = tidybrookes_example("test_tests.csv"))

  demo_tests_raw <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_tests.csv"),
      col_types = "tests"
    )

  duckdb_demo_tests_raw <- dplyr::tbl(con, "tests_raw_unrenamed") |>
    select(-tests_id) |>
    collect()

  expect_identical(duckdb_demo_tests_raw, demo_tests_raw)
})

test_that("duckdb load fsheet", {
  con <- duckdb_tidybrookes_connect()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  duckdb_read_tidybrookes_csv(
    con,
    table = "fsheet",
    file = tidybrookes_example("test_fsheet.csv"))

  demo_fsheet_raw <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_fsheet.csv"),
      col_types = "fsheet"
    )

  duckdb_demo_fsheet_raw <- dplyr::tbl(con, "fsheet_raw_unrenamed") |>
    select(-fsheet_id) |>
    collect()

  expect_equal(duckdb_demo_fsheet_raw, demo_fsheet_raw)
})

test_that("duckdb load fsheet treat space as NA", {
  con <- duckdb_tidybrookes_connect()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  duckdb_read_tidybrookes_csv(
    con,
    table = "fsheet",
    file = tidybrookes_example("test_fsheet.csv"),
    na = c("", " ", "NA")
  )

  demo_fsheet_raw <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_fsheet.csv"),
      col_types = "fsheet",
      na = c("", " ", "NA")
    )

  duckdb_demo_fsheet_raw <- dplyr::tbl(con, "fsheet_raw_unrenamed") |>
    select(-fsheet_id) |>
    collect()

  expect_equal(duckdb_demo_fsheet_raw, demo_fsheet_raw)
})


test_that("duckdb load fsheet", {
  con <- duckdb_tidybrookes_connect()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  duckdb_read_tidybrookes_csv(
    con,
    table = "fsheet",
    file = tidybrookes_example("fsheet.csv"))

  demo_fsheet_raw <-
    read_tidybrookes_csv(
      file = tidybrookes_example("fsheet.csv"),
      col_types = "fsheet"
    )

  duckdb_demo_fsheet_raw <- dplyr::tbl(con, "fsheet_raw_unrenamed") |>
    select(-fsheet_id) |>
    collect()

  expect_equal(duckdb_demo_fsheet_raw, demo_fsheet_raw)
})


test_that("duckdb adm rename", {
  con <- duckdb_tidybrookes_connect()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  duckdb_read_tidybrookes_csv(
    con,
    table = "adm",
    file = tidybrookes_example("test_adm.csv"))

  demo_adm_raw <-
    read_tidybrookes_csv(
      file = tidybrookes_example("test_adm.csv"),
      col_types = "adm"
    )

  duckdb_demo_adm_raw <- dplyr::tbl(con, "adm_raw_unrenamed") |>
    select(-adm_id) |>
    collect()

  expect_identical(duckdb_demo_adm_raw, demo_adm_raw)
})
