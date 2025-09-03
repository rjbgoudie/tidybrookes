#' Connect to duckdb database with suitable defaults
#'
#' This is a wrapper around [DBI::dbConnect()] that sets suitable defaults
#' for use with `tidybrookes` datasets:
#' * uses [bit64] `integer64` representation for big integers, since some
#'   IDs from EHRs can be larger than can be represented by standard integers
#'   and representing as floats/doubles is incorrect
#' * Since duckdb support for timezone is not great in R currently, we store
#'   datetimes in UTC in duckdb and force conversion to local time at export
#'   from duckdb.
#'
#' Facility to specify the `temp_directory` for duckdb is also provided
#'
#' @param db_dir Location for database files. Should be a path to an existing
#'   directory in the file system. With the default (or `""`), all data is
#'   kept in RAM.
#' @param timezone_out The time zone returned to R, defaults to `"UTC"`, which
#'   is currently the only timezone supported by duckdb.
#'   If you want to display datetime values in the local timezone,
#'   set to [Sys.timezone()] or `""`.
#' @param tz_out_convert How to convert timestamp columns to the timezone
#'   specified in `timezone_out`. There are two options: `"with"`, and
#'   `"force"`. If `"with"` is chosen, the timestamp will be returned as it
#'   would appear in the specified time zone.
#'   If `"force"` is chosen, the timestamp will have the same clock
#'   time as the timestamp in the database, but with the new time
#' @param temp_dir Set the directory to which duckdb should write temp files.
#'   For data stored on slow network drives, it may be much faster to set
#'   this to a faster local drive.
#' @export
#' @author R.J.B. Goudie
#' @examples
#' \dontrun{
#' con <- duckdb_tidybrookes_connect()
#' }
duckdb_tidybrookes_connect <- function(db_dir = "",
                                       bigint = "integer64",
                                       timezone_out = "Europe/London",
                                       tz_out_convert = "force",
                                       temp_dir = NULL){
  config <- list()
  if (!is.null(temp_dir)){
    config <- list(temp_directory = temp_dir)
  }

  con <- DBI::dbConnect(
    duckdb::duckdb(bigint = bigint),
    dbdir = db_dir,

    # We are storing datetimes as TIMESTAMP in duckdb, which is untimezoned
    # but we assume datetimes provided are in local clock time (Europe/London
    # timezone) so we need to force convert them
    timezone_out = timezone_out,
    tz_out_convert = tz_out_convert,

    config = config
  )
  con
}

#' Load data from CUH in delimited (including CSV) format into duckdb
#'
#' Convenience wrapper that makes it easier to set the `col_types` for
#' standard data from CUH for loading into `duckdb`.
#'
#' @param src A `DBIConnection` object produced by [duckdb_tidybrookes_connect]
#'   or `DBI::dbConnect()`
#' @param table A string specifying the name of the table that the
#'   data comes from, for which a standard format can be used. Available values
#'   are `adm`, `adt`, `demogs`, `fsheet`, `fsheet_io`, `tests`, `med_admin`,
#'   `med_prescr`, `diagnosis_pl`, `med_hist`, `radiology`. This is translated
#'   into column specification using [`default_col_types`].
#' @param file A path to the CSV file
#' @param table_name The name to use for the table in `duckdb`
#' @param strict_mode Should `duckdb`'s strict mode for loading CSVs be used?
#' @param append Should new data be appended to the end of an existing table in
#'   the database?
#' @inheritParams read_tidybrookes_csv
#' @rdname duckdb_read
#' @author R.J.B. Goudie
#' @export
duckdb_read_tidybrookes_csv <- function(src,
                                        table,
                                        file,
                                        col_types = table,
                                        na = c("", "NA"),
                                        quote = "\"",
                                        table_name = paste0(table, "_raw_unrenamed"),
                                        strict_mode = TRUE,
                                        append = FALSE){
  col_types <- default_col_types(col_types = col_types)
  col_types <- col_types_rewrite_as_database(col_types)

  col_types_df <- tibble::enframe(col_types,
                                  name = "column",
                                  value = "specified")

  present <- utils::read.csv(file, nrows = 1, check.names = FALSE) |>
    colnames()
  cols_df <- tibble(column = present) |>
    mutate(present = TRUE) |>
    full_join(col_types_df, by = "column")

  unspecified <- cols_df |>
    filter(present & is.na(specified)) |>
    pull(column)

  if (length(unspecified) > 0){
    cli_abort(
      c("The types of the following columns are unspecified: {unspecified}")
    )
  }

  col_types_present <- cols_df |>
    filter(present) |>
    select(column, specified)

  col_types_present <- as_duckdb_list(tibble::deframe(col_types_present))

  params <- as_duckdb_params(
    c(types = col_types_present,
      nullstr = as_duckdb_list(na),
      strict_mode = if_else(strict_mode, "true", "false"))
  )

  if (append){
    DBI::dbExecute(src, glue("
      INSERT INTO {table_name} BY NAME
      SELECT * FROM read_csv('{file}', {params});
    "))
  } else {
    DBI::dbExecute(src, glue("
      CREATE TABLE {table_name} AS
      SELECT * FROM read_csv('{file}', {params});
    "))

    DBI::dbExecute(src, glue("
    CREATE SEQUENCE {table_name}_id START 1;
    ALTER TABLE {table_name} ADD COLUMN {table}_id INTEGER DEFAULT nextval('{table_name}_id');
  "))
  }
}

#' Pointer to raw unrenamed table in duckdb
#'
#' @param src A `DBIConnection` object produced by [duckdb_tidybrookes_connect]
#'   or `DBI::dbConnect()`
#' @param table A string specifying the name of the table. Typical values
#'   would be `adm`, `adt`, `demogs`, `fsheet`, `fsheet_io`, `tests`, `med_admin`,
#'   `med_prescr`, `diagnosis_pl`, `med_hist`, `radiology`
#' @export
tbl_raw_unrenamed <- function(src, table){
  dplyr::tbl(src, glue("{table}_raw_unrenamed"))
}

#' Renamed view of raw data to tidy column names
#'
#' Create view of a table with standardised, shorter column names (e.g. all
#' lowercase, underscore-separated, with `_datetime` suffix etc) rather than
#' the default names in the standard data format from Clinical Informatics.
#'
#' The default renaming map can be viewed using `default_rename()`.
#'
#' If variations from this format occur, custom renaming can be performed
#' using the `names` argument.
#'
#' @param src A `DBIConnection` object produced by [duckdb_tidybrookes_connect]
#'   or `DBI::dbConnect()`
#' @param table A string specifying the name of the table. Typical values
#'   would be `adm`, `adt`, `demogs`, `fsheet`, `fsheet_io`, `tests`, `med_admin`,
#'   `med_prescr`, `diagnosis_pl`, `med_hist`, `radiology`
#' @param names A vector of new_name = old_name pairs
#' @author R.J.B. Goudie
#' @export
tbl_raw <- function(src, table, names = c()){
  names <- c(names, default_rename(table))
  name <- glue("{table}_raw")

  tbl_raw_unrenamed(src, table) |>
    relocate_ignoring_missing(names) |>
    as_view(name)
}

#' Convert an R list to a duckdb list
#' @noRd
as_duckdb_list <- function(x){
  n <- names(x)
  if (!is.null(n)){
    l <- glue_collapse(glue("'{n}' : '{x}'"), sep = ",")
    paste0("{", l, "}")
  } else {
    l <- glue_collapse(glue("'{x}'"), sep = ",")
    paste0("[", l, "]")
  }
}

#' Convert an R list to a duckdb parameter list
#' @noRd
as_duckdb_params <- function(x){
  n <- names(x)
  glue_collapse(glue("{n} = {x}"), sep = ",")
}

db_create_view <- function(db_connection, tbl_name, view_name) {
  sql_query <- glue(
    "CREATE OR REPLACE VIEW {view_name} AS\n",
    "{db_sql_render(db_connection, tbl_name)}\n"
  )
  DBI::dbExecute(db_connection, as.character(sql_query))
}

db_from_tbl <- function(tbl){
  tbl$src$con
}

as_view <- function(x, view_name) {
  db_connection <- db_from_tbl(x)
  sql_query <- glue(
    "CREATE OR REPLACE VIEW {view_name} AS\n",
    "{dbplyr::db_sql_render(db_connection, x)}\n"
  )
  DBI::dbExecute(db_connection, as.character(sql_query))
  dplyr::tbl(db_connection, view_name)
}
