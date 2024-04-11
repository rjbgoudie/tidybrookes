#' Load data from Epic in CSV format
#'
#' @param file Either a path to a file, a connection, or literal data (either
#'   a single string or a raw vector), as per [readr::read_delim()]
#' @param col_types Either a string specifying the name of the table that the
#'   data comes from, for which a standard format can be used. Available
#'   value are `adm`, `adt`, `demogs`, `fsheet`, `fsheet_io`, `tests`,
#'   `med_admin`, `med_prescr`, `diagnosis_pl`, `med_hist`, `radiology`.
#'
#'   Or a [readr::cols()] format list of column names and types, which
#'   can be used where a nonstandard data format are supplied.
#' @param n_max Maximum number of lines to read.
#' @param na Character vector of strings to interpret as missing values.
#'   Set this option to `character()` to indicate no missing values.
#' @importFrom readr read_csv cols col_character col_integer col_double locale col_datetime
#' @rdname read
read_tidybrookes_csv <- function(file,
                                 col_types,
                                 n_max = Inf,
                                 na = c("", "NA"),
                                 quote = "\"",
                                 tz = "Europe/London",
                                 nonexistent = NULL,
                                 ambiguous = NULL){
  col_types <- extract_col_types(col_types = col_types)
  col_types_original <- col_types
  col_types <- col_types_rewrite_if_clock(col_types,
                                          nonexistent = nonexistent,
                                          ambiguous = ambiguous)

  out <- read_csv(file = file,
                  col_types = col_types,
                  locale = locale(tz = tz),
                  n_max = n_max,
                  quote = quote,
                  na = na)
  out <- parse_datetime_cols_if_clock(out,
                                      col_types_original,
                                      tz = tz,
                                      nonexistent = nonexistent,
                                      ambiguous = ambiguous)

  out
}

#' Load data from Epic in delimited format
#'
#' @param file Either a path to a file, a connection, or literal data (either
#'   a single string or a raw vector), as per [readr::read_delim()].
#' @param col_types Either a string specifying the name of the table that the
#'   data comes from, for which a standard format can be used. Available
#'   value are `adm`, `adt`, `demogs`, `fsheet`, `fsheet_io`, `tests`,
#'   `med_admin`, `med_prescr`, `diagnosis_pl`, `med_hist`, `radiology`.
#'
#'   Or a [readr::cols()] format list of column names and types, which
#'   can be used where a nonstandard data format are supplied.
#' @param delim Single character used to separate fields within a record.
#' @param n_max Maximum number of lines to read.
#' @param na Character vector of strings to interpret as missing values.
#'   Set this option to `character()` to indicate no missing values.
#' @importFrom readr read_delim cols col_character col_integer col_double locale col_datetime
#' @rdname read
read_tidybrookes_delim <- function(file,
                                   col_types,
                                   delim = "|",
                                   n_max = Inf,
                                   na = c("", "NA"),
                                   quote = "\""){
  col_types <- extract_col_types(col_types = col_types)
  read_delim(file = file,
             delim = delim,
             col_types = col_types,
             locale = locale(tz = "Europe/London"),
             n_max = n_max,
             quote = quote,
             na = na) %>%
    as_tibble
}

#' Load data from Epic in CSV format
#'
#' @param file Either a path to a file, a connection, or literal data (either
#'   a single string or a raw vector), as per [readr::read_delim()]
#' @param col_types Either a string specifying the name of the table that the
#'   data comes from, for which a standard format can be used. Available
#'   value are `adm`, `adt`, `demogs`, `fsheet`, `fsheet_io`, `tests`,
#'   `med_admin`, `med_prescr`, `diagnosis_pl`, `med_hist`, `radiology`.
#'
#'   Or a [readr::cols()] format list of column names and types, which
#'   can be used where a nonstandard data format are supplied.
#' @param n_max Maximum number of lines to read.
#' @param na Character vector of strings to interpret as missing values.
#'   Set this option to `character()` to indicate no missing values.
#' @importFrom readr read_csv_chunked cols col_character col_integer col_double locale col_datetime
#' @importFrom DBI dbWriteTable
#' @rdname db_write
db_write_tidybrookes_csv <- function(file,
                                     connection,
                                     table_name,
                                     col_types,
                                     #n_max = Inf,
                                     na = c("", "NA"),
                                     quote = "\"",
                                     progress = TRUE){
  col_types <- extract_col_types(col_types = col_types)
  cat("Loading..\n")
  read_csv_chunked(
    file = file,
    callback = function(chunk, index) {
      dbWriteTable(connection,
                   chunk,
                   name = table_name,
                   append = TRUE)
    },
    col_types = col_types,
    locale = locale(tz = "Europe/London"),
    quote = quote,
    na = na,
    progress = progress)
}

#' Load data from Epic in delim format
#'
#' @param file Either a path to a file, a connection, or literal data (either
#'   a single string or a raw vector), as per [readr::read_delim()]
#' @param col_types Either a string specifying the name of the table that the
#'   data comes from, for which a standard format can be used. Available
#'   value are `adm`, `adt`, `demogs`, `fsheet`, `fsheet_io`, `tests`,
#'   `med_admin`, `med_prescr`, `diagnosis_pl`, `med_hist`, `radiology`.
#'
#'   Or a [readr::cols()] format list of column names and types, which
#'   can be used where a nonstandard data format are supplied.
#' @param n_max Maximum number of lines to read.
#' @param na Character vector of strings to interpret as missing values.
#'   Set this option to `character()` to indicate no missing values.
#' @importFrom readr read_csv_chunked cols col_character col_integer col_double locale col_datetime
#' @importFrom DBI dbWriteTable
#' @rdname db_write
db_write_tidybrookes_delim <- function(file,
                                       connection,
                                       table_name,
                                       col_types,
                                       #n_max = Inf,
                                       na = c("", "NA"),
                                       quote = "\"",
                                       ...){
  col_types <- extract_col_types(col_types = col_types)
  read_delim_chunked(
    file = file,
    callback = function(chunk, index) {
      dbWriteTable(connection,
                   chunk,
                   name = table_name,
                   append = TRUE)
    },
    col_types = col_types,
    locale = locale(tz = "Europe/London"),
    quote = quote,
    na = na,
    ...)
}
