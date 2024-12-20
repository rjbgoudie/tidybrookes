#' Load data from Epic in delimited (including CSV) format
#'
#' Convenience wrapper around [readr::read_csv] and [readr::read_csv] that makes
#' it easier to set the `col_types` for standard data from Epic. Also makes
#' handling untimezoned data more robust through use of functions from
#' [`clock::date_time_parse`].
#'
#' Named `col_types` are extracted using [`default_col_types()`].
#'
#' At transitions to and from daylight savings times, there are nonexistent and
#' ambiguous times. Using the parameters `nonexistent` and `ambiguous` the
#' handling of these can be made explicit and consistent.
#'
#' @param file Either a path to a file, a connection, or literal data (either a
#'   single string or a raw vector), as per [readr::read_delim()]
#' @param col_types Either a string specifying the name of the table that the
#'   data comes from, for which a standard format can be used. Available values
#'   are `adm`, `adt`, `demogs`, `fsheet`, `fsheet_io`, `tests`, `med_admin`,
#'   `med_prescr`, `diagnosis_pl`, `med_hist`, `radiology`. This is translated
#'   into column specification using [`default_col_types`].
#'
#'   Or a [readr::cols()] format list of column names and types, which can be
#'   used where a nonstandard data format are supplied.
#' @param delim Single character used to separate fields within a record.
#' @param n_max Maximum number of lines to read.
#' @param na Character vector of strings to interpret as missing values. Set
#'   this option to `character()` to indicate no missing values.
#' @param quote Single character used to quote strings
#' @param tz A timezone name, such as `"Europe/London"`
#' @param nonexistent One of the following nonexistent time resolution
#'   strategies, allowed to be either length 1, or the same length as the input:
#'
#'   - `"roll-forward"`: The next valid instant in time.
#'
#'   - `"roll-backward"`: The previous valid instant in time.
#'
#'   - `"shift-forward"`: Shift the nonexistent time forward by the size of
#'   the daylight saving time gap.
#'
#'   - `"shift-backward`: Shift the nonexistent time backward by the size of
#'   the daylight saving time gap.
#'
#'   - `"NA"`: Replace nonexistent times with `NA`.
#'
#'   - `"error"`: Error on nonexistent times.
#'
#'   Using either `"roll-forward"` or `"roll-backward"` is generally recommended
#'   over shifting, as these two strategies maintain the _relative ordering_
#'   between elements of the input.
#'
#'   If `NULL`, defaults to `"error"`.
#' @param ambiguous Passed to [clock::date-time-parse], with the usual values
#'   being one of the following ambiguous time resolution strategies, allowed to
#'   be either length 1, or the same length as the input:
#'
#'   - `"earliest"`: Of the two possible times, choose the earliest one.
#'
#'   - `"latest"`: Of the two possible times, choose the latest one.
#'
#'   - `"NA"`: Replace ambiguous times with `NA`.
#'
#'   - `"error"`: Error on ambiguous times.
#'
#' @seealso Named column types are extracted using [default_col_types()]. The
#'   raw column names, which are messy, can be cleaned using e.g.
#'   [fsheet_rename()]
#' @importFrom readr read_csv cols col_character col_integer col_double locale
#'   col_datetime
#' @rdname read
#' @export
#'
#' @examples
#' adm_file_path <- tidybrookes_example("adm.csv")
#'
#' # the format when col_types = "adm"
#' default_col_types("adm")
#'
#' read_tidybrookes_csv(
#'   file = adm_file_path,
#'   col_types = "adm"
#' )
#'
#' read_tidybrookes_csv(
#'   file = adm_file_path,
#'   col_types = "adm",
#'   tz = "Europe/London",
#'   nonexistent = "roll-forward",
#'   ambiguous = "latest"
#' )
#'
#' # view default cols() format - this is what will be used if a character
#' # string name of a table is used, such as "fsheet"
#' default_col_types("fsheet")
#'
#' # custom col_types can be specified
#' read_tidybrookes_csv(
#'   file = adm_file_path,
#'   col_types = cols(
#'     STUDY_SUBJECT_DIGEST = col_character(),
#'     fsd_id = col_character())
#' )
#'
#' # Can use the clock package to determine handling of ambiguous and
#' # nonexistent dates (due to transitions to and from daylight saving time)
#' read_tidybrookes_csv(
#'   file = adm_file_path,
#'   col_types = cols(
#'     STUDY_SUBJECT_DIGEST = col_character(),
#'     fsd_id = col_character()),
#'   tz = "Europe/London",
#'   nonexistent = "roll-forward",
#'   ambiguous = "latest"
#' )
read_tidybrookes_csv <- function(file,
                                 col_types,
                                 n_max = Inf,
                                 na = c("", "NA"),
                                 quote = "\"",
                                 tz = "Europe/London",
                                 nonexistent = NULL,
                                 ambiguous = NULL){
  col_types <- default_col_types(col_types = col_types)
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

#' @importFrom readr read_delim
#' @export
#' @rdname read
read_tidybrookes_delim <- function(file,
                                   col_types,
                                   delim = "|",
                                   n_max = Inf,
                                   na = c("", "NA"),
                                   quote = "\""){
  col_types <- default_col_types(col_types = col_types)
  read_delim(file = file,
             delim = delim,
             col_types = col_types,
             locale = locale(tz = "Europe/London"),
             n_max = n_max,
             quote = quote,
             na = na) %>%
    as_tibble
}

#' Load data from Epic in delimited format (including CSV) into a database
#'
#' Reads in data in a chunked manner, and writes it to a database using
#' [DBI::dbWriteTable]
#'
#' @inheritParams read_tidybrookes_csv
#' @param connection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param table_name A table name, passed to [DBI::dbWriteTable()]
#' @importFrom readr read_csv_chunked
#' @importFrom DBI dbWriteTable
#' @export
#' @rdname db_write
db_write_tidybrookes_csv <- function(file,
                                     connection,
                                     table_name,
                                     col_types,
                                     #n_max = Inf,
                                     na = c("", "NA"),
                                     quote = "\"",
                                     progress = TRUE){
  col_types <- default_col_types(col_types = col_types)
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

#' @importFrom readr read_delim_chunked
#' @rdname db_write
#' @export
db_write_tidybrookes_delim <- function(file,
                                       connection,
                                       table_name,
                                       col_types,
                                       #n_max = Inf,
                                       na = c("", "NA"),
                                       quote = "\"",
                                       ...){
  col_types <- default_col_types(col_types = col_types)
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
