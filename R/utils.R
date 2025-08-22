#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Get path to tidybrookes example
#'
#' tidybrookes comes bundled with a number of sample files in its `inst/extdata`
#' directory. This function make them easy to access
#'
#' @param file Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' tidybrookes_example()
#' tidybrookes_example("fsheet.csv")
tidybrookes_example <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "tidybrookes"))
  } else {
    system.file("extdata", file, package = "tidybrookes", mustWork = TRUE)
  }
}

#' Helper function to extract nonnumeric value_as_numner values
#'
#' This tests whether everything that remains (after exclusions and conversion)
#' is numeric.
#'
#' @param x A test data frame, with `value_as_number` column, after fixing
#'   up various odd values etc
#'
#' @return The rows of the supplied data frame that are `NA` after conversion to
#'   to numeric form
#' @author R.J.B. Goudie
#' @noRd
nonnumeric <- function(x){
  value_as_number <- x$value_as_number
  value_as_number_numeric <- suppressWarnings({
    as.numeric(value_as_number)
  })
  value_is_nonnumeric <- is.na(value_as_number_numeric)
  x[value_is_nonnumeric, ]
}

#' Convert a vector into a character vector, suitably formatted for use
#' in a R argument list - ie each element on a new line.
#' @param x A vector
#' @return A character string of `x`
#' @noRd
format_as_argument <- function(x){
  # x %>%
  #   dput() %>%
  #   capture.output() %>%
  #   str_flatten() %>%
  #   str_replace_all("\", \"", "\",\n    \"") %>%
  #   str_replace(fixed("c(\""), "\n    c(\"")
  f <- file()
  dput(x, f)
  out <- paste(readLines(f), collapse = " ")
  close.connection(f)
  out
}

#' Alert user if all datetime are midnight
#'
#' Does nothing if a `Date` is supplied
#'
#' @param x A `POSIXct` datetime
#' @noRd
inform_if_all_times_are_midnight <- function(x, datetime){
  is_posixct <- x |>
    head(1) |>
    pull({{ datetime }}) |>
    inherits("POSIXct")

  if (is_posixct){
    is_midnight_distinct <- x |>
      mutate(
        is_midnight =
          {{ datetime }} == lubridate::floor_date({{ datetime }},
                                                  unit = "day")
      ) |>
      ungroup() |>
      distinct(is_midnight) |>
      pull(is_midnight)

    if (isTRUE(is_midnight_distinct)){
      cli::cli_alert_danger(
        c("Supplied datetime column is all midnight - ",
          "did you mean to use _date?"))
    }
  }
}

#' Find shortest possible unique person_id
#'
#' Finds the shortest substring of `person_id` that retains uniqueness. All
#' substrings are the first few characters of the `person_id`s.
#'
#' Shortened person_ids are added by [`adm_annotate()`] using this function.
#'
#' @param person_id A character vector of person IDs to be shorten
#' @returns A character vector of the shortened person IDs
#' @seealso [adm_annotate()]
#' @examples
#'
#' person_id_shorten(c("AAA", "ABC", "ACD", "ADE"))
#'
#' person_id_shorten(c("XAAA", "XABC", "XACD", "XADE"))
#' @export
person_id_shorten <- function(person_id){
  original_unique <- length(unique(person_id))
  shorten_unique <- 0
  n_chars <- 0
  while (shorten_unique < original_unique){
    n_chars <- n_chars + 1
    person_id_short <- str_sub(person_id, end = n_chars)
    shorten_unique <- length(unique(person_id_short))
  }
  person_id_short
}

flip_names_and_values <- function(x){
  setNames(names(x), x)
}

#' Return of write to annotation database
#'
#' @param x The input `tbl` to the `_annotate` function
#' @param x_annotated The out of the `_annotate` function
#' @param annotation_db A `tbl` database source
#' @param id_cols A character vector of the columns to retain in the
#'   annotations database (to enable linking)
return_or_write_to_annotation_db <- function(x,
                                             x_annotated,
                                             annotation_db,
                                             id_cols){
  if (is.null(annotation_db)){
    x_annotated
  } else {
    keep_cols <- c(id_cols, setdiff(colnames(x_annotated), colnames(x)))
    dbAppendTable(annotation_db$src$con,
                  annotation_db$lazy_query$x,
                  x_annotated[, keep_cols])
  }
}
