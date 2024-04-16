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

#' Filter, but report the filtering that occurs
#'
#' @param x A data frame
#' @param ... Passed to filter
#' @param since A character describing the reason for the filtering
filter_inform <- function(x, ..., since = "for unknown reason"){
  previous <- nrow(x)
  out <- filter(x, ...)
  current <- nrow(out)
  if (current != previous){
    sign <- if_else(current < previous, "removed", "added")
    change_abs <- abs(current - previous)
    row <- if_else(change_abs == 1, "row", "rows")
    cli::cli_alert_info("{change_abs} {row} {sign} {since}")
  }
  out
}

#' Distinct, but report the filtering that occurs
#'
#' @param x A data frame
#' @param since A character describing the reason for the filtering
distinct_inform <- function(x){
  since <- "since rows were exact duplicates"
  previous <- nrow(x)
  out <- distinct(x)
  current <- nrow(out)
  if (current != previous){
    sign <- if_else(current < previous, "removed", "added")
    change_abs <- abs(current - previous)
    row <- if_else(change_abs == 1, "row", "rows")
    cli::cli_alert_info("{change_abs} {row} {sign} {since}")
  }
  out
}

fn_inform <- function(x, fn, ..., since = "for unknown reason"){
  previous <- nrow(x)
  out <- fn(x, ...)
  current <- nrow(out)
  if (current != previous){
    sign <- if_else(current < previous, "removed", "added")
    change_abs <- abs(current - previous)
    row <- if_else(change_abs == 1, "row", "rows")
    cli::cli_alert_info("{change_abs} {row} {sign} {since}")
  }
  out
}

#' Check all rows of a data frame satisfy a condition
#'
#' @param x A data frame
#' @param condition A condition
#' @param name Character string, a name for the condition
check_that_all <- function(x,
                           condition,
                           name = "Unnamed",
                           summary = identity){
  condition <- enquo(condition)
  unexpected <- x %>%
    mutate(satisfies_expect_before = !!condition) %>%
    filter(!satisfies_expect_before)

  if (nrow(unexpected) > 0){
    condition_str <- expr_print(condition)
    unexpected_nrow <- nrow(unexpected)
    row <- if_else(unexpected_nrow == 1, "row", "rows")
    cli::cli_alert_warning(
      c("{unexpected_nrow} {row} not satisfying ",
        "{name} condition: ",
        "{condition_str}"))
    print(summary(unexpected))
  }
}

#' Alert user if all datetime are midnight
#'
#' Does nothing if a `Date` is supplied
#'
#' @param x A `POSIXct` datetime
inform_if_all_times_are_midnight <- function(x){
  if (inherits(x, "POSIXct")){
    h <- lubridate:::hour(x)
    m <- lubridate:::minute(x)
    s <- lubridate:::second(x)
    if (all(h == 0L & m == 0L & s == 0)){
      cli::cli_alert_danger(
        c("Supplied datetime column is all midnight - ",
          "did you mean to use _date?"))
    }
  }
}

#' Apply function ignoring missing columns
#'
#'
#' @param fn A function
#' @param x A data frame
#' @param ... Arguments to `fn`, which should be columns names of `x`
fn_ignoring_missing <- function(fn, x, ...){
  name_change <- c(...)
  missing_cols <- name_change[!name_change %in% colnames(x)]
  if (length(missing_cols) > 0){
    cli::cli_alert_warning(
      c("Some columns were missing: ",
        glue::glue_collapse(missing_cols, ",", last = " and ")))
  }
  x %>%
    fn(any_of(name_change))
}

#' Rename columns, ignoring missing columns
#'
#' A version of `rename` that allows providing renaming pairs that do not
#' exist in the supplied data frame. In this case, a message will inform users
#' of the missing columns, but the function will performthe renaming on
#' columns that exist without error.
#' @param ... A data frame, and rename pairs
#' @importFrom dplyr rename
rename_ignoring_missing <- function(...){
  fn_ignoring_missing(rename, ...)
}

#' Change column order, ignoring missing columns
#'
#' A version of `relocate` that gracefully handles columns that do not
#' exist in the supplied data frame. In this case, a message will inform users
#' of the missing columns, but the function will perform the relocate on
#' columns that exist without error.
#' @param ... A data frame, and rename pairs
#' @importFrom dplyr relocate
relocate_ignoring_missing <- function(...){
  fn_ignoring_missing(rename, ...)
}

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
