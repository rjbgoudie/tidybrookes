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
