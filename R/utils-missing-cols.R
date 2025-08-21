#' Apply function ignoring missing columns
#'
#'
#' @param fn A function
#' @param x A data frame
#' @param ... Arguments to `fn`, which should be columns names of `x`
#' @noRd
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
#' @noRd
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
#' @noRd
relocate_ignoring_missing <- function(...){
  fn_ignoring_missing(relocate, ...)
}
