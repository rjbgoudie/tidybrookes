#' Tidy raw diagnosis_pl colnames
#'
#' The standard data format from Clinical Informatics is handled by default. If
#' variations from this format occur, custom renaming can be performed
#' using the `names` argument
#'
#' @param x A data frame of raw diagnosis_pl data
#' @param names A vector of new_name = old_name pairs
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
med_hist_rename <- function(x,
                            names = default_rename("med_hist")){
  x %>%
    relocate_ignoring_missing(names) %>%
    mutate(source = "past_medical_history", .after = entered_datetime)
}
