#' Tidy raw tests colnames
#'
#' The standard data format from Clinical Informatics is handled by default. If
#' variations from this format occur, custom renaming can be performed
#' using the `names` argument
#'
#' @param x A data frame of raw demogs data
#' @param names A vector of new_name = old_name pairs
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
demogs_rename <- function(x,
                       names =
                         c(person_id = "STUDY_SUBJECT_DIGEST",
                           gender = "GENDER_DESC")){
  relocate_ignoring_missing(x, names)
}

#' Untidy raw demogs colnames
#'
#' The standard data format from Clinical Informatics is handled by default. If
#' variations from this format occur, custom renaming can be performed
#' using the `names` argument
#'
#' @param x A data frame of raw demogs data with tidy names
#' @param names A vector of new_name = old_name pairs
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
demogs_unrename <- function(x,
                         names =
                           c(STUDY_SUBJECT_DIGEST = "person_id",
                             GENDER_DESC = "gender")){
  relocate_ignoring_missing(x, names)
}