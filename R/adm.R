#' Tidy raw tests colnames
#'
#' The standard data format from Clinical Informatics is handled by default. If
#' variations from this format occur, custom renaming can be performed
#' using the `names` argument
#'
#' @param x A data frame of raw adm data
#' @param names A vector of new_name = old_name pairs
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
adm_rename <- function(x,
                       names = default_rename("adm")){
  relocate_ignoring_missing(x, names)
}

#' Untidy raw adm colnames
#'
#' The standard data format from Clinical Informatics is handled by default. If
#' variations from this format occur, custom renaming can be performed
#' using the `names` argument
#'
#' @param x A data frame of raw adm data with tidy names
#' @param names A vector of new_name = old_name pairs
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
adm_unrename <- function(x,
                         names =
                           c(STUDY_SUBJECT_DIGEST = "person_id",
                             PAT_ENC_CSN = "visit_id",
                             IN_DTTM = "visit_start_datetime",
                             HOSP_DISCH_TIME = "visit_end_datetime",
                             GENDER_DESC = "gender",
                             ETHNIC_GROUP_GROUPED = "ethnicity",
                             DATE_OF_DEATH = "death_date",
                             AGE_AT_ADM = "age_at_visit_start",
                             ADM_SERVICE = "adm_service",
                             ADT_DEPARTMENT_NAME = "ward",
                             DISCH_DEST = "discharge_destination",
                             DISCH_DECEASED = "discharged_deceased",
                             READMIT_WITHIN_30 = "readmitted_within_30_days")){
  relocate_ignoring_missing(x, names)
}

#' Annotate ADM data
#'
#' Checks that all the gender data are in known values.
#' Adds `visit_length_days`, `person_id_short`.
#'
#' @param x A tidied adm data frame, as tidied by [adm_rename()]
#' @return The supplied data frame `x` with additional annotations
#' @author R.J.B. Goudie
adm_annotate <- function(x){
  check_that_all(x,
                 gender %in% c("Female", "Male", "Unknown"),
                 name = "gender are Female, Male or Unknown")
  original_groups <- group_vars(x)

  x <- x %>%
    ungroup() %>%
    mutate(visit_length_days =
             as.numeric(visit_end_datetime - visit_start_datetime,
                        units = "days"),
           person_id_short = person_id_shorten(person_id)) %>%
    group_by(across(all_of(original_groups)))
}
