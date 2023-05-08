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
diagnosis_pl_rename <- function(x,
                                names = default_rename("diagnosis_pl")){
  x %>%
    relocate_ignoring_missing(names) %>%
    mutate(source = "problem_list", .after = resolved_datetime)
}

#' Untidy raw diagnosis_pl colnames
#'
#' The standard data format from Clinical Informatics is handled by default. If
#' variations from this format occur, custom renaming can be performed
#' using the `names` argument
#'
#' @param x A data frame of raw diagnosis_pl data with tidy names
#' @param names A vector of new_name = old_name pairs
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
diagnosis_pl_unrename <- function(x,
                                  names =
                                    c(STUDY_SUBJECT_DIGEST = "person_id",
                                      DX_DESCRIPTION = "description",
                                      DX_DESC_DISPLAYED = "description_displayed",
                                      PROBLEM_CMT = "comment",
                                      DIAGNOSIS_ENTERED_DATE = "entered_datetime",
                                      DIAGNOSIS_DATE = "diagnosis_datetime",
                                      RESOLVED_DATE = "resolved_datetime",
                                      DIAGNOSIS_STATUS = "status",
                                      ICD10_1 = "icd10_1",
                                      ICD10_2 = "icd10_2",
                                      ICD10_3 = "icd10_3",
                                      ICD10_4 = "icd10_4",
                                      ICD10_LIST = "icd10_list",
                                      SNOMED_CONCEPTS = "snomed")){
  relocate_ignoring_missing(x, names)
}
