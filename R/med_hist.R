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
                            names =
                              c(person_id = "STUDY_SUBJECT_DIGEST",
                                icd10_list = "CURRENT_ICD10_LIST",
                                description = "DX_NAME",
                                entered_datetime = "CONTACT_DATE",
                                medical_history_datetime_freetext = "MEDICAL_HX_DATE",
                                visit_id = "HX_LNK_ENC_CSN",
                                comment = "COMMENTS",
                                comment_annotation = "MED_HX_ANNOTATION")){
  x %>%
    relocate_ignoring_missing(names) %>%
    mutate(source = "past_medical_history", .after = entered_datetime)
}
