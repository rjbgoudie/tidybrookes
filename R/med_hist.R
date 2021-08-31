#' Tidy raw diagnosis_pl colnames
#' @param x A data frame of raw diagnosis_pl data
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
med_hist_rename <- function(x){
  x %>%
    rename(person_id = "STUDY_SUBJECT_DIGEST",
           icd10_list = "CURRENT_ICD10_LIST",
           description = "DX_NAME",,
           entered_datetime = "CONTACT_DATE",
           medical_history_datetime_freetext = "MEDICAL_HX_DATE",
           visit_id = "HX_LNK_ENC_CSN",
           comment = "COMMENTS",
           comment_annotation = "MED_HX_ANNOTATION") %>%
    relocate(person_id, icd10_list, description, entered_datetime,
             medical_history_datetime_freetext, visit_id, comment,
             comment_annotation) %>%
    mutate(source = "past_medical_history", .after = entered_datetime)
}
