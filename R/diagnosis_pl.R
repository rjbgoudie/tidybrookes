#' Tidy raw diagnosis_pl colnames
#' @param x A data frame of raw diagnosis_pl data
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
diagnosis_pl_rename <- function(x){
  x %>%
    rename(person_id = "STUDY_SUBJECT_DIGEST",
           description = "DX_DESCRIPTION",
           description_displayed = "DX_DESC_DISPLAYED",
           comment = "PROBLEM_CMT",
           entered_date = "DIAGNOSIS_ENTERED_DATE",
           diagnosis_date = "DIAGNOSIS_DATE",
           resolved_date = "RESOLVED_DATE",
           status = "DIAGNOSIS_STATUS",
           icd10_1 = "ICD10_1",
           icd10_2 = "ICD10_2",
           icd10_3 = "ICD10_3",
           icd10_4 = "ICD10_4",
           icd10_list = "ICD10_LIST",
           snomed = "SNOMED_CONCEPTS")
}

#' Untidy raw diagnosis_pl colnames
#' @param x A data frame of raw diagnosis_pl data with tidy names
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
diagnosis_pl_unrename <- function(x){
  x %>%
    rename(STUDY_SUBJECT_DIGEST = "person_id",
           DX_DESCRIPTION = "description",
           DX_DESC_DISPLAYED = "description_displayed",
           PROBLEM_CMT = "comment",
           DIAGNOSIS_ENTERED_DATE = "entered_date",
           DIAGNOSIS_DATE = "diagnosis_date",
           RESOLVED_DATE = "resolved_date",
           DIAGNOSIS_STATUS = "status",
           ICD10_1 = "icd10_1",
           ICD10_2 = "icd10_2",
           ICD10_3 = "icd10_3",
           ICD10_4 = "icd10_4",
           ICD10_LIST = "icd10_list",
           SNOMED_CONCEPTS = "snomed")
}