#' Tidy raw med_admin colnames
#' @param x A data frame of raw med_admin data
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
med_admin_rename <- function(x){
  x %>%
    rename(person_id = "STUDY_SUBJECT_DIGEST",
           administered_datetime = "TimeAdministered",
           name = "DrugName",
           dose = "DoseAsLabelled",
           rate = "InfusionRate",
           dose_abbreviated = "DoseUnitAbbreviated",
           route = "RouteOfMedicationAbbreviated",
           department = "DepartmentName",
           visit_id = "MAR_ENC_CSN",
           action = "MARAction")
}

#' Untidy raw med_admin colnames
#' @param x A data frame of raw med_admin data with tidy names
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
med_admin_unrename <- function(x){
  x %>%
    rename(STUDY_SUBJECT_DIGEST = "person_id",
           TimeAdministered = "administered_datetime",
           DrugName = "name",
           DoseAsLabelled = "dose",
           InfusionRate = "rate",
           DoseUnitAbbreviated = "dose_abbreviated",
           RouteOfMedicationAbbreviated = "route",
           DepartmentName = "department",
           MAR_ENC_CSN = "visit_id",
           MARAction = "action")
}
