#' Tidy raw radiology colnames
#' @param x A data frame of raw radiology data
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
radiology_rename <- function(x){
  x %>%
    rename(person_id = "STUDY_SUBJECT_DIGEST",
           name = "Proc_Name",
           procedure_datetime = "Proc_Date",
           code = "Proc_Code",
           narrative = "Proc_Narrative",
           impression = "Proc_Impression",
           addenda = "Proc_Addenda",
           assessment = "Proc_Assessment")
}

#' Untidy raw radiology colnames
#' @param x A data frame of raw radiology data with tidy names
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
radiology_unrename <- function(x){
  x %>%
    rename(STUDY_SUBJECT_DIGEST = "person_id",
           Proc_Name = "name",
           Proc_Date = "procedure_datetime",
           Proc_Code = "code",
           Proc_Narrative = "narrative",
           Proc_Impression = "impression",
           Proc_Addenda = "addenda",
           Proc_Assessment = "assessment")
}
