#' Tidy raw tests colnames
#' @param x A data frame of raw adt data
#' @return The supplied data frame, with column names in tidy-column-name format
#' @importFrom dplyr rename
#' @author R.J.B. Goudie
adt_rename <- function(x){
  x %>%
    rename(person_id = "STUDY_SUBJECT_DIGEST",
           event_type_c = "EVENT_TYPE_C",
           event_type = "EVENT_TYPE",
           in_datetime = "IN_DTTM",
           discharge_datetime = "HOSP_DISCH_TIME",
           admitting_department_name  = "ADT_DEPARTMENT_NAME",
           room = "ROOM_NAME",
           bed = "BED_LABEL",
           service_area = "ADT_SERV_AREA_NAME",
           service_name = "HOSP_SERV_NAME",
           encounter_id = "PAT_ENC_CSN")
}

