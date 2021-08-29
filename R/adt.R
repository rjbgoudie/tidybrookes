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
           start_datetime = "IN_DTTM",
           discharge_datetime = "HOSP_DISCH_TIME",
           department  = "ADT_DEPARTMENT_NAME",
           room = "ROOM_NAME",
           bed = "BED_LABEL",
           service_area = "ADT_SERV_AREA_NAME",
           service_name = "HOSP_SERV_NAME",
           visit_id = "PAT_ENC_CSN") %>%
    relocate(visit_id, .after = person_id)
}


adt_check_discharge_dates_consistent <- function(x){
  x %>%
    group_by(person_id, visit_id) %>%
    arrange(start_datetime) %>%
    summarise(
      check_discharge_dates_consistent =
        if_else(
          is.na(discharge_datetime),
          true = TRUE,
          false = all(last(start_datetime) == discharge_datetime)))
}

adt_annotate <- function(x, fixed_labels, annotate_fn){
  # check numeric event type and text event type match
  stopifnot(x %>% count(event_type_c, event_type) %>% nrow == 3)

  x %>%
    adt_check_discharge_dates_consistent %>%
    check_that_all(check_discharge_dates_consistent == TRUE,
                   name = "discharge dates consistency")

  out <- x %>%
    group_by(person_id) %>%
    arrange(
      # Arrange by event_type_c to ensure admission events appear
      # before other events that occur at the same time
      start_datetime, event_type_c
    ) %>%
    mutate(visit_index = cumsum(event_type == "Admission"),
           .after = event_type) %>%
    select(-event_type_c) %>%
    left_join(department_labels,
              by = "department") %>%
    annotate_fn()

  ## TODO check for NAs in annotations
  ## out %>%
  ##   mutate(across(all_of(colnames(department_labels))))

  out
}
