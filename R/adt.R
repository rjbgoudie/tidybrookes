#' Tidy raw tests colnames
#'
#' The standard data format from Clinical Informatics is handled by default. If
#' variations from this format occur, custom renaming can be performed
#' using the `names` argument
#'
#' @param x A data frame of raw adt data
#' @param names A vector of new_name = old_name pairs
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
adt_rename <- function(x,
                       names =
                         c(person_id = "STUDY_SUBJECT_DIGEST",
                           visit_id = "PAT_ENC_CSN",
                           event_type = "EVENT_TYPE",
                           start_datetime = "IN_DTTM",
                           discharge_datetime = "HOSP_DISCH_TIME",
                           department  = "ADT_DEPARTMENT_NAME",
                           room = "ROOM_NAME",
                           bed = "BED_LABEL",
                           service_area = "ADT_SERV_AREA_NAME",
                           service_name = "HOSP_SERV_NAME",
                           event_type_c = "EVENT_TYPE_C")){
  relocate_ignoring_missing(x, names)
}

#' Check discharge dates identical within a visit
#'
#' All discharge_datetime should be the same within a visit
adt_check_discharge_dates_identical <- function(x){
  x %>%
    group_by(person_id, visit_id) %>%
    summarise(check_all_discharge_dates_identical =
                all(discharge_datetime == first(discharge_datetime)))
}

#' Check disharge date consistent with final date
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

  # Remove duplicate rows
  out <- x %>%
    distinct_inform

  x %>%
    adt_check_discharge_dates_identical %>%
    check_that_all(check_all_discharge_dates_identical == TRUE,
                   name = "discharge dates identical within visit")

  x %>%
    adt_check_discharge_dates_consistent %>%
    check_that_all(check_discharge_dates_consistent == TRUE,
                   name = "discharge dates consistency")

  out <- out %>%
    group_by(person_id) %>%
    arrange(
      # Arrange by event_type_c to ensure admission events appear
      # before other events that occur at the same time
      start_datetime, event_type_c
    ) %>%
    mutate(visit_index = cumsum(event_type == "Admission"),
           .after = event_type) %>%
    select(-event_type_c) %>%
    group_by(visit_id) %>%
    mutate(end_datetime = lead(start_datetime), .after = start_datetime) %>%
    adt_department_summary() %>%
    left_join(department_labels,
              by = "department") %>%
    annotate_fn()

  ## TODO check for NAs in annotations
  ## out %>%
  ##   mutate(across(all_of(colnames(department_labels))))

  out
}

adt_department_summary <- function(x){
  x %>%
    group_by(person_id, visit_id) %>%
    mutate(department_just_moved = replace_na(lag(department) != department, TRUE),
           department_visit_index = cumsum(department_just_moved)) %>%
    group_by(person_id, visit_id, department_visit_index) %>%
    mutate(department_start_datetime = first(start_datetime),
           department_end_datetime = last(end_datetime),
           department_length_days =
             replace_na(interval(department_start_datetime, department_end_datetime)/days(1), 0)) %>%
    ungroup
}

