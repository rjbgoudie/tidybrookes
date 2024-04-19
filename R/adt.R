#' Summarise department visits data in ADT data
#'
#' Annotates department visits with `department_visit_index`, which indexes
#' the department visits WITHIN a `visit_id`.
#'
#' Also adds `department_start_datetime`, `department_end_datetime`,
#' `department_length_days`.
#'
#' @param x Tidy ADT data, as tidied by [adt_rename()]
#' @return The supplied tidy ADT data `x`, with an additional columns
#' @author R.J.B. Goudie
#' @importFrom lubridate days
#' @noRd
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

#' Check discharge dates identical within a visit
#'
#' Check that all discharge_datetime are the same within a `visit_id``
#'
#' @param x Tidy ADT data, as tidied by [adt_rename()]
#' @return The supplied tidy ADT data `x`, with an additional logical column
#'   `check_all_discharge_dates_identical`, with `FALSE` indicating
#'   inconsistencies
#' @author R.J.B. Goudie
#' @export
adt_check_discharge_dates_identical <- function(x){
  x %>%
    group_by(person_id, visit_id) %>%
    summarise(check_all_discharge_dates_identical =
                all(discharge_datetime == first(discharge_datetime)))
}

#' Check disharge date consistent with final date
#'
#' @param x Tidy ADT data, as tidied by [adt_rename()]
#' @return The supplied tidy ADT data `x`, with an additional logical column
#'   `check_discharge_data_consistent`, with `FALSE` indicating inconsistencies
#' @author R.J.B. Goudie
#' @export
adt_check_discharge_dates_consistent <- function(x){
  x %>%
    group_by(person_id, visit_id) %>%
    arrange(start_datetime) %>%
    summarise(
      check_discharge_dates_consistent =
        all(
          if_else(
            is.na(discharge_datetime),
            true = TRUE,
            false = all(last(start_datetime) == discharge_datetime))
        )
    )
}

#' Basic cleanup, checks, and annotation of ADT data
#'
#' Performs basic cleanup, checks and annotation of ADT data with indexes for the
#' hospital visit (`visit_index`) and the visit to each departments
#' (`department_visit_index`), and basic labels for each `department`.
#'
#' The clean and checks performed are:
#' 1. Removes duplicate rows
#' 1. Checks discharge dates are identical within a visit
#" 1. Checks discharge date matches the final datetime for the visit
#'
#' The annotations added are:
#' 1. Labels hospital visits via `visit_index`
#' 1. Adds the `end_datetime` for each row of ADT (which is not always included
#'    in the raw data)
#' 1. Labels visits to specific departments. A visit to a single department
#'    may involve multiple rows of ADT data, since a patient may move beds
#'    within a department one or more times. The visits are labelled with
#'    `department_visit_index`, and the corresponding times recorded in
#'    `department_start_datetime` and `department_end_datetime`, and
#'    `department_length_days`.
#' 1. (Left) joins in `fixed_labels`
#' 1. Runs the user-supplied annotation function `annotate_fn`
#'
#' @param x Tidy ADT data, as tidied by [adt_rename()]
#' @param fixed_labels A data frame with a colum `department`, which will be
#'   joined to the ADT data
#' @param annotate_fn A function that provides additional annotation
#' @return The supplied tidy ADT data `x` with additional annotations
#' @author R.J.B. Goudie
#' @seealso [adm_annotate()] annotates ADM data
#' @examples
#' fixed_labels <-
#'   tibble::tribble(
#'     ~department, ~is_ed,
#'     "ADD EMERGENCY DEPT", TRUE,
#'     "ADD C2 WARD", FALSE,
#'     "ADD D2 WARD", FALSE,
#'     "DIALYSIS CENTRE", FALSE,
#'     "ADD A1 WARD", FALSE,
#'     "ADD A2 WARD", FALSE,
#'     "ADD A3 WARD", FALSE,
#'     "ADD A4 WARD", FALSE,
#'     "ADD X WARD", FALSE,
#'     "ADD Y WARD", FALSE,
#'     "ADD Z WARD", FALSE)
#'
#' adt_data_example <-
#'   read_tidybrookes_csv(
#'     file = tidybrookes_example("adt.csv"),
#'     col_types = "adt"
#'   ) %>%
#'   adt_rename
#'
#' adt_data_example %>%
#'   adt_annotate(annotate_fn = identity,
#'                fixed_labels = fixed_labels)
#'
#' @export
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
    left_join(fixed_labels,
              by = "department") %>%
    annotate_fn()

  ## TODO check for NAs in annotations
  ## out %>%
  ##   mutate(across(all_of(colnames(department_labels))))

  out
}
