#' Basic checks and annotation of ADM data
#'
#' Performs basic checks and annotation of ADM data.
#'
#' Checks that
#' - all gender data are in known values (Female, Male or Unknown)
#'
#' Annotates each row with
#' - `visit_length_days`: length of stay (in days)
#' - `person_id_short`: shortened `person_id` created using [`person_id_shorten()`]
#'
#' @param x A tidied adm data frame, as tidied by [adm_rename()]
#' @return The supplied data frame `x` with additional annotations
#' @author R.J.B. Goudie
#' @importFrom dplyr group_vars
#' @seealso [adt_annotate()] annotates ADT data
#' @examples
#' adm_data_example
#' adm_annotate(adm_data_example) %>%
#'   relocate(visit_length_days, person_id_short)
#' @export
adm_annotate <- function(x){
  label_check_that_all(x,
                 gender %in% c("Female", "Male", "Unknown", "Not specified"),
                 label = "gender_all_female_male_unknown")
  original_groups <- group_vars(x)

  # duplication due to difftime() and interval() translation not yet in
  # duckdb
  if (inherits(x, "tbl_sql")){
    x %>%
      ungroup() %>%
      mutate(visit_length_days =
               date_diff('seconds',
                         visit_start_datetime,
                         visit_end_datetime)/(24 * 60 * 60),
             visit_length_hours =
               date_diff('seconds',
                         visit_start_datetime,
                         visit_end_datetime)/(60 * 60)) |>
      # collect() |>
      # mutate(person_id_short = person_id_shorten(person_id)) %>%
      group_by(across(all_of(original_groups)))
  } else {
  x %>%
    ungroup() %>%
    mutate(visit_length_days =
             interval(visit_start_datetime, visit_end_datetime)/ddays(1),
           visit_length_hours =
             interval(visit_start_datetime, visit_end_datetime)/dhours(1),
           person_id_short = person_id_shorten(person_id)) %>%
    group_by(across(all_of(original_groups)))
  }
}

