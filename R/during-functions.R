#' Extract first or last measurement during a timeperiod
#'
#' Extract the first measurement or the last measurement during a timeperiod.
#'
#' @inheritDotParams summarise_during x y datetime during
#' @rdname during_functions
#' @examples
#' # raw data that we will combine
#' adm_data_example
#' fsheet_news2_example
#'
#' # extract the first of each fsheet symbol during the hospital visit
#' adm_data_example %>%
#'   first_during(fsheet_news2_example,
#'                datetime = measurement_datetime,
#'                during = "during_visit") %>%
#'   select(person_id,
#'          visit_id,
#'          visit_start_datetime,
#'          visit_end_datetime,
#'          contains("news2"))
#'
#' # extract the last of each fsheet symbol during the hospital visit
#' adm_data_example %>%
#'  last_during(fsheet_news2_example,
#'              datetime = measurement_datetime,
#'              during = "during_visit") %>%
#'   select(person_id,
#'          visit_id,
#'          visit_start_datetime,
#'          visit_end_datetime,
#'          contains("news2"))
#'
#' # can pipe together two summaries
#' adm_data_example %>%
#'   first_during(fsheet_news2_example,
#'                datetime = measurement_datetime,
#'                during = "during_visit") %>%
#'   last_during(fsheet_news2_example,
#'               datetime = measurement_datetime,
#'               during = "during_visit") %>%
#'   select(person_id,
#'          visit_id,
#'          visit_start_datetime,
#'          visit_end_datetime,
#'          contains("news2"))
#'
#' # example data with 3 fsheet data fields
#' fsheet_example
#'
#' # summaries for all symbols can be calculated together
#' adm_data_example %>%
#'   first_during(fsheet_example,
#'                datetime = measurement_datetime,
#'                during = "during_visit") %>%
#'   select(person_id,
#'          visit_id,
#'          visit_start_datetime,
#'          visit_end_datetime,
#'          contains("during"))
#' @export
first_during <- function(...){
  summarise_during(...,
                   type = "slice",
                   formula = which.min(datetime),
                   names_suffix = "first")
}

#' @rdname during_functions
#' @export
last_during <- function(...){
  summarise_during(...,
                   type = "slice",
                   formula = which.max(datetime),
                   names_suffix = "last")
}

#' Extract first measurement after/last measurement before a specific event,
#' during a timeperiod
#'
#' Extract the last measurement before or the first measurement after a
#' specific other event, during a timepoint.
#'
#' @inheritDotParams summarise_during x y datetime during
#' @param event_datetime The datetime that the extraction should be relative
#'   to.
#' @param names_suffix A text label to use as the end of the column name
#'   created.
#' @rdname adjacent_event_during
#' @examples
#' #' # raw data that we will combine
#' adm_data_example
#' fsheet_news2_example
#'
#' # extract the first NEWS2 record during each hospital visit
#' first_news2 <- adm_data_example %>%
#'   first_during(fsheet_news2_example,
#'              datetime = measurement_datetime,
#'              during = "during_visit") %>%
#'   select(person_id,
#'          visit_id,
#'          visit_start_datetime,
#'          visit_end_datetime,
#'          contains("news2"))
#'
#' # extract the first SpO2 measurement that FOLLOW the first NEWS2 record
#' # during each hospital visit
#' first_news2 %>%
#'  first_during_after_event(fsheet_spo2_example,
#'                           datetime = measurement_datetime,
#'                           during = "during_visit",
#'                           event_datetime = news2_first_during_visit_datetime,
#'                           names_suffix = "first_news2") %>%
#'   select(person_id,
#'          visit_id,
#'          contains("spo2"),
#'          contains("news2"))
#'
#' # extract the LAST SpO2 measurement that is before (or the same time as)
#' # the first NEWS2 record during each hospital visit
#' first_news2 %>%
#'  last_during_before_event(fsheet_spo2_example,
#'                           datetime = measurement_datetime,
#'                           during = "during_visit",
#'                           event_datetime = news2_first_during_visit_datetime,
#'                           names_suffix = "first_news2") %>%
#'   select(person_id,
#'          visit_id,
#'          contains("spo2"),
#'          contains("news2"))
#' @export
first_during_after_event <- function(..., event_datetime, names_suffix = ""){
  event_datetime <- enquo(event_datetime)
  summarise_during(
    ...,
    type = "slice",
    formula =
      interval(!! event_datetime, datetime) %>%
      int_length() %>%
      purrr:::map(~ case_when(.x < 0 ~ NA,
                              TRUE ~ .x)) %>%
      which.min(),
    names_suffix = glue("first_after_{names_suffix}"))
}

#' @rdname adjacent_event_during
#' @export
last_during_before_event <- function(..., event_datetime, names_suffix = ""){
  event_datetime <- enquo(event_datetime)
  summarise_during(
    ...,
    type = "slice",
    formula =
      interval(!! event_datetime, datetime) %>%
      int_length() %>%
      purrr:::map(~ case_when(.x > 0 ~ NA,
                              TRUE ~ .x)) %>%
      which.max(),
    names_suffix = glue("last_before_{names_suffix}"))
}

#' Extract measurements during a timeperiod nearest to a specific event
#'
#' Extract the nearest measurement to a specific other event.
#'
#' @inheritDotParams summarise_during x y datetime during
#' @param event_datetime The datetime that the extraction should be relative
#'   to.
#' @param names_suffix A text label to use as the end of the column name
#'   created.
#' @examples
#' #' # raw data that we will combine
#' #' # raw data that we will combine
#' adm_data_example
#' fsheet_news2_example
#'
#' # extract the first NEWS2 record during each hospital visit
#' first_news2 <- adm_data_example %>%
#'   first_during(fsheet_news2_example,
#'              datetime = measurement_datetime,
#'              during = "during_visit") %>%
#'   select(person_id,
#'          visit_id,
#'          visit_start_datetime,
#'          visit_end_datetime,
#'          contains("news2"))
#'
#' # extract the NEAREST (either before or after) SpO2 measurement that to
#' # the first NEWS2 record during each hospital visit
#' first_news2 %>%
#'  nearest_to_event_during(fsheet_spo2_example,
#'                          datetime = measurement_datetime,
#'                          during = "during_visit",
#'                          event_datetime = news2_first_during_visit_datetime,
#'                          names_suffix = "first_news2") %>%
#'   select(person_id,
#'          visit_id,
#'          contains("spo2"),
#'          contains("news2"))
#'
#' # extract the NEAREST (either before or after) SpO2 measurement that to
#' # the first NEWS2 record during each hospital visit
#' first_news2 %>%
#'  nearest_visit_start_during(
#'    fsheet_spo2_example,
#'    datetime = measurement_datetime,
#'    during = "14_days_before_visit_until_visit_end") %>%
#'   select(person_id,
#'          visit_id,
#'          contains("spo2"),
#'          contains("news2"))
#' @rdname nearest_to_event_during
#' @export
nearest_to_event_during <- function(..., event_datetime, names_suffix = ""){
  event_datetime <- enquo(event_datetime)
  summarise_during(
    ...,
    type = "slice",
    formula =
      interval(!! event_datetime, datetime) %>%
      int_length() %>%
      map(~abs(.x)) %>%
      which.min(),
    names_suffix = glue("nearest_to_{names_suffix}"))
}

#' @rdname nearest_to_event_during
#' @export
nearest_visit_start_during <- function(...){
  summarise_during(
    ...,
    type = "slice",
    formula =
      interval(visit_start_datetime, datetime) %>%
      int_length() %>%
      abs() %>%
      which.min(),
    names_suffix = "nearest_visit_start")
}

# nearest_to_event_during <- function(...,
#                                     event_datetime,
#                                     names_suffix = "",
#                                     max_before_hours = Inf,
#                                     max_after_hours = Inf){
#   event_datetime <- enquo(event_datetime)
#   summarise_during(
#     ...,
#     type = "slice", # TOD REVERT
#     formula = slice_nearest_within(!! event_datetime,
#                                    datetime,
#                                    max_before_hours,
#                                    max_after_hours)
#     ,
#     names_suffix = glue("nearest_to_{names_suffix}"))
# }
#
# slice_nearest_within <- function(datetime, event_datetime, max_before_hours, max_after_hours){
#   which_min <- interval(event_datetime, datetime) %>%
#     map(~ .x / hours(1)) %>%
#     keep(~ .x >= -max_before_hours & .x <= max_after_hours) %>%
#     map(~abs(.x)) %>%
#     which.min()
#   if (length(which_min) == 0){
#     NA
#   } else if(length(which_min) == 1){
#     rep(which_min, n())
#   }
# }

#' Extremes during
#'
#' @inheritDotParams summarise_during x y datetime during
#' @examples
#' # raw data that we will combine
#' adm_data_example
#' fsheet_news2_example
#'
#' # extract the max of each fsheet symbol during the hospital visit
#' adm_data_example %>%
#'   max_during(fsheet_news2_example,
#'              datetime = measurement_datetime,
#'              during = "during_visit") %>%
#'   select(person_id,
#'          visit_id,
#'          visit_start_datetime,
#'          visit_end_datetime,
#'          contains("news2"))
#'
#' # extract the min of each fsheet symbol during the hospital visit
#' adm_data_example %>%
#'  min_during(fsheet_news2_example,
#'             datetime = measurement_datetime,
#'             during = "during_visit") %>%
#'   select(person_id,
#'          visit_id,
#'          visit_start_datetime,
#'          visit_end_datetime,
#'          contains("news2"))
#'
#' # can pipe together two summaries
#' adm_data_example %>%
#'   max_during(fsheet_news2_example,
#'              datetime = measurement_datetime,
#'              during = "during_visit") %>%
#'   min_during(fsheet_news2_example,
#'              datetime = measurement_datetime,
#'              during = "during_visit") %>%
#'   select(person_id,
#'          visit_id,
#'          visit_start_datetime,
#'          visit_end_datetime,
#'          contains("news2"))
#' @rdname extremes_during
#' @export
max_during <- function(...){
  summarise_during(...,
                   type = "slice",
                   formula = which.max(value_as_number),
                   names_suffix = "max")
}

#' @rdname extremes_during
#' @export
min_during <- function(...){
  summarise_during(...,
                   type = "slice",
                   formula = which.min(value_as_number),
                   names_suffix = "min")
}


#' @export
ever_during <- function(...){
  summarise_during(...,
                   type = "summarise",
                   formula =
                     tibble(value_as_logical = any(value_as_logical),
                            type = "logical"),
                   names_suffix = "ever")
}
