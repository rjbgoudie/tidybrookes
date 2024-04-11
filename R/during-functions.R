#' During functions
#'
#' @rdname during_functions
first_during <- function(...){
  summarise_during(...,
                   type = "slice",
                   formula = which.min(datetime),
                   names_suffix = "first")
}

#' @rdname during_functions
last_during <- function(...){
  summarise_during(...,
                   type = "slice",
                   formula = which.max(datetime),
                   names_suffix = "last")
}

#' @rdname during_functions
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

#' During, adjacent to, functions
#'
#' @rdname adjacent_event_during
first_during_after_event <- function(..., event_datetime, names_suffix = ""){
  event_datetime <- enquo(event_datetime)
  summarise_during(
    ...,
    type = "slice",
    formula =
      interval(!! event_datetime, datetime) %>%
      int_length() %>%
      purrr:::keep(~ .x > 0) %>%
      which.min(),
    names_suffix = glue("first_after_{names_suffix}"))
}

#' @rdname adjacent_event_during
nearest_to_event_during <- function(..., event_datetime, names_suffix = ""){
  event_datetime <- enquo(event_datetime)
  summarise_during(
    ...,
    type = "slice",
    formula =
      interval(!! event_datetime, datetime) %>%
      int_length() %>%
      abs() %>%
      which.min(),
    names_suffix = glue("nearest_to_{names_suffix}"))
}

#' @rdname during_functions
last_during_before_event <- function(..., event_datetime, names_suffix = ""){
  event_datetime <- enquo(event_datetime)
  summarise_during(
    ...,
    type = "slice",
    formula =
      interval(!! event_datetime, datetime) %>%
      int_length() %>%
      purrr:::keep(~ .x < 0) %>%
      which.max(),
    names_suffix = glue("last_before_{names_suffix}"))
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
#' @rdname extremes_during
max_during <- function(...){
  summarise_during(...,
                   type = "slice",
                   formula = which.max(value_as_number),
                   names_suffix = "max")
}

#' @rdname extremes_during
min_during <- function(...){
  summarise_during(...,
                   type = "slice",
                   formula = which.min(value_as_number),
                   names_suffix = "min")
}


ever_during <- function(...){
  summarise_during(...,
                   type = "summarise",
                   formula =
                     tibble(value_as_logical = any(value_as_logical),
                            type = "logical"),
                   names_suffix = "ever")
}
