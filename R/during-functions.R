first_during <- function(...){
  summarise_during(...,
                   type = "slice",
                   formula = which.min(datetime),
                   names_suffix = "first")
}

last_during <- function(...){
  summarise_during(...,
                   type = "slice",
                   formula = which.max(datetime),
                   names_suffix = "last")
}

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

max_during <- function(...){
  summarise_during(...,
                   type = "slice",
                   formula = which.max(value_as_number),
                   names_suffix = "max")
}

min_during <- function(...){
  summarise_during(...,
                   type = "slice",
                   formula = which.min(value_as_number),
                   names_suffix = "min")
}

ever_during <- function(..., formula){
  formula <- enquo(formula)
  summarise_during(...,
                   type = "summarise",
                   formula = !! formula,
                   names_suffix = "ever")
}
