coarsen_by_day <- function(x){
  x_numeric <- filter_type_numeric(x)
  if (nrow(x_numeric) > 0){
    x_numeric <- x_numeric %>%
      mutate(datetime = floor_date(datetime, unit = "day")) %>%
      group_by(person_id, visit_id, symbol, datetime) %>%
      summarise(across(),
                value_as_number = mean(value_as_number, na.rm = TRUE))
  }
  x_logical <- filter_type_logical(x)
  if (nrow(x_logical) > 0){
    x_logical <- x_logical %>%
      mutate(datetime = floor_date(datetime, unit = "day")) %>%
      group_by(person_id, visit_id, symbol, datetime) %>%
      summarise(across(),
                value_as_logical =  mean(value_as_logical, na.rm = TRUE) >= 0.5
      )
  }
  x_character <- filter_type_character(x)
  if (nrow(x_character) > 0){
    x_character <- x_character %>%
      mutate(datetime = floor_date(datetime, unit = "day")) %>%
      group_by(person_id, visit_id, symbol, datetime) %>%
      summarise(across(),
                value_as_character =  mode(value_as_character))
  }
  bind_rows(x_numeric, x_logical, x_character)
}
