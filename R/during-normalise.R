
normalise_dataframe <- function(x, type = "fsheet"){
  if (type == "fsheet"){
    x <- x %>%
      select(person_id = person_id,
             symbol = symbol,
             value_as_number = value_as_number,
             value_as_character = value_as_character,
             type = type,
             datetime = measurement_datetime)
  }
  x %>%
    arrange(datetime)
}
