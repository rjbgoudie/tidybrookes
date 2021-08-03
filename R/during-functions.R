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

max_during <- function(...){
  summarise_during(...,
                   type = "slice",
                   formula = which.max(value_as_number),
                   names_suffix = "max")
}
