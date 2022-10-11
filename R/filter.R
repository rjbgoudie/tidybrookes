filter_person <- function(x, ...){
  x %>%
    filter(str_detect(.data$person_id, ...))
}

filter_visit <- function(x, visit_id){
  x %>%
    filter(.data$visit_id == visit_id)
}

filter_tests <- function(x, pattern){
  filter(x, str_detect(name, coll(pattern, ignore_case = TRUE)))
}

filter_med_prescr <- filter_med_admin <- function(x, pattern){
  x %>%
    filter(str_detect(name, coll(pattern, ignore_case = TRUE)))
}

filter_numeric <- function(x){
  filter(x, type == "numeric")
}

filter_logical <- function(x){
  filter(x, type == "logical")
}

filter_character <- function(x){
  filter(x, type == "character")
}
