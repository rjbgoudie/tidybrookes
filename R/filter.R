#' Filtering functions
#'
#' @rdname filter
#' @export
filter_person <- function(x, ...){
  x %>%
    filter(str_detect(.data$person_id, ...))
}

#' @rdname filter
#' @export
filter_visit <- function(x, visit_id){
  x %>%
    filter(.data$visit_id == visit_id)
}

#' @rdname filter
#' @export
filter_tests <- function(x, pattern){
  filter(x, str_detect(name, coll(pattern, ignore_case = TRUE)))
}

#' @rdname filter
#' @export
filter_med_prescr <- filter_med_admin <- function(x, pattern){
  x %>%
    filter(str_detect(name, regex(pattern, ignore_case = TRUE)))
}

#' @rdname filter
#' @export
filter_numeric <- function(x){
  filter(x, type == "numeric")
}

#' @rdname filter
#' @export
filter_logical <- function(x){
  filter(x, type == "logical")
}

#' @rdname filter
#' @export
filter_character <- function(x){
  filter(x, type == "character")
}
