#' General convenience functions for filtering
#'
#' Convenience functions for filter any data by `person_id` (using
#' `filter_person()`), `visit_id` (using `filter_visit()`), test name (using
#' `filter_tests()`), drug name (using `filter_med_prescr()`); or by type of
#' data (numerical, logical or character).
#'
#' @param x A data frame with the relevant column (e.g. `person_id` or
#'   `visit_id`)
#' @param pattern A pattern to search for in a case insensitive manner (it is
#'   passed to [`stringr::coll()`] with `ignore_case = TRUE`.
#' @param id A visit id (either numeric or character)
#' @rdname filter
#' @examples
#' # Filtering is case insensitive
#' filter_person(adm_data_example, "A")
#' filter_person(adm_data_example, "a")
#'
#' # can filter visits by visit_id
#' filter_visit(adm_data_example, 4)
#' filter_visit(adm_data_example, "4")
#' @export
filter_person <- function(x, pattern){
  x %>%
    filter(str_detect(.data$person_id, coll(pattern, ignore_case = TRUE)))
}

#' @rdname filter
#' @export
filter_visit <- function(x, id){
  x %>%
    filter(.data$visit_id == id)
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
    filter(str_detect(name, coll(pattern, ignore_case = TRUE)))
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
