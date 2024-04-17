#' Count functions
#'
#' @export
#' @rdname count
n_person <- function(x){
  length(unique(x$person_id))
}

#' @rdname count
#' @export
n_symbol <- function(x){
  length(unique(x$symbol))
}
