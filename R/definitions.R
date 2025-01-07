#' Wrap definition in a list
#'
#' Makes handling single-item definition lists easier by wrapping them in an
#' enclosing list, so that they can be handled using lapply etc.
#'
#' @param def A definition list
wrap_def_if_single <- function(def){
  if (length(def) > 5 & "symbol" %in% names(def)){
    list(def)
  } else {
    def
  }
}
