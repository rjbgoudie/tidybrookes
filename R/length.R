n_person <- function(x){
  length(unique(x$person_id))
}

n_symbol <- function(x){
  length(unique(x$symbol))
}
