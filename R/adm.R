#' Annotate ADM data
#'
#' Checks that all the gender data are in known values.
#' Adds `visit_length_days`, `person_id_short`.
#'
#' @param x A tidied adm data frame, as tidied by [adm_rename()]
#' @return The supplied data frame `x` with additional annotations
#' @author R.J.B. Goudie
#' @export
adm_annotate <- function(x){
  check_that_all(x,
                 gender %in% c("Female", "Male", "Unknown"),
                 name = "gender are Female, Male or Unknown")
  original_groups <- group_vars(x)

  x <- x %>%
    ungroup() %>%
    mutate(visit_length_days =
             as.numeric(visit_end_datetime - visit_start_datetime,
                        units = "days"),
           person_id_short = person_id_shorten(person_id)) %>%
    group_by(across(all_of(original_groups)))
}
