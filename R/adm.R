#' Basic checks and annotation of ADM data
#'
#' Performs basic checks and annotation of ADM data.
#'
#' Checks that
#' - all gender data are in known values (Female, Male or Unknown)
#'
#' Annotates each row with
#' - `visit_length_days`: length of stay (in days)
#' - `person_id_short`: shortened `person_id` created using [`person_id_shorten()`]
#'
#' @param x A tidied adm data frame, as tidied by [adm_rename()]
#' @return The supplied data frame `x` with additional annotations
#' @author R.J.B. Goudie
#' @importFrom dplyr group_vars
#' @seealso [adt_annotate()] annotates ADT data
#' @examples
#' adm_data_example
#' adm_annotate(adm_data_example) %>%
#'   relocate(visit_length_days, person_id_short)
#' @export
adm_annotate <- function(x){
  check_that_all(x,
                 gender %in% c("Female", "Male", "Unknown"),
                 name = "gender are Female, Male or Unknown")
  original_groups <- group_vars(x)

  x %>%
    ungroup() %>%
    mutate(visit_length_days =
             as.numeric(visit_end_datetime - visit_start_datetime,
                        units = "days"),
           person_id_short = person_id_shorten(person_id)) %>%
    group_by(across(all_of(original_groups)))
}
