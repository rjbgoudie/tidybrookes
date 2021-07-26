#' Left join with fsheet data
#'
#' @param x An `adm` data frame
#' @param y A (tidy) `fsheet` data frame: with `person_id`, `symbol`,
#'   `value_as_number` (or `value_as_character`; or both, plus a `type` column
#'   indicating which to use when summarising) and `measurement_datetime`.
#' @param during The time period to extract data for, one of: `"during_visit"`,
#'   `"during_icu"`
#'
#' @return
#' A data frame, with a row for each fsheet measurement during the relevant
#' time period for each patient. Note that rows of the `adm` data will be
#' repeated multiple times (one for each fsheet measurement data point).
#' @author R.J.B. Goudie
fsheet_all_during <- function(x, y, during){
  y_standardised <- normalise_dataframe(y, type = "fsheet")

  all_during(x = x,
             y = y_standardised,
             during = during)
}

fsheet_summarise_during <- function(x,
                                    y,
                                    during,
                                    type,
                                    formula,
                                    names_suffix = ""){
  formula <- enquo(formula)
  y_standardised <- normalise_dataframe(y, type = "fsheet")

  summarise_during(x = x,
                   y = y_standardised,
                   during = during,
                   type = type,
                   formula = !! formula,
                   names_suffix = glue("{names_suffix}_{during}"))
}

fsheet_first_during <- function(...){
  fsheet_summarise_during(...,
                          type = "slice",
                          formula = which.min(datetime),
                          names_suffix = "first")
}

fsheet_last_during <- function(...){
  fsheet_summarise_during(...,
                          type = "slice",
                          formula = which.max(datetime),
                          names_suffix = "last")
}
