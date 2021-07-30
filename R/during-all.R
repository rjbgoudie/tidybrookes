#' Left join adm data with other (standardised) data
#'
#' Left joins an `adm` data frame with another data frame (with a
#' `person_id` for linking and with the relevant datetime called `datetime`),
#' and filter to only those rows of `y` with `datetime` within the specified
#' time period.
#'
#' @param x An `adm` data frame
#' @param y A (tidy) data frame, with a `datetime` column
#' @param during The time period to extract data for, one of: `"during_visit"`,
#'   `"during_icu"`
#' @param names_from A character vector containing the names of columns that
#' should be used to filter by (in addition to those implied by `during`).
#'
#' @return
#' A data frame, with a row for each `y` measurement during the relevant
#' time period for each patient. Note that rows of the `adm` data will be
#' repeated multiple times (one for each `y` measurement data point).
#' @author R.J.B. Goudie
all_during <- function(x,
                       y,
                       during,
                       names_from = "symbol"){
 if (during == "during_visit"){
   out <- x %>%
      left_join_filter(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime &
                       datetime <= visit_end_datetime,
                    is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime))
 } else if (during == "during_icu"){
   out <- x %>%
     left_join_filter(
       y,
       join_by = "person_id",
       filter_by = c("person_id", "visit_id", "icu_visit_id", names_from),
       filter_condition =
         case_when(!is.na(icu_end_datetime) ~
                     datetime >= icu_start_datetime &
                      datetime <= icu_end_datetime,
                   is.na(icu_end_datetime) ~
                     datetime >= icu_start_datetime))
 } else {
   stop("all_during does not know how to hangle this `during` value:",
        during)
 }
  out
}

#' Left join according to a filter and summarise in one
#'
#' @inheritParams inner_join_filter
#'
#' @return A data frame, with all rows of `x`, potentially duplicated.
#' Where there is no matching `y` row, `NA`s will be present.
#'
#' A match is determined by joining by `join_by` and then
#' filtering using the `filter_condition`, under the `filter_by` grouping.
#'
#' @author R.J.B. Goudie
left_join_filter <- function(x, y, ...){
  out <- inner_join_filter(x = x,
                           y = y,
                           ...)

  # join again to x on common columns, so that anyone WITHOUT a y is included
  by <- intersect(colnames(x), colnames(out))
  left_join(x, out, by = by)
}

#' Inner join according to a filter and summarise in one
#'
#' Join two data frames according to some columns (`join_by`), but then
#' remove some of the rows according to the `filter_condition` (which is run
#' after grouping via `filter_by`). This amounts to an inner join.
#'
#' @param x A data frame
#' @param y A data frame
#' @param join_by A character vector of variables to join `x` and `y` by
#' @param filter_by A character vector indicating the grouping to use when
#'   filtering
#' @param filter_condition An expression that indicates whether the
#'   corresponding row should be included
#'
#' @return A data frame, with all rows of `x` (potentially duplicated) with at
#' least one matching `y`
#' row, where the match is determined by joining by `join_by` and then
#' filtering using the `filter_condition`, under the `filter_by` grouping.
#' @author R.J.B. Goudie
inner_join_filter <- function(x,
                              y,
                              join_by,
                              filter_by,
                              filter_condition){
  filter_condition <- enquo(filter_condition)
  formula <- enquo(formula)

  filter_by <- as.list(filter_by)
  filter_by_symbol <- rlang::syms(filter_by)

  out <- x %>%
    left_join(y, by = join_by, na_matches = "never") %>%
    group_by(!!! filter_by_symbol) %>%
    mutate(satisfies_filter_condition = !! filter_condition) %>%
    filter(satisfies_filter_condition) %>%
    group_by(!!! filter_by_symbol) %>%
    select(-satisfies_filter_condition)
}
