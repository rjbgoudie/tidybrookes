#' Join adm data with other (standardised) data
#'
#' Joins an `adm` data frame with another data frame (with a
#' `person_id` for linking and with the relevant datetime called `datetime`),
#' and filter to only those rows of `y` with `datetime` within the specified
#' time period.
#'
#' @param x An `adm` data frame
#' @param y A (tidy) data frame, with a `datetime` column
#' @param datetime The column of `y` to use as the main datetime for matching
#' @param during The time period to extract data for, one of:
#'
#' * `"during_visit"`
#' * `"during_visit_initial_24h"`
#' * `"during_visit_initial_72h`
#' * `"during_icu"`
#' * `"before_visit_initial_24h"`
#' * `"14_days_before_visit_until_visit_end"`
#' * `"year_before_visit_until_visit_end"`
#' * `"before_visit_end"`
#' * `"30_days_before_visit_start"`
#' * `"year_before_initial_24h"`
#' * `"during_value"`
#' @param names_from A character vector containing the names of columns that
#' should be used to filter by (in addition to those implied by `during`).
#' @param join Either `"left"`, in which a left join is performed (so all `x`
#'   rows will be returned, even if they have no correspoding `y`) or
#'   `"inner"`, in which an inner join is performed (so only `x` rows with at
#'   least one `y` row will be returned)
#'
#' @return
#' A data frame, with a row for each `y` measurement during the relevant
#' time period for each patient. Note that rows of the `adm` data will be
#' repeated multiple times (one for each `y` measurement data point).
#' @seealso If only a summary of the values during a timeperiod is needed,
#'   then [`summarise_during()`] will be more convenient, or the more
#'   specialised forms described at [during_functions], [adjacent_event_during],
#'   [extremes_during].
#' @examples
#' adm_data_example
#' fsheet_news2_example
#'
#' all_during(adm_data_example,
#'            fsheet_news2_example,
#'            datetime = measurement_datetime,
#'            during = "during_visit") %>%
#'   select(person_id, visit_id, symbol, datetime, value_as_number)
#' @author R.J.B. Goudie
#' @export
all_during <- function(x,
                       y,
                       datetime,
                       during,
                       names_from = "symbol",
                       join = "left"){
  y <- y %>%
    rename(datetime = {{datetime}}) %>%
    arrange(datetime)

  inform_if_all_times_are_midnight(y$datetime)

  common_cols <- intersect(colnames(x), colnames(y))
  if (!"person_id" %in% common_cols){
    stop("`person_id` not in both data frames")
  } else if (length(common_cols) > 1) {
    stop(paste0("More common columns than just `person_id` in data frames: ",
                format_as_argument(setdiff(common_cols, "person_id"))))
  }

  if (join == "left"){
    join_fn <- left_join_filter
  } else if (join == "inner"){
    join_fn <- inner_join_filter
  }
  if (during == "anytime"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          TRUE)
  } else if (during == "during_visit"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime &
                       datetime <= visit_end_datetime,
                    is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime)) %>%
      mutate(days_since_visit_start = interval(visit_start_datetime, datetime)/ddays(1))
  } else if (during == "during_visit_initial_24h"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime &
                       datetime <= visit_start_datetime + dhours(24) &
                       datetime <= visit_end_datetime,
                    is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime &
                      datetime <= visit_start_datetime + dhours(24)))
  } else if (during == "during_visit_initial_72h"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime &
                       datetime <= visit_start_datetime + dhours(72) &
                       datetime <= visit_end_datetime,
                    is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime &
                      datetime <= visit_start_datetime + dhours(72)))
  } else if (during == "during_department"){
    warning("Note that this only handle single icu_visit at the moment, at hosital visit level")
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", "department_visit_index", names_from),
        filter_condition =
          case_when(!is.na(department_end_datetime) ~
                      datetime >= department_start_datetime &
                      datetime <= department_end_datetime,
                    is.na(department_end_datetime) ~
                      datetime >= department_start_datetime))
  } else if (during == "during_department_after_48h"){
    warning("Note that this only handle single icu_visit at the moment, at hosital visit level")
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", "department_visit_index", names_from),
        filter_condition =
          case_when(!is.na(department_end_datetime) ~
                      datetime >= department_start_datetime + dhours(48) &
                      datetime <= department_end_datetime,
                    is.na(department_end_datetime) ~
                      datetime >= department_start_datetime + dhours(48)))
  } else if (during == "during_icu"){
    warning("Note that this only handle single icu_visit at the moment, at hosital visit level")
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(first_icu_level2_or_level3_end_datetime) ~
                      datetime >= first_icu_level2_or_level3_start_datetime &
                       datetime <= first_icu_level2_or_level3_end_datetime,
                    is.na(first_icu_level2_or_level3_end_datetime) ~
                      datetime >= first_icu_level2_or_level3_start_datetime))
  } else if (during == "before_visit_initial_24h"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(visit_end_datetime) ~
                      datetime <= visit_start_datetime + dhours(24) &
                       datetime <= visit_end_datetime,
                    is.na(visit_end_datetime) ~
                      datetime <= visit_start_datetime + dhours(24)))
  } else if (during == "14_days_before_visit_until_visit_end"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime - ddays(14) &
                       datetime <= visit_end_datetime,
                    is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime - ddays(14)))
  } else if (during == "year_before_visit_until_visit_end"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime - dyears(1) &
                      datetime <= visit_end_datetime,
                    is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime - dyears(1)))
  } else if (during == "before_visit_end"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(visit_end_datetime) ~
                      datetime <= visit_end_datetime,
                    is.na(visit_end_datetime) ~
                      TRUE))
  } else if (during == "30_days_before_visit_start"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime - ddays(30) &
                       datetime <= visit_start_datetime,
                    is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime - ddays(30) &
                      datetime <= visit_start_datetime))
  } else if (during == "year_before_initial_24h"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime - dyears(1) &
                       datetime <= visit_start_datetime + dhours(24) &
                       datetime <= visit_end_datetime,
                    is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime - dyears(1) &
                      datetime <= visit_start_datetime + dhours(24)))
  } else if (during == "during_value"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(value_end_datetime) ~
                      datetime >= value_start_datetime &
                      datetime <= value_end_datetime,
                    is.na(value_end_datetime) ~
                      datetime >= value_start_datetime))
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
#' @noRd
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
#' @noRd
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
