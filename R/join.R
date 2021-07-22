#' Inner join according to a filter and summarise in one
#'
#' Inner join two data frames according to some columns (`join_by`), but then
#' remove some of the rows according to the `filter_condition` (which is run
#' after grouping via `filter_by`).
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
#' @param x A data frame
#' @param y A data frame
#' @param join_by A character vector of variables to join `x` and `y` by
#' @param filter_by A character vector indicating the grouping to use when
#'   filtering
#' @param filter_condition An expression that indicates whether the
#'   corresponding row should be included
#' @param group_by,values_from A pair of arguments describing which column
#' (or columns) to get the name of the output column (`group_by`), and which
#' column (or columns) to get the cell values from (`values_from`).
#' @param names_suffix A character string for naming the summary output
#'
#' @return A data frame
#' Where `type` is `"none"` all matched rows in `y`.
#' Where `type` is `"slice`, rows satifying the `formula` under `slice` are
#' returned
#' Where `type` is `"summarise"`, a new `summary` column is returned
#'
#' @author R.J.B. Goudie
summarise_pivot_wider <- function(x,
                                  type = "summary",
                                  formula,
                                  pivot_by,
                                  group_by,
                                  values_from,
                                  names_suffix = NULL){
  formula <- enquo(formula)

  group_and_pivot_by <- c(pivot_by, group_by)
  group_and_pivot_by <- as.list(group_and_pivot_by)
  group_and_pivot_by_symbol <- rlang::syms(group_and_pivot_by)

  x <- x %>%
    group_by(!!! group_and_pivot_by_symbol)

  if (type == "slice"){
    out <- x %>%
      slice(!! formula)
  } else if (type == "summarise"){
    out <- x %>%
      summarise(!! formula)
  }

  if (!is.null(names_suffix)){
    names_suffix <- paste0("_", names_suffix)
  }

  colnames_strip_new <- function(colname){
    case_when(str_starts(colname, "__new__") ~ str_remove(colname, "^__new__"),
              TRUE ~ colname)
  }
  colnames_strip_value <- function(colname){
    case_when(str_ends(colname, "_value") ~ str_remove(colname, "_value$"),
              TRUE ~ colname)
  }

  out <- out %>%
    pivot_wider(
      id_cols = all_of(pivot_by),
      names_from = all_of(group_by),
      values_from = all_of(values_from),
      names_glue = paste0("__new__{",
                          group_by, "}",
                          names_suffix,
                          "_{.value}")
    ) %>%
    rename_with(.fn = colnames_strip_value) %>%
    select(!starts_with("__new__"), sort(colnames(.))) %>%
    rename_with(.fn = colnames_strip_new)
}

#' Summarise (standardised) data during a time period
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
#'
#' @return
#' A data frame, with a row for each `y` measurement during the relevant
#' time period for each patient. Note that rows of the `adm` data will be
#' repeated multiple times (one for each `y` measurement data point).
#' @author R.J.B. Goudie
summarise_during <- function(x,
                             y,
                             during,
                             type = "none",
                             formula,
                             group_by,
                             values_from,
                             names_suffix){
  formula <- enquo(formula)
  out <- x %>%
    all_during(y,
               during = during,
               group_by = group_by)

  out <- out %>%
    summarise_pivot_wider(type = type,
                          formula = !! formula,
                          pivot_by = c("person_id", "visit_id"),
                          group_by = group_by,
                          values_from = values_from,
                          names_suffix = names_suffix)
  out
}

# TODO buffer = "date"

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
#'
#' @return
#' A data frame, with a row for each `y` measurement during the relevant
#' time period for each patient. Note that rows of the `adm` data will be
#' repeated multiple times (one for each `y` measurement data point).
#' @author R.J.B. Goudie
all_during <- function(x,
                       y,
                       during,
                       group_by){
  if (during == "during_icu"){
    out <- x %>%
      left_join_filter(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", "icu_visit_id", group_by),
        filter_condition =
          case_when(!is.na(icu_end_datetime) ~
                      datetime >= icu_start_datetime &
                       datetime <= icu_end_datetime,
                    is.na(icu_end_datetime) ~
                      datetime >= icu_start_datetime))
  } else if (during == "during_visit"){
    out <- x %>%
      left_join_filter(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", group_by),
        filter_condition =
          case_when(!is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime &
                       datetime <= visit_end_datetime,
                    is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime))
  } else {
    stop("unexpected during value")
  }
  out
}

normalise_dataframe <- function(x, type = "fsheet"){
  if (type == "fsheet"){
    x <- x %>%
      select(person_id = person_id,
             symbol = symbol,
             value = value_as_number, # TODO make generic
             datetime = measurement_datetime)
  }
  x %>%
    arrange(datetime)
}

#' Left join with fsheet data
#'
#' @param x An `adm` data frame
#' @param y A (tidy) `fsheet` data frame: with `person_id`, `symbol`,
#'   `value_as_number` (TODO) and `measurement_datetime`.
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
             during = during,
             group_by = "symbol")
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
                   group_by = "symbol",
                   values_from = c("value", "datetime"),
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
