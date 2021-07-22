#' Inner join according to a filter and summarise in one
#'
#' @param x A data frame
#' @param y A data frame
#' @param join_by A character vector of variables to join `x` and `y` by
#' @param filter_by A character vector indicating the grouping to use when
#'   filtering
#' @param filter_condition An expression that indicates whether the
#'   corresponding row should be included
#' @param type Either `"slice"`, `"summarise`, or `"none` (for to return all
#'   values)
#' @param formula An expression for slicing or summarising
#'
#' @return A data frame, with all rows of `x` (potentially duplicated) with at
#' least one matching `y`
#' row, where the match is determined by joining by `join_by` and then
#' filtering using the `filter_condition`, under the `filter_by` grouping.
#'
#' Where `type` is `"none"` all matched rows in `y`.
#' Where `type` is `"slice`, rows satifying the `formula` under `slice` are
#' returned
#' Where `type` is `"summarise`, a new `summary` column is returned
#' @author R.J.B. Goudie
inner_join_filter <- function(x,
                              y,
                              join_by,
                              filter_by,
                              filter_condition,
                              type,
                              formula){
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

  if (type == "slice"){
    out <- out %>%
      slice(!! formula)
  } else if (type == "summarise"){
    out <- out %>%
      summarise(!! formula)
  } else if (type == "none"){
    out <- out
  }
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
#' Where `type` is `"none"` all matched rows in `y`.
#' Where `type` is `"slice`, rows satifying the `formula` under `slice` are
#' returned
#' Where `type` is `"summarise`, a new `summary` column is returned
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
#' @param names_from,values_from A pair of arguments describing which column
#' (or columns) to get the name of the output column (`names_from`), and which
#' column (or columns) to get the cell values from (`values_from`).
#' @param values_fn_name A character string for naming the output of `values_fn`
#'
#' @return A data frame
#' @author R.J.B. Goudie
inner_join_filter_pivot <- function(x,
                                    y,
                                    join_by,
                                    filter_by,
                                    filter_condition,
                                    type = "none",
                                    formula,
                                    pivot_by,
                                    names_from,
                                    values_from,
                                    values_fn_name = NULL){
  filter_condition <- enquo(filter_condition)
  formula <- enquo(formula)
  out <- inner_join_filter(
    x = x,
    y = y,
    join_by = join_by,
    filter_by = filter_by,
    filter_condition = !! filter_condition,
    type = type,
    formula = !! formula)

  if (!is.null(values_fn_name)){
    values_fn_name <- paste0("_", values_fn_name)
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
      names_from = all_of(names_from),
      values_from = all_of(values_from),
      names_glue = paste0("__new__{",
                          names_from, "}",
                          values_fn_name,
                          "_{.value}")
    ) %>%
    rename_with(.fn = colnames_strip_value) %>%
    select(!starts_with("__new__"), sort(colnames(.))) %>%
    rename_with(.fn = colnames_strip_new)
}

#' @inheritParams inner_join_filter_pivot
left_join_filter_pivot <- function(x, ...){
  out <- inner_join_filter_pivot(x = x, ...)
  # join again to x on common columns, so that anyone WITHOUT a y is included
  by <- intersect(colnames(x), colnames(out))
  left_join(x, out, by = by)
}


summarise_during <- function(x,
                             y,
                             during,
                             type = "none",
                             formula,
                             summary_name,
                             names_from,
                             values_from,
                             values_fn_name = NULL){
  formula <- enquo(formula)
  if (during == "during_icu"){
    out <- x %>%
      left_join_filter_pivot(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", "icu_visit_id", names_from),
        filter_condition =
          case_when(!is.na(icu_end_datetime) ~
                      datetime >= icu_start_datetime &
                       datetime <= icu_end_datetime,
                    is.na(icu_end_datetime) ~
                      datetime >= icu_start_datetime),
        type = type,
        formula = !! formula,
        pivot_by = c("person_id", "visit_id"),
        names_from = names_from,
        values_from = values_from,
        values_fn_name = values_fn_name)
  } else if (during == "during_visit"){
    out <- x %>%
      left_join_filter_pivot(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime &
                       datetime <= visit_end_datetime,
                    is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime),
        type = type,
        formula = !! formula,
        pivot_by = c("person_id", "visit_id"),
        names_from = names_from,
        values_from = values_from,
        values_fn_name = values_fn_name)
  } else if (during == "before_end_visit"){
    out <- x %>%
      left_join_filter_pivot(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(visit_end_datetime) ~
                      datetime <= visit_end_datetime,
                    is.na(visit_end_datetime) ~
                      TRUE),
        type = type,
        formula = !! formula,
        pivot_by = c("person_id", "visit_id"),
        names_from = names_from,
        values_from = values_from,
        values_fn_name = values_fn_name)
  } else if (during == "before_end_icu"){
    out <- x %>%
      left_join_filter_pivot(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", "icu_visit_id", names_from),
        filter_condition =
          case_when(!is.na(icu_end_datetime) ~
                      datetime <= icu_end_datetime,
                    is.na(icu_end_datetime) ~
                      TRUE),
        type = type,
        formula = !! formula,
        pivot_by = c("person_id", "visit_id"),
        names_from = names_from,
        values_from = values_from,
        values_fn_name = values_fn_name)
  }
  out
}

# TODO buffer = "date"

all_during <- function(x,
                       y,
                       during,
                       names_from){
  if (during == "during_icu"){
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
                      datetime >= icu_start_datetime),
        type = "none")
  } else if (during == "during_visit"){
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
                      datetime >= visit_start_datetime),
        type = "none")
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

fsheet_all_during <- function(x, y, during){
  y_standardised <- normalise_dataframe(y, type = "fsheet")

  all_during(x = x,
             y = y_standardised,
             during = during,
             names_from = "symbol")
}

fsheet_summarise_during <- function(x,
                                    y,
                                    during,
                                    type,
                                    formula,
                                    summary_name = ""){
  formula <- enquo(formula)
  y_standardised <- normalise_dataframe(y, type = "fsheet")

  summarise_during(x = x,
                   y = y_standardised,
                   during = during,
                   type = type,
                   formula = !! formula,
                   summary_name = glue("{summary_name}_{during}"),
                   names_from = "symbol",
                   values_from = c("value", "datetime"),
                   values_fn_name = glue("{summary_name}_{during}"))
}

fsheet_first_during <- function(...){
  fsheet_summarise_during(...,
                          type = "slice",
                          formula = which.min(datetime),
                          summary_name = "first")
}

fsheet_last_during <- function(...){
  fsheet_summarise_during(...,
                          type = "slice",
                          formula = which.max(datetime),
                          summary_name = "last")
}
