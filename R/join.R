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

summarise_or_pivot <- function(x,
                               type = "summarise",
                               formula,
                               group_by){
  formula <- enquo(formula)

  group_by <- as.list(group_by)
  group_by_symbol <- rlang::syms(group_by)

  x <- x %>%
    group_by(!!! group_by_symbol)

  if (type == "slice"){
    out <- x %>%
      slice(!! formula)
  } else if (type == "summarise"){
    out <- x %>%
      summarise(!! formula)
  }
}

split_data_by_type <- function(x){
  has_value_as_number_col <- "value_as_number" %in% colnames(x)
  has_value_as_character_col <- "value_as_character" %in% colnames(x)
  has_type_col <- "type" %in% colnames(x)

  clearly_specifies_types <-
    has_value_as_number_col & has_value_as_character_col & has_type_col
  has_only_value_as_number_col <-
    has_value_as_number_col & !has_value_as_character_col
  has_only_value_as_character_col <-
    !has_value_as_number_col & has_value_as_character_col

  if (clearly_specifies_types){
    x_numeric <- x %>%
      filter(type == "numeric")
    x_character <- x %>%
      filter(type == "character")
    any_numeric <- nrow(x_numeric) > 0
    any_character <- nrow(x_character) > 0
  } else if (has_only_value_as_number_col){
    any_numeric <- TRUE
    any_character <- FALSE
    x_numeric <- x
    x_character <- NULL
  } else if (has_only_value_as_character_col){
    any_numeric <- FALSE
    any_character <- TRUE
    x_character <- x
    x_numeric <- NULL
  } else {
    stop("unexpected2")
  }
  list(any_numeric = any_numeric,
       any_character = any_character,
       numeric = x_numeric,
       character = x_character)
}

#' Summarise (or slice) data then pivot data wider
#'
#' Summarise (or slice if `type = "summarise"`) data grouped by
#' `c(id_cols, names_from)`, using the supplied `formula`.
#'
#' The pivot the data wider, so that `names_from` appears in columns, with
#' the values taken from `values_from`
#'
#' The new columns are sorted alphabetically (so that, for example, `x` and
#' `x_datetime` are adjacent), and column names ending in `_value` are
#' shortened
#'
#' @param x A data frame
#' @param type Either `"summarise"` or `"slice"`
#' @param formula A formula specifying the summarise or slice to perform
#' @param id_cols A set of columns that uniquely identifies each observation.
#' @param names_from,values_from A pair of arguments describing which column
#' (or columns) to get the name of the output column (`group_by`), and which
#' column (or columns) to get the cell values from (`values_from`).
#' @param names_suffix A character string for naming the summarised output
#'
#' @return A data frame
#'
#' @author R.J.B. Goudie
  summarise_pivot_wider <- function(x,
                                    type = "summarise",
                                    formula,
                                    id_cols,
                                    names_from,
                                    values_from,
                                    names_suffix = NULL){
    formula <- enquo(formula)
    out <- summarise_or_pivot(x,
                              type = type,
                             formula = !! formula,
                             group_by = c(id_cols, names_from))

    # magic string to add to the start of new column names to allow identification
    # of new columns
    magic_prefix <- "__new__"

    suffix_to_remove_number <- "_value_as_number"
    suffix_to_remove_character <- "_value_as_character"

    if (!is.null(names_suffix)){
      names_suffix <- paste0("_", names_suffix)
    }

    colnames_strip_new <- function(colname){
      case_when(str_starts(colname, magic_prefix) ~
                  str_remove(colname, glue("^", magic_prefix)),
                TRUE ~ colname)
    }

    colnames_strip_suffix_to_remove <- function(colname, suffix){
      case_when(str_ends(colname, suffix) ~
                  str_remove(colname, glue("{suffix}$")),
                TRUE ~ colname)
    }

    out_split <- split_data_by_type(out)

    if (out_split$any_numeric){
      out_numeric <- out_split$numeric %>%
        pivot_wider(
          id_cols = all_of(id_cols),
          names_from = all_of(names_from),
          values_from = all_of(setdiff(values_from, "value_as_character")),
          names_glue = paste0(magic_prefix,  "{",
                              names_from, "}",
                              names_suffix,
                              "_{.value}")) %>%
        rename_with(.fn = function(x) colnames_strip_suffix_to_remove(x, suffix_to_remove_number))
    }
    if (out_split$any_character){
      out_character <- out_split$character %>%
        pivot_wider(
          id_cols = all_of(id_cols),
          names_from = all_of(names_from),
          values_from = all_of(setdiff(values_from, "values_as_number")),
          names_glue = paste0(magic_prefix,  "{",
                              names_from, "}",
                              names_suffix,
                              "_{.value}")) %>%
        rename_with(.fn = function(x) colnames_strip_suffix_to_remove(x, suffix_to_remove_character))
    }

    if (out_split$any_numeric & out_split$any_character){
      out <- full_join(out_numeric, out_character)
    } else if (out_split$any_numeric){
      out <- out_numeric
    } else if (out_split$any_character){
      out <- out_character
    } else {
      stop("Neither numeric or character output from slice/summary found")
    }
    out  %>%
      select(!starts_with(magic_prefix), sort(colnames(.))) %>%
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
                             names_from,
                             values_from,
                             names_suffix){
  formula <- enquo(formula)
  out <- x %>%
    all_during(y,
               during = during,
               names_from = names_from)

  out <- out %>%
    summarise_pivot_wider(type = type,
                          formula = !! formula,
                          id_cols = c("person_id", "visit_id"),
                          names_from = names_from,
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
                      datetime >= icu_start_datetime))
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
             value_as_number = value_as_number,
             value_as_character = value_as_character,
             type = type,
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
             names_from = "symbol")
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
                   names_from = "symbol",
                   values_from = c("value_as_number",
                                   "value_as_character",
                                   "datetime"),
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
