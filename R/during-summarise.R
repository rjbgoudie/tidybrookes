#' Summarise (standardised) data during a time period
#'
#' Left joins an `adm` data frame with another data frame (with a
#' `person_id` for linking and with the relevant datetime called `datetime`),
#' and filter to only those rows of `y` with `datetime` within the specified
#' time period.
#'
#' @inheritParams all_during
#' @param names_from,values_from A pair of arguments describing which column
#' (or columns) to get the name of the output column (`group_by`), and which
#' column (or columns) to get the cell values from (`values_from`).
#' @param names_suffix A character string for naming the summarised output
#'
#' @return
#' A data frame, with a row for each `y` measurement during the relevant
#' time period for each patient. Note that rows of the `adm` data will be
#' repeated multiple times (one for each `y` measurement data point).
#' @author R.J.B. Goudie
#' @examples
#' adm_data_example
#' fsheet_news2_example
#'
#' # An alternative re-implemention a version of max_during()
#' summarise_during(adm_data_example,
#'                  fsheet_news2_example,
#'                  datetime = measurement_datetime,
#'                  during = "during_visit",
#'                  type = "summarise",
#'                  formula =
#'                    tibble(value_as_number = max(value_as_number),
#'                           type = "numeric"),
#'                  names_suffix = "mymax") %>%
#'   select(person_id, visit_id, contains("mymax"))
#' @export
summarise_during <- function(x,
                             y,
                             datetime,
                             during,
                             type = "none",
                             formula,
                             names_from = "symbol",
                             group_by = c(c("person_id", "visit_id"),
                                          names_from),
                             values_from = c("value_as_number",
                                             "value_as_character",
                                             "value_as_logical",
                                             "datetime"),
                             names_suffix){
  cli::cli_progress_step("Extracting all during")
  out <- x %>%
    all_during(y,
               datetime = {{ datetime }},
               during = during,
               names_from = names_from,
               join = "inner",
               arrange = FALSE) |>
    rename(datetime = {{ datetime }})

  cli::cli_progress_step("Summarising/slicing")
  out <- out %>%
    grouped_summarise_or_slice(
      type = type,
      formula = {{ formula }},
      group_by = group_by) |>
    ungroup()

  cli::cli_progress_step("Pivoting wider")
  out <- pivot_value_wider_spec(
      out,
      y,
      id_cols = setdiff(group_by, names_from),
      values_from = values_from,
      names_suffix = glue("{names_suffix}_{during}")
  )

  # join again to x on common columns, so that anyone WITHOUT a y is included
  by <- intersect(colnames(x), colnames(out))
  left_join(x, out, by = by)
}

pivot_value_wider_spec <- function(x,
                                   y,
                                   id_cols,
                                   values_from,
                                   names_suffix){
  # Prefix for new column names to allow identification of new columns
  magic_prefix <- "__new__"

  if (!is.null(names_suffix)){
    names_suffix <- paste0("_", names_suffix)
  }

  # only include datetime if it is in the data frame
  # this handles cases like "ever_during" where no datetime is available
  values_from <- if ("datetime" %in% colnames(x)){
    values_from
  } else {
    setdiff(values_from, "datetime")
  }

  cli::cli_progress_step("Identifying symbol-type pairs")
  distinct_symbol_type <- y |>
    ungroup() |>
    select(symbol, type) |>
    distinct() |>
    collect()

  if (any(is.na(distinct_symbol_type$type))){
    cli::cli_abort("Not all `type` are specified")
  }

  spec <- distinct_symbol_type |>
    mutate(
      values_from =
        list(case_when(
          type == "numeric" ~ c("value_as_number", "datetime"),
          type == "character" ~ c("value_as_character", "datetime"),
          type == "logical" ~ c("value_as_logical", "datetime")
        )),
      .by = symbol) |>
    tidyr::unnest(values_from) |>
    mutate(.name = glue("{magic_prefix}{symbol}{names_suffix}_{values_from}"),
           .value = values_from) |>
    select(-values_from, -type)

  cli::cli_progress_step("Pivoting wider")
  if (inherits(x, "tbl_sql")){
    x <- dbplyr::dbplyr_pivot_wider_spec(x, spec, id_cols = all_of(id_cols))
  } else {
    x <- tidyr::pivot_wider_spec(x, spec, id_cols = all_of(id_cols))
  }

  cli::cli_progress_step("Relocating columns")
  relocate_and_clean_new_cols(x, magic_prefix = magic_prefix)

}

#' Summarise or slice grouped data
#'
#' Summarise (or slice if `type = "slice"`) data grouped by
#' `group_by`, using the supplied `formula`.
#'
#' @param x A data frame
#' @param type Either `"summarise"` or `"slice"`
#' @param formula An expression describing the summarise or slice
#' @param group_by A character vector listing columns that should be used
#'   to group the summarise or slice
#' @return
#' A summarised (or sliced) data frame, usually with one row per group
#' @author R.J.B. Goudie
#' @noRd
grouped_summarise_or_slice <- function(x,
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
  } else if (type == "slice_min"){
    out <- x %>%
      slice_min(!! formula)
  } else if (type == "summarise"){
    out <- x %>%
      summarise(!! formula)
  } else if (type == "mutate"){
    # for debugging
    out <- x %>%
      mutate(output = !! formula)
  }
}

#' Tidy pivot_wider dataframe column names and arrangement
#'
#' Moves new columns to the end, and sorts them (so that values and datetimes
#' are adjacent). The suffixes `_value_as_number`, `_value_as_character` and
#' `_value_as_logical` are also stripped to provide nicer column names
#'
#' @param x A data frame
#' @param magic_prefix A character string, providing the prefix that indicates
#'   which columns are "new"
#' @return
#' The data frame `x`, which renamed and reorder columns
#' @noRd
relocate_and_clean_new_cols <- function(x, magic_prefix){
  colnames_strip_new <- function(colname){
    case_when(str_starts(colname, magic_prefix) ~
                str_remove(colname, glue("^", magic_prefix)),
              TRUE ~ colname)
  }

  # remove these default names to produce cleaner column names
  suffix_to_remove_number <- "_value_as_number"
  suffix_to_remove_character <- "_value_as_character"
  suffix_to_remove_logical <- "_value_as_logical"

  x %>%
    select(!starts_with(magic_prefix), sort(colnames(.))) %>%
    rename_with(.fn = function(colname){
      case_when(str_ends(colname, suffix_to_remove_character) ~
                  str_remove(colname, glue("{suffix_to_remove_character}$")),
                str_ends(colname, suffix_to_remove_number) ~
                  str_remove(colname, glue("{suffix_to_remove_number}$")),
                str_ends(colname, suffix_to_remove_logical) ~
                  str_remove(colname, glue("{suffix_to_remove_logical}$")),
                TRUE ~ colname)
    }) %>%
    rename_with(.fn = colnames_strip_new)
}
