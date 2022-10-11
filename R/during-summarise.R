#' Summarise (standardised) data during a time period
#'
#' Left joins an `adm` data frame with another data frame (with a
#' `person_id` for linking and with the relevant datetime called `datetime`),
#' and filter to only those rows of `y` with `datetime` within the specified
#' time period.
#'
#' @param x An `adm` data frame
#' @param y A (tidy) data frame
#' @param datetime The column of `y` to use as the main datetime for matching
#' @param during The time period to extract data for, one of: `"during_visit"`,
#'   `"during_icu"`
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
summarise_during <- function(x,
                             y,
                             datetime,
                             during,
                             type = "none",
                             formula,
                             names_from = "symbol",
                             values_from = c("value_as_number",
                                             "value_as_character",
                                             "value_as_logical",
                                             "datetime"),
                             names_suffix){
  formula <- enquo(formula)
  datetime <- enquo(datetime)
  out <- x %>%
    all_during(y,
               datetime = !! datetime,
               during = during,
               names_from = names_from,
               join = "inner")

  out <- out %>%
    grouped_summarise_or_slice(
      type = type,
      formula = !! formula,
      group_by = c(c("person_id", "visit_id"), names_from)) %>%
    pivot_value_wider(id_cols = c("person_id", "visit_id"),
                      names_from = names_from,
                      values_from = values_from,
                      names_suffix = glue("{names_suffix}_{during}"))

  # join again to x on common columns, so that anyone WITHOUT a y is included
  by <- intersect(colnames(x), colnames(out))
  left_join(x, out, by = by)
}

#' Pivot data wider, with multiple value type
#'
#' Pivot long into a wider format, so that `names_from` appears in columns.
#' The function handles long format with a mix of `value_as_numeric`,
#' `value_as_character` and `value_as_logical` value columns.
#'
#' New columns are added to the end, are sorted (so that values and datetimes
#' are adjacent). The suffixes `_value_as_number` and `_value_as_character` are
#' also stripped to provide nicer column names.
#'
#' @param x A data frame
#' @param id_cols A set of columns that uniquely identifies each observation.
#' @param names_from,values_from A pair of arguments describing which column
#' (or columns) to get the name of the output column (`group_by`), and which
#' column (or columns) to get the cell values from (`values_from`).
#' @param names_suffix A character string for naming the summarised output
#'
#' @return A data frame
#'
#' @author R.J.B. Goudie
pivot_value_wider <- function(x,
                              id_cols = c("person_id", "visit_id"),
                              names_from = "symbol",
                              values_from = c("value_as_number",
                                              "value_as_character",
                                              "value_as_logical"),
                              names_suffix = NULL){
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

  names_glue <- paste0(magic_prefix,  "{",
                       names_from, "}",
                       names_suffix,
                       "_{.value}")

  x_split <- split_by_type(x)
  anything <-
    x_split$any_numeric ||
    x_split$any_character ||
    x_split$any_logical

  x_numeric <- NULL
  x_character <- NULL
  x_logical <- NULL

  if (x_split$any_numeric){
    values_from_numeric <- setdiff(values_from,
                                   c("value_as_character", "value_as_logical"))
    x_numeric <- x_split$numeric %>%
      pivot_wider(
        id_cols = all_of(id_cols),
        names_from = all_of(names_from),
        values_from = all_of(values_from_numeric),
        names_glue = names_glue)
  }

  if (x_split$any_character){
    values_from_character <- setdiff(values_from,
                                     c("value_as_number", "value_as_logical"))
    x_character <- x_split$character %>%
      pivot_wider(
        id_cols = all_of(id_cols),
        names_from = all_of(names_from),
        values_from = all_of(values_from_character),
        names_glue = names_glue)
  }

  if (x_split$any_logical){
    values_from_logical <- setdiff(values_from,
                                   c("value_as_number", "value_as_character"))
    x_logical <- x_split$logical %>%
      pivot_wider(
        id_cols = all_of(id_cols),
        names_from = all_of(names_from),
        values_from = all_of(values_from_logical),
        names_glue = names_glue)
  }

  if (anything){
    x <- list(x_numeric, x_character, x_logical) %>%
      compact %>%
      reduce(full_join)
  } else {
    stop("Neither numeric or character output from slice/summary found")
  }

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
  } else if (type == "summarise"){
    out <- x %>%
      summarise(!! formula)
  } else if (type == "mutate"){
    # for debugging
    out <- x %>%
      mutate(output = !! formula)
  }
}

#' Split a data frame into number, character and logical value rows
#'
#' Determine whether to use the `value_as_number`, `value_as_character` or
#' `value_as_logical` column for each row of a data frame.
#'
#' - Where a suitable `type` col is present, this is used to split the rows
#' - Where only a `value_as_number` column is present (no `value_as_character`
#'   or `value_as_logical` are present), then all rows are assumed numeric
#' - Similar when only a `value_as_character` or `value_as_logical` column
#'   are present
#'
#' @param x A data frame
#'
#' @return
#' A list with 4 components:
#' - `any_numeric` a logical indicating whether any rows are numbers
#' - `any_character` a logical indicating whether any rows are characters
#' - `any_logical` a logical indicating whether any rows are logical
#' - `numeric` A data frame containing numeric rows
#' - `character` A data frame containing character rows
#' - `logical` A data frame containing logical rows
split_by_type <- function(x){
  has_value_as_number_col <- "value_as_number" %in% colnames(x)
  has_value_as_character_col <- "value_as_character" %in% colnames(x)
  has_value_as_logical_col <- "value_as_logical" %in% colnames(x)
  has_type_col <- "type" %in% colnames(x)

  if (has_type_col){
    possible_types <- c("numeric", "character", "logical")
    type_col_is_possible <- all(unique(x$type) %in% possible_types)
  }

  clearly_specifies_types <-
    has_type_col && type_col_is_possible
  has_only_value_as_number_col <-
    has_value_as_number_col &
    !has_value_as_character_col &
    !has_value_as_logical_col
  has_only_value_as_character_col <-
    !has_value_as_number_col &
    has_value_as_character_col &
    !has_value_as_logical_col
  has_only_value_as_logical_col <-
    !has_value_as_number_col &
    !has_value_as_character_col &
    has_value_as_logical_col

  if (clearly_specifies_types){
    x_numeric <- x %>%
      filter(type == "numeric")
    x_character <- x %>%
      filter(type == "character")
    x_logical <- x %>%
      filter(type == "logical")
    any_numeric <- nrow(x_numeric) > 0
    any_character <- nrow(x_character) > 0
    any_logical <- nrow(x_logical) > 0
  } else if (has_only_value_as_number_col){
    any_numeric <- TRUE
    any_character <- FALSE
    any_logical <- FALSE
    x_numeric <- x
    x_character <- NULL
    x_logical <- NULL
  } else if (has_only_value_as_character_col){
    any_numeric <- FALSE
    any_character <- TRUE
    any_logical <- FALSE
    x_character <- x
    x_numeric <- NULL
    x_logical <- NULL
  } else if (has_only_value_as_logical_col){
    any_numeric <- FALSE
    any_character <- FALSE
    any_logical <- TRUE
    x_character <- NULL
    x_logical <- x
    x_numeric <- NULL
  } else {
    stop("Could not establish whether to use numeric or character values")
  }
  list(any_numeric = any_numeric,
       any_character = any_character,
       any_logical = any_logical,
       numeric = x_numeric,
       character = x_character,
       logical = x_logical)
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
