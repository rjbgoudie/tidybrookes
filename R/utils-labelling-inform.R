#' Summarise current exclusion
#'
#' Update the `exclude` column to be `TRUE` if any columns starting `exclude_`
#' are `TRUE`.
#' @param x A data frame
#' @noRd
update_exclude_labels <- function(x){
  mutate(x,
         exclude = if_any(starts_with("exclude_") & where(is_logical),
                          ~ !is.na(.) & .))
}

# Label conditions --------------------------------------------------------

#' Label rows satisfying a condition and report the labelling that occurs
#'
#' @param x A data frame
#' @param inform_col A label for the column
#' @param condition A condition
#' @param since A character describing the reason for the label
#' @noRd
label_condition_inform <- function(x,
                                   inform_col,
                                   condition,
                                   since = "for unknown reason"){
  x <- x |>
    mutate("{{ inform_col }}" := {{ condition }})

  n_labelled <- x %>%
    pull({{ inform_col }}) %>%
    sum(na.rm = TRUE)

  if (n_labelled > 0){
    sign <- "labelled"
    change_abs <- n_labelled
    s <- if_else(change_abs == 1, "", "s")
    cli::cli_alert_info("{change_abs} row{s} {sign} {since}")
  }
  x
}

#' Label rows that satify an exclusion reason, and report the labelling
#'
#' The condition is modified to ensure rows that already have `exclude = TRUE`
#' are not considered. The `exclude` column is updated afterwards.
#'
#' @param x A data frame
#' @param inform_col A label for the column
#' @param condition A condition
#' @param ... Passed to `label_condition_inform()`
#' @noRd
exclusion_label_condition_inform <- function(x,
                                             inform_col,
                                             condition,
                                             ...){
  label_condition_inform(x,
                         inform_col = {{ inform_col }},
                         condition = case_when(exclude ~ as.logical(NA),
                                              TRUE ~ {{ condition }}),
                         ...) %>%
    update_exclude_labels()
}


# Duplicate rows ----------------------------------------------------------

#' Label duplicate rows and report the labelling that occurs
#'
#' @param x A data frame
#' @param ignore_columns Columns that should be excluded prior to considering
#' which rows are duplicates
#' @noRd
label_duplicates_inform <- function(x, ignore_columns){
  x <- x |>
    mutate(is_duplicate_of = vctrs::vec_duplicate_id(pick(-{{ ignore_columns }}))) |>
    mutate(is_duplicate = is_duplicate_of != row_number(),
           .before = is_duplicate_of)

  n_duplicates <- sum(x$is_duplicate)
  if (n_duplicates > 0){
    since <- "as exact duplicate"
    sign <- "labelled"
    change_abs <- n_duplicates
    s <- if_else(change_abs == 1, "", "s")
    cli::cli_alert_info("{change_abs} row{s} {sign} {since}{s}")
  }
  x
}

# TODO does this handle already excluded rows reasonably-enough?

#' Label duplicate rows and report the labelling
#'
#' The labels are called `exclude_is_duplate` and
#' `exclude_is_duplicate_of_row_number`.
#' The `exclude` column is updated afterwards.
#'
#' @param ... Passed to `label_duplicates_inform()`
#' @noRd
exclusion_label_duplicates_inform <- function(...){
  label_duplicates_inform(...) %>%
    mutate(exclude_is_duplicate = is_duplicate,
           exclude_is_duplicate_of_row_number = is_duplicate_of) %>%
    select(-is_duplicate,
           -is_duplicate_of) %>%
    update_exclude_labels()
}

# General functions -------------------------------------------------------

#' Label rows using a applied function to a data frame and report
#'
#' @param x A data frame
#' @param fn A function
#' @param ... Arguments to the function `fn`
#' @param rows_to_consider A logical vector, specifying which rows should
#' considered/included when applying the function `fn`
#' @param since A character describing the reason for the labelling
#' @noRd
label_fn_inform <- function(x,
                            fn,
                            ...,
                            inform_col,
                            rows_to_consider = TRUE,
                            since = "for unknown reason"){
  inform_col <- enquo(inform_col)

  x[rows_to_consider, ] <- fn(x[rows_to_consider, ], ...)
  x <- mutate(x,
              "{{ inform_col }}" :=
                case_when(!rows_to_consider ~ as.logical(NA),
                          TRUE ~ !!inform_col))

  n_labelled <- x %>%
    pull(!! inform_col) %>%
    sum(na.rm = TRUE)
  if (n_labelled > 0){
    sign <- "labelled"
    change_abs <- n_labelled
    s <- if_else(change_abs == 1, "", "s")
    cli::cli_alert_info("{change_abs} row{s} {sign} {since}")
  }
  x
}

#' Label rows for exclusion using a applied function to a data frame and report
#'
#' Rows that already have `exclude = TRUE` will have not have `fn` applied.
#' The `exclude` column is updated afterwards.
#' @param x A data frame
#' @param fn A function
#' @param ... Arguments to the function `fn`
#' @param rows_to_consider A logical vector, specifying which rows should
#' considered/included when applying the function `fn`
#' @param since A character describing the reason for the labelling
#' @noRd
exclusion_label_fn_inform <- function(x, ...){
  rows_to_consider <- !is.na(x$exclude) & !x$exclude
  label_fn_inform(x, ..., rows_to_consider = rows_to_consider) %>%
    update_exclude_labels()
}

# Check that all ----------------------------------------------------------

#' Label all rows of a data frame satisfy a condition
#'
#' The `condition` is checked for all rows.
#' The result of the check is recorded in `satisfies_{label}`.
#' If any rows don't satisfy the condition, this is reported.
#'
#' @param x A data frame
#' @param condition A condition
#' @param label Character string, a label for the condition
#' @param summary A function that should be used to summarise unexpected
#' rows
#' @noRd
label_check_that_all <- function(x,
                                 condition,
                                 label,
                                 summary = identity){
  condition <- enquo(condition)
  col <- rlang::englue("satisfies_{label}")
  x <- x %>%
    mutate("satisfies_{label}" := !!condition)

  unexpected_nrow <- x %>%
    filter(!if_any(all_of(col))) %>%
    count() %>%
    pull(n)

  if (unexpected_nrow > 0){
    condition_str <- expr_print(condition)
    row <- if_else(unexpected_nrow == 1, "row", "rows")
    cli::cli_alert_warning(
      c("{unexpected_nrow} {row} not satisfying ",
        "{label} condition: ",
        "{condition_str}"))
  }
  x
}

#' Label rows (except those excluded) of a data frame satisfy a condition
#'
#' The `condition` is checked for all rows - except those with `exclude = TRUE`
#' already. The result of the check is recorded in `satisfies_{label}`.
#' If any rows don't satisfy the condition, this is reported.
#'
#' @param x A data frame
#' @param condition A condition
#' @param label Character string, a label for the condition
#' @param summary A function that should be used to summarise unexpected
#' rows
#' @noRd
exclusion_label_check_that_all <- function(x, condition, ...){
  label_check_that_all(x,
                       condition = case_when(exclude ~ as.logical(NA),
                                             TRUE ~ {{condition}}),
                       ...)
}
