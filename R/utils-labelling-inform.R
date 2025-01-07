update_exclude_labels <- function(x){
  mutate(x,
         exclude = if_any(starts_with("exclude_") & where(is_logical),
                          ~ !is.na(.) & .))
}


# Label conditions --------------------------------------------------------

#' Filter, but report the filtering that occurs
#' #'
#' #' @param x A data frame
#' #' @param ... Passed to filter
#' #' @param since A character describing the reason for the filtering
#' #' @noRd
label_condition_inform <- function(x,
                                   inform_col,
                                   condition,
                                   since = "for unknown reason"){
  x <- x |>
    mutate("{{ inform_col }}" :=
             case_when(exclude ~ as.logical(NA),
                       TRUE ~ {{ condition }}))

  # removing NA since, for example, silently_exclude_when may not handle
  # cases that are already handled by silently_exclude_na_when
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

exclusion_label_condition_inform <- function(...){
  label_condition_inform(...) %>%
    update_exclude_labels()
}


# Duplicate rows ----------------------------------------------------------

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
exclusion_label_duplicates_inform <- function(...){
  label_duplicates_inform(...) %>%
    mutate(exclude_is_duplicate = is_duplicate,
           exclude_is_duplicate_of_row_number = is_duplicate_of) %>%
    select(-is_duplicate,
           -is_duplicate_of) %>%
    update_exclude_labels()
}

# General functions -------------------------------------------------------

label_fn_inform <- function(x,
                            fn,
                            ...,
                            inform_col,
                            since = "for unknown reason"){
  inform_col <- enquo(inform_col)

  rows_to_consider <- !is.na(x$exclude) & !x$exclude
  x[rows_to_consider, ] <- fn(x[rows_to_consider, ], ...)
  x <- mutate(x,
              "{{ inform_col }}" :=
                case_when(exclude ~ as.logical(NA),
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

exclusion_label_fn_inform <- function(...){
  label_fn_inform(...) %>%
    update_exclude_labels()
}

# Check that all ----------------------------------------------------------

label_check_that_all <- function(x,
                                 condition,
                                 label,
                                 summary = identity){
  condition <- enquo(condition)
  col <- rlang::englue("satisfies_{label}")
  x <- x %>%
    mutate("satisfies_{label}" :=
             case_when(exclude ~ as.logical(NA),
                       TRUE ~ !!condition))
  unexpected_nrow <- x %>%
    filter(!if_any(all_of(col))) %>%
    nrow

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
