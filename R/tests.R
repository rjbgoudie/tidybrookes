#' Tidy raw tests colnames
#' @param x A data frame of raw test data
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
tests_rename <- function(x){
  x %>%
    relocate(person_id = STUDY_SUBJECT_DIGEST,
             name = TestName,
             value = ResultValue,
             ordered_datetime = ORDERED_DATETIME,
             collected_datetime = COLLECTED_DATETIME,
             received_datetime = RECEIVED_DATETIME,
             result_datetime = ResultDate,
             ordering_department = ORDERING_DEPARTMENT_NAME,
             range_low = ReferenceLow,
             range_high = ReferenceHigh,
             unit = ResultUnit,
             method = Method,
             group = TestGroupName,
             order_id = OrderProcId)
}

#' Untidy raw tests colnames
#' @param x A data frame of raw tests data with tidy names
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
tests_unrename <- function(x){
  x %>%
    relocate(STUDY_SUBJECT_DIGEST = person_id,
             TestName = name,
             ResultValue = value,
             ORDERED_DATETIME = ordered_datetime,
             COLLECTED_DATETIME = collected_datetime,
             RECEIVED_DATETIME = received_datetime,
             ResultDate = result_datetime,
             ORDERING_DEPARTMENT_NAME = ordering_department,
             ReferenceLow = range_low,
             ReferenceHigh = range_high,
             ResultUnit = unit,
             Method = method,
             TestGroupName = group,
             OrderProcId = order_id)
}

#' Add a new definition of a test item
#'
#' @param test_def A list of existing test definitions to add this to
#' @param symbol A short name for the flowsheet item. This should be a character vector that is suitable to use as an R object name
#' @param title The full name for the test item, suitable for using in
#'   figure titles etc
#' @param names_cuh The names of the test (`TestName` in raw format) for tests
#'   to include from CUH
#' @param names_external The names of the test (`TestName` in raw format) for
#'   tests to include from external sources. NOTE CURRENTLY WE DON'T INCLUDE
#'   EXTERNAL TESTS AT ALL.
#' @param search_pattern A charater vector of potential synonymns for this
#'   test, in case new names (`TestName` in raw format) for the
#'   same test are added in future.
#' @param search_exclude A character vector of test names
#'   (`TestName` in raw format) that are known NOT to be relevant to this test.
#' @param search_exclude_group A character vector of test group names
#'   (`TestGroupName` in raw format) that are known NOT to be relevant to
#'   this test.
#' @param silently_exclude_na_when An expression that returns a logical value,
#'   specifying the circumstances when `NA` values should be automatically
#'   excluded.
#' @param silently_exclude_when An expression that returns a logical value,
#'  specifying the circumstances when values should be automatically
#'  excluded
#' @param censoring_fn A function specifying the value of the `censoring` column
#' @param value_fn A function specifying how to generate the
#'   `value` column
#' @param unit_rescale_fn A function specifying how to rescale the result
#'   values. By default, does not rescale.
#' @param unit_relabel_fn A function specifying how to relabel the units of the
#'   the result values. By default, does not rescale.
#' @param expect_before An expression that returns a logical value,
#'  specifying a condition that should be `TRUE` in the raw data
#' @param expect_after_all_numeric Should all output be numeric?
#' @param expect_after An expression that returns a logical value,
#'  specifying a condition that should be `TRUE` in the processed (output) data
#' @param range_mainly_low,range_mainly_high An indicative lower and upper range
#'   for most values. Values outside this range are NOT excluded: this is purely
#'   for setting default scales of plots etc
#' @param range_discard_below,range_discard_above This pair set the lower and #'   upper range for the flowsheet item. Values outside this range are
#'   EXCLUDED.
#' @author R.J.B. Goudie
tests_add <- function(test_def,
                      symbol,
                      title,
                      names_cuh = NA,
                      names_external = NA,
                      search_pattern,
                      search_exclude = NA,
                      search_exclude_group = NA,
                      silently_exclude_na_when = FALSE,
                      silently_exclude_when = FALSE,
                      censoring_fn = case_when(TRUE ~ NA_character_),
                      value_fn = case_when(TRUE ~ value_numeric),
                      unit_rescale_fn = case_when(TRUE ~ value),
                      unit_relabel_fn = case_when(TRUE ~ unit),
                      expect_before = TRUE,
                      expect_after_all_numeric = TRUE,
                      expect_after = TRUE,
                      range_mainly_low = NA,
                      range_mainly_high = NA,
                      range_discard_below = NA,
                      range_discard_above = NA){
  censoring_fn <- enquo(censoring_fn)
  value_fn <- enquo(value_fn)
  silently_exclude_na_when <- enquo(silently_exclude_na_when)
  silently_exclude_when <- enquo(silently_exclude_when)
  unit_rescale_fn <- enquo(unit_rescale_fn)
  unit_relabel_fn <- enquo(unit_relabel_fn)
  expect_before <- enquo(expect_before)
  expect_after <- enquo(expect_after)

  new <- list(symbol = symbol,
              title = title,
              names_cuh = names_cuh,
              names_external = names_external,
              search_pattern = search_pattern,
              search_exclude = search_exclude,
              search_exclude_group = search_exclude_group,
              silently_exclude_na_when = silently_exclude_na_when,
              silently_exclude_when = silently_exclude_when,
              censoring_fn = censoring_fn,
              value_fn = value_fn,
              unit_rescale_fn = unit_rescale_fn,
              unit_relabel_fn = unit_relabel_fn,
              expect_before = expect_before,
              expect_after_all_numeric = expect_after_all_numeric,
              expect_after = expect_after,
              range_mainly_low = range_mainly_low,
              range_mainly_high = range_mainly_high,
              range_discard_below = range_discard_below,
              range_discard_above = range_discard_above)
  test_def <- append(test_def, list(new))
  names(test_def)[length(test_def)] <- symbol
  test_def
}

#' Extract tests data into tidy format
#'
#' @param x Test data in tidy-column-name format (after applying
#'   `tests_rename`)
#' @param tests_def A tests definition
#' @param errors A function indicating what to do when an error is found
#'
#' @return
#' A data frame with the following columns:
#' `person_id`, `symbol`, `value`, `censoring`, `ordered_datetime`,
#' `collected_datetime`, `received_datetime`, `result_datetime`,
#' `ordering_department_name`, `range_low`, `range_high`, `unit`,
#' `name`, `title`, `method`, `group`
#' @author R.J.B. Goudie
tests_extract <- function(x, tests_def, errors = stop){
  if (length(tests_def) == 1 & "symbol" %in% names(tests_def)){
    tests_extract_single(x, tests_def)
  } else {
    bind_rows(lapply(tests_def, function(y){
      cat(paste0("\nExtracting ", y$title, " (", y$symbol, "): "))
      tests_extract_single(x, y, errors = errors)
    }))
  }
}

tests_extract_single <- function(x, test_def, errors = stop){
  # Filter to only CUH tests
  out <- x %>%
    filter(name %in% test_def$names_cuh)

  # Add symbol and title
  out <- out %>%
    mutate(symbol = test_def$symbol, .after = person_id) %>%
    mutate(title = test_def$title, .after = unit) %>%
    relocate(name, .after = unit)

  # Check expect_before condition
  unexpected <- out %>%
    filter(!(!!test_def$expect_before))
  if (nrow(unexpected)){
    unexpected <- unexpected %>%
      select(group, name, value, range_low, range_high, unit)
    errors("unexpected cases\n", print(unexpected))
  }

  # Remove duplicate rows
  nrow_data_original <- nrow(out)
  out <- out %>%
    distinct
  nrow_data_distinct <- nrow(out)
  if (nrow_data_original != nrow_data_distinct){
    cat(nrow_data_original - nrow_data_distinct, "duplicate rows discarded. ")
  }

  # Exclude NAs when requested
  out <- out %>%
    mutate(will_silently_exclude_na = (is.na(value) & !!test_def$silently_exclude_na_when))

  n_silently_exclude_na <- nrow(out) - sum(!out$will_silently_exclude_na)
  if (n_silently_exclude_na > 0){
    cat(n_silently_exclude_na, "NAs excluded. ")
  }

  # Exclude other rows when requested
  out <- out %>%
    filter(!will_silently_exclude_na) %>%
    mutate(will_silently_exclude = (!!test_def$silently_exclude_when))

  n_silently_exclude <- nrow(out) - sum(!out$will_silently_exclude)
  if (n_silently_exclude > 0){
    cat(n_silently_exclude, "excluded. ")
  }

  out <- out %>%
    filter(!will_silently_exclude)

  # Convert values to numeric, and handle censoring
  out <- out %>%
    group_by(name) %>%
    mutate(
      value_original = value,
      value_numeric = suppressWarnings({
        as.numeric(value)
      }),
      censoring = !!test_def$censoring_fn,
      value = !!test_def$value_fn) %>%
    relocate(censoring, .after = value) %>%
    ungroup

  # Check for nonnumeric values
  if (isTRUE(test_def$expect_after_all_numeric)){
    nonnumeric <- tests_nonnumeric(out)
    if (nrow(nonnumeric) > 0){
      errors("*** Non-numeric value found ***\n",
           print(nonnumeric %>%
                   group_by(value_original) %>%
                   select(value_original, value_numeric, value) %>%
                   distinct),
           "\n")
    }
  }

  # Rescale units
  out <- out %>%
    mutate(value = !!test_def$unit_rescale_fn,
           unit = !!test_def$unit_relabel_fn)

  # Discard too high values
  if (!is.na(test_def$range_discard_above)){
    nrow_data_prior_too_high <- nrow(out)
    out <- out %>%
      mutate(is_too_high = value > test_def$range_discard_above) %>%
      filter(!is_too_high)
    n_discard_too_high <- nrow_data_prior_too_high - nrow(out)
    if (n_discard_too_high > 0){
      cat(paste0(n_discard_too_high,
                 " excluded since >",
                 test_def$range_discard_above,
                 ". "))
    }
  } else {
    out <- out %>%
      mutate(is_too_high = FALSE)
  }

  # Discard too low values
  if (!is.na(test_def$range_discard_below)){
    nrow_data_prior_too_low <- nrow(out)
    out <- out %>%
      mutate(is_too_low = value < test_def$range_discard_below) %>%
      filter(!is_too_low)
    n_discard_too_low <- nrow_data_prior_too_low - nrow(out)
    if (n_discard_too_low > 0){
      cat(paste0(n_discard_too_low,
                 " excluded since <",
                 test_def$range_discard_below,
                 ". "))
    }
  } else {
    out <- out %>%
      mutate(is_too_low = FALSE)
  }

  # Check expect_after condition
  unexpected <- out %>%
    filter(!(!!test_def$expect_after))
  if (nrow(unexpected)){
    unexpected <- unexpected %>%
      select(group, name, value, range_low, range_high, unit)
   errors("unexpected cases\n", print(unexpected))
  }

  # Return result
  cat(nrow(out), "extracted.")
  out %>%
    select(-will_silently_exclude, -will_silently_exclude_na,
           -value_numeric, -value_original, -is_too_high, -is_too_low)
}


tests_check_for_new <- function(x,
                                test_def){
  if (!all(is.na(test_def$search_exclude))){
    test_def$search_exclude <- c(test_def$names_cuh, test_def$search_exclude)
  } else {
    test_def$search_exclude <- test_def$names_cuh
  }
  if (!all(is.na(test_def$names_external))){
    test_def$search_exclude <- c(test_def$search_exclude,
                                 test_def$names_external)
  }
  if (all(is.na(test_def$search_exclude_group))){
    test_def$search_exclude_group <- c()
  }
  test_def$search_pattern <- paste0(test_def$search_pattern, collapse = "|")
  x %>%
    filter(str_detect(name, regex(test_def$search_pattern, ignore_case = TRUE)) &
             (!x[[name]] %in% c(test_def$names_cuh, test_def$search_exclude)) &
             (!group %in% test_def$search_exclude_group)) %>%
    group_by(name) %>%
    tally
}

#' Widen test data
#'
#' Reshapes the data so that all tests from the same `order_id` for the
#' same person appear in columns
#'
#' @param x A tidy test data frame
#' @param values_fn A function to summarise multiple results for a particular
#'   test from a particular order.
#' @return A data frame with the tests in columns
#' @author R.J.B. Goudie
tests_pivot_order <- function(x, values_fn = NULL){
  # check for non-unique results for a single order_id
  multiple_results <- x %>%
    pivot_wider(id_cols = c("person_id", "order_id"),
                names_from = symbol,
                values_from = value,
                values_fn = length) %>%
    select(-person_id, -order_id) %>%
    filter(if_any(everything(), ~ .x > 1))

  if (nrow(multiple_results)){
    message("Note ",
            nrow(multiple_results),
            " non-unique results by order found")
  }
  x %>%
    pivot_wider(id_cols = c("person_id", "order_id"),
                names_from = symbol,
                values_from = value,
                values_fn = values_fn)
}

#' Helper function to extract nonnumeric test values
#'
#' @param x A test data frame, with `value` column
#'
#' @return The rows of the supplied data frame that are `NA` after conversion to
#'   to numeric form
#' @author R.J.B. Goudie
tests_nonnumeric <- function(x){
  value <- x$value
  value_numeric <- suppressWarnings({
    as.numeric(value)
  })
  value_is_nonnumeric <- is.na(value_numeric)
  x[value_is_nonnumeric, ]
}
