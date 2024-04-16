#' Add a new definition of a test item
#'
#' @param med_prescr_def A list of existing test definitions to add this to
#' @param symbol A short name for the flowsheet item. This should be a
#'   character vector that is suitable to use as an R object name
#' @param title The full name for the test item, suitable for using in
#'   figure titles etc
#' @param names The names of the Medications (`DrugName` in raw format)
#' @param search_pattern A charater vector of potential synonymns for this
#'   test, in case new names (`TestName` in raw format) for the
#'   same test are added in future.
#' @param search_exclude A character vector of test names
#'   (`TestName` in raw format) that are known NOT to be relevant to this test.
#' @param search_exclude_group A character vector of test group names
#'   (`TestGroupName` in raw format) that are known NOT to be relevant to
#'   this test.
#' @param type Whether flowsheet item is a number or a string. Either
#'   `"numeric"` or `"character"`
#' @param silently_exclude_na_when An expression that returns a logical value,
#'   specifying the circumstances when `NA` values should be automatically
#'   excluded.
#' @param silently_exclude_when An expression that returns a logical value,
#'  specifying the circumstances when values should be automatically
#'  excluded
#' @param censoring_fn A function specifying the value of the `censoring` column
#' @param value_as_number_fn A function specifying how to generate the
#'   `value_as_number` column
#' @param value_as_character_fn A function specifying how to generate the
#'   `value_as_character` column
#' @param unit_rescale_fn A function specifying how to rescale the result
#'   values. By default, does not rescale.
#' @param unit_relabel_fn A function specifying how to relabel the units of the
#'   the result values. By default, does not rescale.
#' @param expect_before An expression that returns a logical value,
#'  specifying a condition that should be `TRUE` in the raw data
#' @param expect_after An expression that returns a logical value,
#'  specifying a condition that should be `TRUE` in the processed (output) data
#' @param range_mainly_low,range_mainly_high An indicative lower and upper range
#'   for most values. Values outside this range are NOT excluded: this is purely
#'   for setting default scales of plots etc
#' @param range_discard_below,range_discard_above This pair set the lower and
#'   upper range for the flowsheet item. Values outside this range are
#'   EXCLUDED.
#' @author R.J.B. Goudie
#' @rdname med_prescr_extract
#' @export
med_prescr_add <- function(med_prescr_def,
                           symbol,
                           title,
                           names = NA,
                           search_pattern,
                           search_exclude = NA,
                           silently_exclude_na_routes = FALSE,
                           route,
                           route_exclude = NA,
                           dose_as_number_fn = case_when(TRUE ~ dose_as_number),
                           dose_rescale_fn = case_when(TRUE ~ as.numeric(dose_as_number)),
                           unit_relabel_fn = case_when(TRUE ~ unit),
                           expect_after = TRUE){
  expect_after <- enquo(expect_after)
  dose_as_number_fn <- enquo(dose_as_number_fn)
  dose_rescale_fn <- enquo(dose_rescale_fn)
  unit_relabel_fn <- enquo(unit_relabel_fn)

  new <- list(symbol = symbol,
              title = title,
              names = names,
              search_pattern = search_pattern,
              search_exclude = search_exclude,
              silently_exclude_na_routes = silently_exclude_na_routes,
              route = route,
              route_exclude = route_exclude,
              dose_as_number_fn = dose_as_number_fn,
              dose_rescale_fn = dose_rescale_fn,
              unit_relabel_fn = unit_relabel_fn,
              expect_after = expect_after)
  med_prescr_def <- append(med_prescr_def, list(new))
  names(med_prescr_def)[length(med_prescr_def)] <- symbol
  med_prescr_def
}

#' Extract med_prescr data into tidy format
#'
#' @param x Medication administration data in tidy-column-name format (after
#'   applying `med_prescr_rename`)
#' @param med_prescr_def A med_prescr definition
#' @param errors A function indicating what to do when an error is found
#'
#' @return
#' A data frame with the following columns:
#' `person_id`, `symbol`, `value_as_number`, `value_as_character`,
#' `censoring`, `administered_datetime`,
#'
#' `unit`, `name`, `title`, `method`, `group`, `type`
#'
#' The result will be sorted by administered_datetime
#' @author R.J.B. Goudie
#' @rdname med_prescr_extract
#' @export
med_prescr_extract <- function(x, med_prescr_def, errors = stop){
  if (length(med_prescr_def) == 1 & "symbol" %in% names(med_prescr_def)){
    med_prescr_extract_single(x, med_prescr_def)
  } else {
    out <- bind_rows(lapply(med_prescr_def, function(y){
      cli::cli_alert_info("Extracting {y$title} ({y$symbol})")
      med_prescr_extract_single(x, y, errors = errors)
    }))
    out %>% arrange(symbol, start_date)
  }
}

med_prescr_extract_single <- function(x, med_prescr_def, errors = stop){
  possible_new <- med_prescr_check_for_new_names(x, med_prescr_def)
  if (nrow(possible_new) > 0){
    possible_new_names <- format_as_argument(possible_new$name)
    cli::cli_alert_warning(
      c("{nrow(possible_new)} possible new medication names: ",
        "{possible_new_names}"))
  }

  # Filter to only CUH med_prescr
  out <- x %>% filter(name %in% med_prescr_def$names)

  possible_new_route <- med_prescr_check_for_new_route(out, med_prescr_def)
  if (nrow(possible_new_route) > 0){
    possible_new_routes <- format_as_argument(possible_new_route$route)
    cli::cli_alert_warning(
      c("{nrow(possible_new_route)} possible new medication routes: ",
        "{possible_new_routes}"))
  }

  out <- out %>%
    filter(route %in% med_prescr_def$route)

  # Add symbol and title
  out <- out %>%
    mutate(symbol = med_prescr_def$symbol, .after = person_id) %>%
    mutate(title = med_prescr_def$title, .after = unit) %>%
    relocate(name, .after = unit) %>%
    mutate(value_as_logical = TRUE) %>%
    rename(dose_original = dose) %>%
    mutate(start_date = as.Date(start_datetime),
           end_date = as.Date(end_datetime)) %>%
    select(-start_datetime, -end_datetime)

  # Remove duplicate rows
  out <- out %>%
    distinct_inform

  # Exclude NAs when requested
  out <- out %>%
    mutate(will_silently_exclude_na_routes = (is.na(route) & med_prescr_def$silently_exclude_na_routes)) %>%
    filter_inform(!will_silently_exclude_na_routes,
                  since = "since route was NA")

  out <- out %>%
    group_by(name) %>%
    mutate(dose_as_number = suppressWarnings({
      as.numeric(dose_original)
    }),
    dose_as_number = !!med_prescr_def$dose_as_number_fn,
    .after = dose_original) %>%
    ungroup

  check_that_all(out,
                 suppressWarnings({!is.na(as.numeric(dose_as_number))}),
                 name = "all doses being numeric")

  # Rescale units
  out <- out %>%
    mutate(dose = !!med_prescr_def$dose_rescale_fn,
           unit = !!med_prescr_def$unit_relabel_fn)

  # Check expect_after condition
  check_that_all(out, !!med_prescr_def$expect_after, "expect_after")

  # Return result
  cli::cli_alert_info("{nrow(out)} rows extracted")
  out %>%
    select(-will_silently_exclude_na_routes) %>%
    arrange(start_date)
}


med_prescr_check_for_new_names <- function(x,
                                           med_prescr_def){
  if (!all(is.na(med_prescr_def$search_exclude))){
    med_prescr_def$search_exclude <- c(med_prescr_def$names, med_prescr_def$search_exclude)
  } else {
    med_prescr_def$search_exclude <- med_prescr_def$names
  }
  med_prescr_def$search_pattern <- paste0(med_prescr_def$search_pattern, collapse = "|")
  x %>%
    filter(str_detect(name, regex(med_prescr_def$search_pattern, ignore_case = TRUE)) &
             (!name %in% c(med_prescr_def$names, med_prescr_def$search_exclude))) %>%
    count(name)
}

med_prescr_check_for_new_route <- function(x,
                                           med_prescr_def){
  x %>%
    filter(!route %in% c(med_prescr_def$route_exclude,
                         med_prescr_def$route)) %>%
    count(route)
}


#' Generate a stub definition of medication administration
#'
#' Returns the code needed to define a med_prescr_def object.
#'
#' Typically, you may wish to run with `search` set to the drug name, and
#' then rerun once you have filtered the list of medications down. This
#' ensures that the `route` and `action` listed only includes those found
#' in the data for your medications of interest.
#'
#' @param x Medication data in tidy-column-name format (after applying
#'   `med_prescr_rename`)
#' @param symbol A short name for the medication administration item. This
#'   should be a character vector that is suitable to use as an R object name
#' @param title The full name for the medication administration item, suitable
#'   for using in figure titles etc
#' @param search A charater vector of potential synonymns for this
#'   test, in case new names/variants of the same medication added in future.
#'   If `NULL`, then `names` is used to define a specific list of medication
#'   names to include.
#' @param names The names of the Medications (`DrugName` in raw format). Only
#'   used if `search = NULL`.
med_prescr_stub_def <-
  function(x,
           symbol,
           title,
           search = NULL,
           names = NULL){
    if (!is.null(search)){
      possible_includes <- filter_med_prescr(x, search)
    } else {
      possible_includes <- x %>%
        filter(name %in% names)
    }

    possible_excludes <- possible_includes %>%
      filter(str_detect(name, coll("MOUTHWASH", ignore_case = TRUE)) |
               str_detect(name, coll("EYE DROPS", ignore_case = TRUE)) |
               str_detect(name, coll("OINTMENT", ignore_case = TRUE)) |
               str_detect(name, coll("CREAM", ignore_case = TRUE)) |
               str_detect(name, coll("EAR DROP", ignore_case = TRUE)))

    possible_includes_names <- possible_includes %>%
      count(name, sort = TRUE) %>%
      pull(name)

    possible_excludes_names <- possible_excludes %>%
      count(name, sort = TRUE) %>%
      pull(name)

    possible_includes_routes <- possible_includes %>%
      count(route, sort = TRUE) %>%
      pull(route)

    possible_exclude_na_routes <- ""
    if (any(is.na(possible_includes_routes))){
      possible_exclude_na_routes <- "silently_exclude_na_routes = FALSE,\n  "
      possible_includes_routes <- possible_includes_routes[!is.na(possible_includes_routes)]
    }

    possible_includes_unit <- possible_includes %>%
      count(unit, sort = TRUE) %>%
      pull(unit)

    possible <- format_as_argument(setdiff(possible_includes_names, possible_excludes_names))
    excludes <- format_as_argument(possible_excludes_names)
    search <- format_as_argument(search)
    possible_excludes_names <- format_as_argument(possible_excludes_names)
    possible_includes_routes <- format_as_argument(possible_includes_routes)

    possible_includes_expect_before <- ""
    if (length(possible_includes_unit) == 1 && !is.na(possible_includes_unit)){
      possible_includes_expect_before <-
        glue(",\n    expect_before = (unit == {format_as_argument(possible_includes_unit)})")
    } else if (length(possible_includes_unit) > 1 & all(!is.na(possible_includes_unit))){
      possible_includes_expect_before <-
        glue(",\n    expect_before = (unit %in% {format_as_argument(possible_includes_unit)})")
    }

    glue("med_prescr_def <- med_prescr_def %>%
  med_prescr_add(
    symbol = \"{symbol}\",
    title = \"{title}\",
    names = {possible},
    search_pattern = {search},
    search_exclude = {possible_excludes_names},
    {possible_exclude_na_routes}route = {possible_includes_routes},
    route_exclude = c())")
  }
