#' Tidy raw med_admin colnames
#' @param x A data frame of raw med_admin data
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
med_admin_rename <- function(x){
  x %>%
    rename(person_id = "STUDY_SUBJECT_DIGEST",
           administered_datetime = "TimeAdministered",
           name = "DrugName",
           dose = "DoseAsLabelled",
           rate = "InfusionRate",
           unit = "DoseUnitAbbreviated",
           route = "RouteOfMedicationAbbreviated",
           department = "DepartmentName",
           visit_id = "MAR_ENC_CSN",
           action = "MARAction")
}

#' Untidy raw med_admin colnames
#' @param x A data frame of raw med_admin data with tidy names
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
med_admin_unrename <- function(x){
  x %>%
    rename(STUDY_SUBJECT_DIGEST = "person_id",
           TimeAdministered = "administered_datetime",
           DrugName = "name",
           DoseAsLabelled = "dose",
           InfusionRate = "rate",
           DoseUnitAbbreviated = "unit",
           RouteOfMedicationAbbreviated = "route",
           DepartmentName = "department",
           MAR_ENC_CSN = "visit_id",
           MARAction = "action")
}


#' Add a new definition of a test item
#'
#' @param med_admin_def A list of existing test definitions to add this to
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
med_admin_add <- function(med_admin_def,
                          symbol,
                          title,
                          names = NA,
                          search_pattern,
                          search_exclude = NA,
                          silently_exclude_na_routes = FALSE,
                          route,
                          route_exclude = NA,
                          action,
                          action_exclude = NA,
                          expect_after = TRUE){
  expect_after <- enquo(expect_after)

  new <- list(symbol = symbol,
              title = title,
              names = names,
              search_pattern = search_pattern,
              search_exclude = search_exclude,
              silently_exclude_na_routes = silently_exclude_na_routes,
              route = route,
              route_exclude = route_exclude,
              action = action,
              action_exclude = action_exclude,
              expect_after = expect_after)
  med_admin_def <- append(med_admin_def, list(new))
  names(med_admin_def)[length(med_admin_def)] <- symbol
  med_admin_def
}

#' Extract med_admin data into tidy format
#'
#' @param x Medication administration data in tidy-column-name format (after
#'   applying `med_admin_rename`)
#' @param med_admin_def A med_admin definition
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
med_admin_extract <- function(x, med_admin_def, errors = stop){
  if (length(med_admin_def) == 1 & "symbol" %in% names(med_admin_def)){
    med_admin_extract_single(x, med_admin_def)
  } else {
    out <- bind_rows(lapply(med_admin_def, function(y){
      inform(format_error_bullets(c(
        glue("\nExtracting {y$title} ({y$symbol})"))))
      med_admin_extract_single(x, y, errors = errors)
    }))
    out %>% arrange(symbol, administered_datetime)
  }
}

med_admin_extract_single <- function(x, med_admin_def, errors = stop){
  ## possible_new <- med_admin_check_for_new(x, med_admin_def)
  ## if (nrow(possible_new) > 0){
  ##   possible_new_names <- str_flatten(possible_new$name, "; ")
  ##   warning(format_error_bullets(c(
  ##     i = glue("{nrow(possible_new)} possible new medication names: ",
  ##              "{possible_new_names}"))),
  ##     immediate. = TRUE)
  ## }

  # Filter to only CUH med_admin
  out <- x %>%
    filter(name %in% med_admin_def$names &
             route %in% med_admin_def$route &
             action %in% med_admin_def$action)

  # Add symbol and title
  out <- out %>%
    mutate(symbol = med_admin_def$symbol, .after = person_id) %>%
    mutate(title = med_admin_def$title, .after = unit) %>%
    relocate(name, .after = unit)

  # Remove duplicate rows
  out <- out %>%
    distinct_inform

  # Exclude NAs when requested
  out <- out %>%
    mutate(will_silently_exclude_na_routes = (is.na(route) & med_admin_def$silently_exclude_na_routes)) %>%
    filter_inform(!will_silently_exclude_na_routes,
                  since = "since route was NA")

  # Check expect_after condition
  check_that_all(out, !!med_admin_def$expect_after, "expect_after")

  # Return result
  inform(format_error_bullets(c(i = glue("{nrow(out)} rows extracted"))))
  out %>%
    select(-will_silently_exclude_na_routes) %>%
    arrange(administered_datetime)
}

#' Generate a stub definition of medication administration
#'
#' Returns the code needed to define a med_admin_def object.
#'
#' Typically, you may wish to run with `search` set to the drug name, and
#' then rerun once you have filtered the list of medications down. This
#' ensures that the `route` and `action` listed only includes those found
#' in the data for your medications of interest.
#'
#' @param x Medication data in tidy-column-name format (after applying
#'   `med_admin_rename`)
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
med_admin_stub_def <-
  function(x,
           symbol,
           title,
           search = NULL,
           names = NULL,
           action_exclude = c("Missed",
                              "Cancelled Entry",
                              "MAR Hold",
                              "MAR Unhold",
                              "Doctor Medication Unhold",
                              "See Alternative",
                              "Doctor Medication Hold",
                              "Held",
                              "Automatically Held",
                              "Pending",
                              "Due",
                              "Stopped")){
    if (!is.null(search)){
      possible_includes <- filter_med_admin(x, search)
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

  possible_includes_action <- possible_includes %>%
    count(action, sort = TRUE) %>%
    pull(action)

  excludes_action <- intersect(possible_includes_action, action_exclude)
  possible_includes_action <- setdiff(possible_includes_action,
                                      excludes_action)

  possible_includes_unit <- possible_includes %>%
    count(unit, sort = TRUE) %>%
    pull(unit)

  possible <- format_as_argument(setdiff(possible_includes_names, possible_excludes_names))
  excludes <- format_as_argument(possible_excludes_names)
  search <- format_as_argument(search)
  possible_excludes_names <- format_as_argument(possible_excludes_names)
  possible_includes_routes <- format_as_argument(possible_includes_routes)
  possible_includes_action <- format_as_argument(possible_includes_action)
  excludes_action <- format_as_argument(excludes_action)

  possible_includes_expect_before <- ""
  if (length(possible_includes_unit) == 1 && !is.na(possible_includes_unit)){
    possible_includes_expect_before <-
      glue(",\n    expect_before = (unit == {format_as_argument(possible_includes_unit)})")
  } else if (length(possible_includes_unit) > 1 & all(!is.na(possible_includes_unit))){
    possible_includes_expect_before <-
      glue(",\n    expect_before = (unit %in% {format_as_argument(possible_includes_unit)})")
  }

  glue("med_admin_def <- med_admin_def %>%
  med_admin_add(
    symbol = \"{symbol}\",
    title = \"{title}\",
    names = {possible},
    search_pattern = {search},
    search_exclude = {possible_excludes_names},
    {possible_exclude_na_routes}route = {possible_includes_routes},
    route_exclude = c(),
    action = {possible_includes_action},
    action_exclude = {excludes_action}{possible_includes_expect_before})")
}
