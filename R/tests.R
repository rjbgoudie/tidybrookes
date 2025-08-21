# TODO make these the default
censored_number_regex <-
  paste0("^(?<censoring>(?:[<>](?:=)?)|Less than|More than)?(?:\\s)*",
         "(?<value>(?:[-+])?(?:[.|\\d])*)$")

censoring_default <- function(value_original){
  censoring <-
    str_match(value_original, censored_number_regex)[, "censoring"]

  case_when(censoring %in% c(">", ">=", "More than") ~ "right",
            censoring %in% c("<", "<=", "Less than") ~ "left",
            TRUE ~ NA_character_)

}

value_as_number_default <- function(value_original, value_as_number, censoring){
  censored_value <- str_match(value_original, censored_number_regex)[, "value"]
  case_when(censoring %in% c("left", "right") ~
              suppressWarnings({
                as.numeric(censored_value)
              }),
            TRUE ~ value_as_number)
}

#' Add a new definition of a test item
#'
#' @param test_def A list of existing test definitions to add this to
#' @param symbol A short name for the flowsheet item. This should be a
#'   character vector that is suitable to use as an R object name
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
#' @param value_as_logical_fn A function specifying how to generate the
#'   `value_as_logical` column
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
#' @examples
#' tests_raw_example <-
#'   read_tidybrookes_csv(
#'     file = tidybrookes_example("tests.csv"),
#'     col_types = "tests") %>%
#'     tests_rename
#'
#' tests_def <- list() %>%
#'   tests_add(
#'     symbol = "magnesium",
#'     title = "Magnesium",
#'     names_cuh = c("MAGNESIUM"),
#'     names_external = "EXT MAGNESIUM",
#'     search_pattern = c("magnesium", "mg"),
#'     search_exclude = c("ETOH MG/100ML",
#'                        "RETIRED-ETOH MG/100ML",
#'                        "RIST 0.2MG/ML",
#'                        "RIST 0.77MG/ML",
#'                        "RISTOCETIN 0.5MG/ML",
#'                        "RISTOCETIN 1.5MG/ML"),
#'     silently_exclude_na_when = FALSE,
#'     censoring_fn = case_when(value_original == "<0.29" ~ "left", # very low
#'                              TRUE ~ NA_character_),
#'     value_as_number_fn = case_when(value_original == "<0.29" ~ 0.29,
#'                                    TRUE ~ value_as_number),
#'     expect_before = (unit == "mmol/L"),
#'     range_mainly_low = 0.4,
#'     range_mainly_high = 1.2,
#'     range_discard_above = 1.8)
#'
#' tests_raw_example %>% filter(name == "MAGNESIUM")
#'
#' # Note that
#' # - the high value of 3.0 is deleted because it is above
#' #   range_discard_above
#' # - note that the censored value "<0.29" is converted to 0.29, and "left"
#' #   added to the censoring column
#' tests_extract(tests_raw_example, tests_def)
#' @rdname tests_extract
#' @export
tests_add <- function(test_def,
                      symbol,
                      title,
                      names_cuh = NULL,
                      names_external = NULL,
                      search_pattern,
                      search_exclude = NULL,
                      search_exclude_group = NULL,
                      type = "numeric",
                      silently_exclude_na_when = FALSE,
                      silently_exclude_when = FALSE,
                      censoring_fn = case_when(TRUE ~ NA_character_),
                      value_as_number_fn =
                        if_else(type == "numeric",
                                case_when(TRUE ~ value_as_number),
                                case_when(TRUE ~ NA_real_)),
                      value_as_character_fn =
                        if_else(type == "character",
                                case_when(TRUE ~ value_as_character),
                                case_when(TRUE ~ NA_character_)),
                      value_as_logical_fn =
                        if_else(type == "logical",
                                case_when(TRUE ~ value_as_logical),
                                case_when(TRUE ~ as.logical(NA))),
                      unit_rescale_fn = case_when(TRUE ~ value_as_number),
                      unit_relabel_fn = case_when(TRUE ~ unit),
                      expect_before = TRUE,
                      expect_after = TRUE,
                      range_mainly_low = NULL,
                      range_mainly_high = NULL,
                      range_discard_below = NULL,
                      range_discard_above = NULL){
  censoring_fn <- enquo(censoring_fn)
  value_as_number_fn <- enquo(value_as_number_fn)
  value_as_character_fn <- enquo(value_as_character_fn)
  value_as_logical_fn <- enquo(value_as_logical_fn)
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
              type = type,
              silently_exclude_na_when = silently_exclude_na_when,
              silently_exclude_when = silently_exclude_when,
              censoring_fn = censoring_fn,
              value_as_number_fn = value_as_number_fn,
              value_as_character_fn = value_as_character_fn,
              value_as_logical_fn = value_as_logical_fn,
              unit_rescale_fn = unit_rescale_fn,
              unit_relabel_fn = unit_relabel_fn,
              expect_before = expect_before,
              expect_after = expect_after,
              range_mainly_low = range_mainly_low,
              range_mainly_high = range_mainly_high,
              range_discard_below = range_discard_below,
              range_discard_above = range_discard_above)
  test_def <- append(test_def, list(new))
  names(test_def)[length(test_def)] <- symbol
  test_def
}

tests_remove <- function(test_def, symbol){
  index <- names(test_def) == symbol
  test_def[!index]
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
#' `person_id`, `symbol`, `value_as_number`, `value_as_character`,
#' `value_as_logical`, `censoring`, `ordered_datetime`, `collected_datetime`,
#' `received_datetime`, `result_datetime`, `ordering_department_name`,
#' `range_low`, `range_high`, `unit`, `name`, `title`, `method`, `group`,
#' `type`
#'
#' The result will be sorted by collected_datetime
#' @author R.J.B. Goudie
#' @rdname tests_extract
#' @export
tests_extract <- function(x,
                          tests_def,
                          errors = stop,
                          rds_only = FALSE,
                          rds_filepath_fn = NULL){
  tests_def <- wrap_def_if_single(tests_def)

  if (!rds_only){
    out <- bind_rows(lapply(tests_def, function(y){
      tests_extract_single(x, y, errors = errors)
    }))
    out %>% arrange(symbol, collected_datetime)
  } else if (rds_only){
    lapply(tests_def, function(y){
      symbol <- y$symbol
      out <- tests_extract_single(x, y, errors = errors)
      out <- out %>% arrange(symbol, collected_datetime)
      saveRDS(out, file = rds_filepath_fn(symbol))
    })
  }
}


tests_extract_single <- function(x, tests_def, errors = stop){
  out <- tests_annotate_single(x, tests_def)

  out <- out %>%
    filter(!exclude)

  cli::cli_alert_info("{nrow(out)} rows extracted")
  out %>%
    relocate(person_id,
             symbol,
             measurement_datetime,
             value_as_number,
             censoring,
             value_as_logical,
             value_as_character,
             name,
             unit,
             type,
             template,
             form) %>%
    arrange(collected_datetime)
}


#' Add annotations to tests data
#'
#' Annotate tests data to note rows that should be excluded etc.
#'
#' Annotations can either be added as additional columns (when
#' `annotation_db = NULL`), or stored in a separate database table (by
#' setting `annotation_db` to a database `tbl`).
#'
#' Note that when adding the annotations as additiona columns,
#' the result will not be sorted, since it is left in the original
#' order of `x`
#'
#' @param x Tests data in renamed format (after applying `tests_rename`)
#' @param tests_def A tests definition, or list of tests definitions
#' @param annotation_db Either `NULL` or a `tbl` reference to the annotations
#'   database table that the annotations should be stored in. If `NULL` the
#'   annotations are added to `x` as additional columns.
#' @author R.J.B. Goudie
tests_annotate <- function(x,
                           tests_def,
                           annotation_db = NULL){
  tests_def <- wrap_def_if_single(tests_def)

  out <- lapply(tests_def, function(y){
    symbol <- y$symbol
    x_annotated <- tests_annotate_single(x, y, errors = errors)

    return_or_write_to_annotation_db(x,
                                     x_annotated,
                                     annotation_db,
                                     id_cols = "tests_id")
  })
  if (is.null(annotation_db)){
    x |>
      left_join(bind_rows(out))
  }
}


tests_annotation_schema <- function(){
  tibble(
    tests_id = integer(0),
    symbol = character(0),
    title = character(0),
    value_as_character = character(0),
    value_as_number = numeric(0),
    censoring = character(0),
    value_as_logical = logical(0),
    type = character(0),
    satisfies_expect_before = logical(0),
    satisfies_all_numeric = logical(0),
    satisfies_expect_after = logical(0),
    exclude = logical(0),
    exclude_is_duplicate = logical(0),
    exclude_is_duplicate_of_row_number = integer(0),
    exclude_is_silently_exclude_na = logical(0),
    exclude_is_silently_exclude = logical(0),
    # exclude_is_coalesced = logical(0),
    exclude_is_too_high = logical(0),
    exclude_is_too_low = logical(0),
    range_mainly_low = numeric(0),
    range_mainly_high = numeric(0),
    range_discard_below = numeric(0),
    range_discard_above = numeric(0),
    unit = character(0)
  )
}


tests_annotate_single <- function(x, test_def, errors = stop){
  cli::cli_alert_info(
    c("Extracting {test_def$title} ",
      "({test_def$symbol}) from {nrow(out)} raw rows"))

  out <- x |>
    filter(name %in% test_def$names_cuh)

  if (inherits(x, "tbl_sql")){
    out <- collect(out)
  }

  out <- out %>%
    bind_rows(tests_annotation_schema())

  # possible_new <- tests_check_for_new(x, test_def)
  # if (nrow(possible_new) > 0){
  #   possible_new_names <- format_as_argument(possible_new$name)
  #   warning(format_error_bullets(c(
  #     i = glue("{nrow(possible_new)} possible new test names: ",
  #              "{possible_new_names}"))),
  #     immediate. = TRUE)
  # }

  # TODO fsheet_info vs test_info
  # make more similar and add , exclude_lists = TRUE
  info <- fsheet_info(list(test_def), exclude_lists = TRUE) |>
    select(-any_of(c("names_cuh",
                     "names_external",
                     "search_pattern",
                     "search_exclude",
                     "search_exclude_group")))

  if (nrow(out) == 0){
    cli::cli_alert_info("{nrow(out)} rows extracted")
    return(out)
  }
  # Add symbol and title
  out <- out %>%
    mutate(symbol = test_def$symbol, .after = person_id) %>%
    rows_update(info, by = "symbol") %>%
    relocate(title, .after = unit) %>%
    relocate(name, .after = unit) %>%
    rename(value_original = value)

  # Check expect_before condition
  out <- exclusion_label_check_that_all(out,
                                        !!test_def$expect_before,
                                        label = "expect_before",
                                        summary = function(x){
                                          x %>%
                                            count(name, unit, value_original)
                                        })

  # Remove duplicate rows
  out <- out %>%
    exclusion_label_duplicates_inform(ignore_columns = tests_id)

  # Exclude NAs when requested
  out <- out %>%
    exclusion_label_condition_inform(
      exclude_is_silently_exclude_na,
      (is.na(value_original) & !!test_def$silently_exclude_na_when),
      since = "since value was NA")

  # Exclude other rows when requested
  out <- out %>%
    exclusion_label_condition_inform(
      exclude_is_silently_exclude,
      (!!test_def$silently_exclude_when),
      since = "due to exclude_when condition")

  if (nrow(out) == 0){
    out %>% select(-will_silently_exclude,
                   -will_silently_exclude_na,
                   -value_original)
  } else {
    # Convert values to numeric, and handle censoring
    out <- out %>%
      group_by(name) %>%
      mutate(
        censoring = !!test_def$censoring_fn,
        value_as_character = suppressWarnings({
          as.character(value_original)
        }),
        value_as_number = suppressWarnings({
          as.numeric(value_original)
        }),
        value_as_logical = suppressWarnings({
          as.logical(value_original)
        }),
        value_as_character = !!test_def$value_as_character_fn,
        value_as_number = !!test_def$value_as_number_fn,
        value_as_logical = !!test_def$value_as_logical_fn,
        .after = value_original) %>%
      relocate(censoring, .after = value_as_number) %>%
      ungroup

    # Check for nonnumeric values in the post-exclusion and
    # post-handling-censoring data frame
    if (test_def$type == "numeric"){
      out <-
        exclusion_label_check_that_all(out,
                                       suppressWarnings({!is.na(as.numeric(value_as_number))}),
                                       label = "all_numeric")
    }

    # Rescale units
    out <- out %>%
      mutate(value_as_number = !!test_def$unit_rescale_fn,
             unit = !!test_def$unit_relabel_fn)

    # Discard too high values
    if (!is.null(test_def$range_discard_above)){
      out <- out %>%
        exclusion_label_condition_inform(
          exclude_is_too_high,
          value_as_number > test_def$range_discard_above,
          since = glue("since >{test_def$range_discard_above}"))
    } else {
      out <- out %>%
        mutate(exclude_is_too_high = FALSE)
    }

    # Discard too low values
    if (!is.null(test_def$range_discard_below)){
      out <- out %>%
        exclusion_label_condition_inform(
          exclude_is_too_low,
          value_as_number < test_def$range_discard_below,
          since = glue("since <{test_def$range_discard_below}"))
    } else {
      out <- out %>%
        mutate(exclude_is_too_low = FALSE)
    }

    # Check expect_after condition
    out <- exclusion_label_check_that_all(out,
                                          (!!test_def$expect_after),
                                          label = "expect_after")

    # Return result
    cli::cli_alert_info("{nrow(out)} rows extracted")
    out %>%
      select(-value_original) %>%
      arrange(collected_datetime)
  }
}

tests_check_for_new <- function(x,
                                test_def){
  if (!all(is.null(test_def$search_exclude))){
    test_def$search_exclude <- c(test_def$names_cuh, test_def$search_exclude)
  } else {
    test_def$search_exclude <- test_def$names_cuh
  }
  if (!all(is.null(test_def$names_external))){
    test_def$search_exclude <- c(test_def$search_exclude,
                                 test_def$names_external)
  }
  if (all(is.null(test_def$search_exclude_group))){
    test_def$search_exclude_group <- c()
  }
  test_def$search_pattern <- paste0(test_def$search_pattern, collapse = "|")
  x %>%
    filter(str_detect(name, regex(test_def$search_pattern, ignore_case = TRUE)) &
             (!name %in% c(test_def$names_cuh, test_def$search_exclude)) &
             (!group %in% test_def$search_exclude_group)) %>%
    count(name)
}




#' Widen test data
#'
#' Reshapes the data so that all tests from the same `order_id` for the
#' same person appear in columns
#'
#' @param x A tidy test data frame
#' @param values_fn A function to summarise multiple results for a particular
#'   test from a particular order.
#' @param include_censoring Logical, should censoring column be included?
#' @return A data frame with the tests in columns
#' @author R.J.B. Goudie
#' @export
tests_pivot_wider_order <- function(x,
                                    values_fn = NULL,
                                    include_censoring = FALSE){
  # check for non-unique results for a single order_id
  multiple_results <- x %>%
    pivot_wider(id_cols = c("person_id",
                            "order_id",
                            "collected_datetime",
                            "received_datetime",
                            "result_datetime"),
                names_from = symbol,
                values_from = value_as_number,
                values_fn = length) %>%
    filter(dplyr:::if_any(!c(person_id,
                             order_id,
                             collected_datetime,
                             received_datetime,
                             result_datetime), ~ .x > 1))

  if (nrow(multiple_results)){
    message("Note ",
            nrow(multiple_results),
            " non-unique results by order found")
  }
  if (include_censoring){
    x %>%
      pivot_wider(id_cols = c("person_id",
                              "order_id",
                              "collected_datetime",
                              "received_datetime",
                              "result_datetime"),
                  names_from = symbol,
                  values_from = c("value_as_number", "censoring"),
                  names_glue = "{symbol}_{.value}",
                  names_sort = TRUE) %>%
      rename_with(.fn = ~ if_else(str_ends(.x, "_value_as_number"),
                                  true = str_replace(.x, "_value_as_number", "_value"),
                                  false = .x))
  } else {
    x %>%
      pivot_wider(id_cols = c("person_id",
                              "order_id",
                              "collected_datetime",
                              "received_datetime",
                              "result_datetime"),
                  names_from = symbol,
                  values_from = "value_as_number",
                  values_fn = values_fn)
  }
}

#' Pivot tests longer by order
#'
#' @export
tests_pivot_longer_order <- function(x){
  x %>%
    tidyr:::pivot_longer(cols = !c(person_id,
                                   order_id,
                                   collected_datetime,
                                   received_datetime,
                                   result_datetime),
                         values_to = "value_as_number",
                         names_to = "symbol")
}

#' Reshape arterial blood gas data
#'
#' @export
tests_abg <- function(bg_data, bg_specimen_type){
  bg_specimen_type <- bg_specimen_type %>%
    select(person_id,
           order_id,
           result_datetime,
           specimen_type = value_as_character) %>%
    mutate(specimen_type_simplified =
             if_else(specimen_type == "Arterial blood",
                     true = "Arterial blood",
                     false = "Non-arterial blood",
                     missing = "Non-arterial blood"))

  # There are missing values in blood specimen type
  # Note there is no POC BLOOD SPECIMEN TYPE row for this OrderProcId
  # we will label these as "Non-arterial blood"
  bg_data <- bg_data %>%
    pivot_wider(id_cols = c("person_id", "order_id", "result_datetime"),
                names_from = symbol,
                values_from = c("value_as_number", "censoring"),
                names_glue = "{symbol}_{.value}",
                names_sort = TRUE) %>%
    # full join means that results that orders with no result (but a specimen
    # type remain in the data frame
    full_join(bg_specimen_type,
              by = c("person_id", "order_id", "result_datetime")) %>%
    relocate(specimen_type,
             specimen_type_simplified,
             .after = "order_id") %>%
    rename_with(.fn = ~ if_else(str_ends(.x, "_value_as_number"),
                                true = str_replace(.x, "_value_as_number", "_value"),
                                false = .x))

  abg_data <- bg_data %>%
    filter(specimen_type_simplified == "Arterial blood") %>%
    select(-specimen_type, -specimen_type_simplified) %>%
    rename_with(.fn = ~ str_replace(.x, "_bg", "_abg"),
                .cols = !c(person_id, order_id, result_datetime)) %>%
    mutate(pao2_fio2_ratio_value = po2_temp_abg_value * 7.5/fio2_abg_value) %>%
    tidyr:::pivot_longer(cols = !c(person_id, order_id, result_datetime),
                         names_to = c("symbol", ".value"),
                         names_pattern = c("([A-Za-z0-9_]+)_([^_]+)$")) %>%
    rename(value_as_number = value) %>%
    mutate(type = "numeric",
           # Since point of care test, all of these are the same
           collected_datetime = result_datetime,
           ordered_datetime = result_datetime)

  abg_data
}

#' Convert tests defintion list to a dataframe
#'
#' @param tests_def A test definition list
#' @examples
#' tests_info(tests_def_example)
#' @rdname tests_info
#' @export
tests_info <- function(tests_def){
  if (length(tests_def) == 1 & "symbol" %in% names(tests_def)){
    tests_info_single(tests_def)
  } else {
    out <- lapply(tests_def, function(y){
      tests_info_single(y)
    })
    bind_rows(out)
  }
}

tests_info_single <- function(test_def){
  tibble(symbol = test_def$symbol,
         title = test_def$title,
         type = test_def$type,
         names_cuh = list(test_def$names_cuh %||% character(0)),
         names_external = list(test_def$names_external %||% character(0)),
         range_mainly_low = test_def$range_mainly_low,
         range_mainly_high = test_def$range_mainly_high,
         range_discard_below = test_def$range_discard_below,
         range_discard_above = test_def$range_discard_above,
         search_pattern = list(test_def$search_pattern),
         search_exclude = list(test_def$search_exclude %||% character(0)),
         search_exclude_group = list(test_def$search_exclude_group %||% character(0)))
}
