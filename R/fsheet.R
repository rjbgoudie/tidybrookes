#' Tidy raw fsheet colnames
#' @param x A data frame of raw fsheet data
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
fsheet_rename <- function(x){
  x %>%
    relocate(
      person_id = STUDY_SUBJECT_DIGEST,
      name = disp_name,
      value = measured_value,
      comment = meas_comment,
      measurement_datetime = MEASURE_TIME,
      data_id = fsd_id,
      measurement_id = "flo-meas_id",
      line_id = line,
      template = template,
      form = form)
}

#' Untidy raw fsheet colnames
#' @param x A data frame of raw fsheet data with tidy names
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
fsheet_unrename <- function(x){
  x %>%
    relocate(
      STUDY_SUBJECT_DIGEST = person_id,
      disp_name = name,
      measured_value = value,
      meas_comment = comment,
      MEASURE_TIME = measurement_datetime,
      fsd_id = data_id,
      `flo-meas_id` = measurement_id,
      line = line_id,
      template = template,
      form = form)
}

#' Add a new definition of a flowsheet item
#'
#' @param fsheet_def A list of existing fsheet definitions to add this to
#' @param symbol A short name for the flowsheet item. This should be a character vector that is suitable to use as an R object name
#' @param title The full name for the flowsheet item, suitable for using in
#'   figure titles etc
#' @param names The display names (`disp_name` in raw format) of the flowsheet
#'   items to include
#' @param search_pattern A charater vector of potential synonymns for this
#'   flowsheet item, in case new names (`disp_name` in raw format) for the
#'   same item are added in future.
#' @param search_exclude A character vector of flowsheet item names
#'   (`disp_name` in raw format)  that are known NOT to be relevant to this
#'   flowsheet item.
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
#' @param unit_rescale_fn A function specifying how to rescale the flowsheet
#'   values. By default, does not rescale
#' @param unit_relabel_fn A function specifying how to relabel the units of the
#'   the flowsheet values. By default, does not rescale
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
fsheet_add <- function(fsheet_def,
                       symbol,
                       title,
                       names = NA,
                       search_pattern,
                       search_exclude = NA,
                       type = "numeric",
                       silently_exclude_na_when = FALSE,
                       silently_exclude_when = FALSE,
                       censoring_fn = case_when(TRUE ~ NA_character_),
                       value_as_number_fn =
                         if_else(type == "numeric",
                                 case_when(TRUE ~ value_as_number),
                                 case_when(TRUE ~ NA_real_)),
                       value_as_character_fn =
                         if_else(type == "numeric",
                                 case_when(TRUE ~ NA_character_),
                                 case_when(TRUE ~ value_as_character)),
                       unit_rescale_fn = case_when(TRUE ~ value_as_number),
                       unit_relabel_fn = case_when(TRUE ~ NA_character_),
                       expect_before = TRUE,
                       expect_after = TRUE,
                       range_mainly_low = NA,
                       range_mainly_high = NA,
                       range_discard_below = NA,
                       range_discard_above = NA){
  censoring_fn <- enquo(censoring_fn)
  value_as_number_fn <- enquo(value_as_number_fn)
  value_as_character_fn <- enquo(value_as_character_fn)
  silently_exclude_na_when <- enquo(silently_exclude_na_when)
  silently_exclude_when <- enquo(silently_exclude_when)
  unit_rescale_fn <- enquo(unit_rescale_fn)
  unit_relabel_fn <- enquo(unit_relabel_fn)
  expect_before <- enquo(expect_before)
  expect_after <- enquo(expect_after)

  new <- list(symbol = symbol,
              title = title,
              names = names,
              search_pattern = search_pattern,
              search_exclude = search_exclude,
              type = type,
              silently_exclude_na_when = silently_exclude_na_when,
              silently_exclude_when = silently_exclude_when,
              censoring_fn = censoring_fn,
              value_as_number_fn = value_as_number_fn,
              value_as_character_fn = value_as_character_fn,
              unit_rescale_fn = unit_rescale_fn,
              unit_relabel_fn = unit_relabel_fn,
              expect_before = expect_before,
              expect_after = expect_after,
              range_mainly_low = range_mainly_low,
              range_mainly_high = range_mainly_high,
              range_discard_below = range_discard_below,
              range_discard_above = range_discard_above)
  fsheet_def <- append(fsheet_def, list(new))
  names(fsheet_def)[length(fsheet_def)] <- symbol
  fsheet_def
}

#' Extract fsheet data into tidy format
#'
#' @param x Flowsheet data in renamed format (after applying `fsheet_rename`)
#' @param fsheet_def A fsheet definition
#' @param errors A function indicating what to do when an error is found
#'
#' @return
#' A data frame with the following columns:
#' `person_id`, `symbol`, `value_as_character`, `value_as_number`, `censoring`,
#' `comment`, `measurement_datetime`, `name`, `title`, `data_id`,
#' `measurement_id`, `line_id`, `template`, `form`, `type`, `unit`
#' @author R.J.B. Goudie
fsheet_extract <- function(x, fsheet_def, errors = stop){
  if (length(fsheet_def) == 1 & "symbol" %in% names(fsheet_def)){
    fsheet_extract_single(x, fsheet_def)
  } else {
    out <- bind_rows(lapply(fsheet_def, function(y){
      inform(format_error_bullets(c(
        glue("\nExtracting {y$title} ({y$symbol})"))))
      fsheet_extract_single(x, y, errors = errors)
    }))
    out %>% arrange(symbol, measurement_datetime)
  }
}


fsheet_extract_single <- function(x, fsheet_def, errors = stop){
  out <- x %>%
    filter(name %in% fsheet_def$names)

  # Add symbol and title
  out <- out %>%
    mutate(symbol = fsheet_def$symbol, .after = person_id) %>%
    mutate(title = fsheet_def$title, .after = measurement_datetime) %>%
    relocate(name, .after = measurement_datetime) %>%
    rename(value_original = value) %>%
    mutate(type = fsheet_def$type)

  # Check expect_before condition
  check_that_all(out, !!fsheet_def$expect_before, "expect_before")

  # Remove duplicate rows
  out <- out %>%
    distinct_inform

  # Exclude NAs when requested
  out <- out %>%
    mutate(will_silently_exclude_na = (is.na(value_original) & !!fsheet_def$silently_exclude_na_when)) %>%
    filter_inform(!will_silently_exclude_na,
                  since = "since value was NA")

  # Exclude other rows when requested
  out <- out %>%
    mutate(will_silently_exclude = (!!fsheet_def$silently_exclude_when)) %>%
    filter_inform(!will_silently_exclude,
                  since = "due to exclude_when condition")

  # Convert values to numeric, and handle censoring
  out <- out %>%
    group_by(name) %>%
    mutate(
      value_as_character = suppressWarnings({
        as.character(value_original)
      }),
      value_as_number = suppressWarnings({
        as.numeric(value_original)
      }),
      value_as_character = !!fsheet_def$value_as_character_fn,
      value_as_number = !!fsheet_def$value_as_number_fn,
      censoring = !!fsheet_def$censoring_fn,
      .after = value_original) %>%
    relocate(censoring, .after = value_as_number) %>%
    ungroup

  if (fsheet_def$type == "numeric"){
    check_that_all(out,
                   suppressWarnings({!is.na(as.numeric(value_as_number))}),
                   name = "all values being numeric")
  }

  # Rescale units
  out <- out %>%
    mutate(value_as_number = !!fsheet_def$unit_rescale_fn,
           unit = !!fsheet_def$unit_relabel_fn)

  # Discard too high values
  if (!is.na(fsheet_def$range_discard_above)){
    out <- out %>%
      mutate(is_too_high = value_as_number > fsheet_def$range_discard_above) %>%
      filter_inform(!is_too_high,
                    since = glue("since >{fsheet_def$range_discard_above}"))
  } else {
    out <- out %>%
      mutate(is_too_high = FALSE)
  }

  # Discard too low values
  if (!is.na(fsheet_def$range_discard_below)){
    out <- out %>%
      mutate(is_too_low = value_as_number < fsheet_def$range_discard_below) %>%
      filter_inform(!is_too_low,
                    since = glue("since <{fsheet_def$range_discard_below}"))
  } else {
    out <- out %>%
      mutate(is_too_low = FALSE)
  }

  # Check expect_after condition
  check_that_all(out, !!fsheet_def$expect_after, "expect_after")

  # Return result
  inform(format_error_bullets(c(i = glue("{nrow(out)} rows extracted"))))
  out %>%
    select(-will_silently_exclude, -will_silently_exclude_na,
           -value_original, -is_too_high, -is_too_low) %>%
    arrange(measurement_datetime)
}
