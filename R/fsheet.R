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
      measurement_id = flo.meas_id,
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
      flo.meas_id = measurement_id,
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
#' @param range_discard_below,range_discard_above This pair set the lower and #'   upper range for the flowsheet item. Values outside this range are
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
      cat(paste0("\nExtracting ", y$title, " (", y$symbol, "): "))
      fsheet_extract_single(x, y, errors = errors)
    }))
    out %>% arrange(symbol, measurement_datetime)
  }
}


fsheet_extract_single <- function(x, fsheet_def, errors = stop){
  out <- x %>%
    filter(name %in% fsheet_def$names)

  out <- out %>%
    mutate(symbol = fsheet_def$symbol, .after = person_id) %>%
    mutate(title = fsheet_def$title, .after = measurement_datetime) %>%
    relocate(name, .after = measurement_datetime) %>%
    mutate(type = fsheet_def$type)

  unexpected <- out %>%
    filter(!(!!fsheet_def$expect_before))
  if (nrow(unexpected)){
    unexpected <- unexpected %>%
      select(name, value, comment, template, form)
    errors("unexpected cases\n", print(unexpected))
  }

  nrow_data_original <- nrow(out)
  out <- out %>%
    distinct
  nrow_data_distinct <- nrow(out)
  if (nrow_data_original != nrow_data_distinct){
    cat(nrow_data_original - nrow_data_distinct, "duplicate rows discarded. ")
  }

  out <- out %>%
    mutate(will_silently_exclude_na = (is.na(value) & !!fsheet_def$silently_exclude_na_when))

  n_silently_exclude_na <- nrow(out) - sum(!out$will_silently_exclude_na)
  if (n_silently_exclude_na > 0){
    cat(n_silently_exclude_na, "NAs excluded. ")
  }

  out <- out %>%
    filter(!will_silently_exclude_na) %>%
    mutate(will_silently_exclude = (!!fsheet_def$silently_exclude_when))

  n_silently_exclude <- nrow(out) - sum(!out$will_silently_exclude)
  if (n_silently_exclude > 0){
    cat(n_silently_exclude, "excluded. ")
  }

  out <- out %>%
    filter(!will_silently_exclude)

  out <- out %>%
    group_by(name) %>%
    mutate(
      value_original = value,
      value_as_character = suppressWarnings({
        as.character(value)
      }),
      value_as_number = suppressWarnings({
        as.numeric(value)
      }),
      value_as_character = !!fsheet_def$value_as_character_fn,
      value_as_number = !!fsheet_def$value_as_number_fn,
      censoring = !!fsheet_def$censoring_fn,
      .after = value) %>%
    relocate(censoring, .after = value_as_number) %>%
    ungroup

  if (fsheet_def$type == "numeric"){
    nonnumeric <- fsheet_nonnumeric(out)
    if (nrow(nonnumeric) > 0){
      errors("*** Non-numeric value found ***\n",
           print(nonnumeric %>%
                   group_by(value_original) %>%
                   select(value_original, value_as_number,
                          value_as_character, value) %>%
                   distinct),
           "\n")
    }
  }

  out <- out %>%
    mutate(value_as_number = !!fsheet_def$unit_rescale_fn,
           unit = !!fsheet_def$unit_relabel_fn)

  if (!is.na(fsheet_def$range_discard_above)){
    nrow_data_prior_too_high <- nrow(out)
    out <- out %>%
      mutate(is_too_high = value_as_number > fsheet_def$range_discard_above) %>%
      filter(!is_too_high)
    n_discard_too_high <- nrow_data_prior_too_high - nrow(out)
    if (n_discard_too_high > 0){
      cat(paste0(n_discard_too_high,
                 " excluded since >",
                 fsheet_def$range_discard_above,
                 ". "))
    }
  } else {
    out <- out %>%
      mutate(is_too_high = FALSE)
  }

  if (!is.na(fsheet_def$range_discard_below)){
    nrow_data_prior_too_low <- nrow(out)
    out <- out %>%
      mutate(is_too_low = value_as_number < fsheet_def$range_discard_below) %>%
      filter(!is_too_low)
    n_discard_too_low <- nrow_data_prior_too_low - nrow(out)
    if (n_discard_too_low > 0){
      cat(paste0(n_discard_too_low,
                 " excluded since <",
                 fsheet_def$range_discard_below,
                 ". "))
    }
  } else {
    out <- out %>%
      mutate(is_too_low = FALSE)
  }

  unexpected <- out %>%
    filter(!(!!fsheet_def$expect_after))
  if (nrow(unexpected)){
    unexpected <- unexpected %>%
      select(name, value, comment, template, form)
   errors("unexpected cases\n", print(unexpected))
  }

  cat(nrow(out), "extracted.")
  out %>%
    select(-will_silently_exclude, -will_silently_exclude_na,
           -value, -value_original, -is_too_high, -is_too_low) %>%
    arrange(measurement_datetime)
}


#' Helper function to extract nonnumeric fsheet values
#'
#' @param x A flowsheet data frame, with `value_as_number` column
#'
#' @return The rows of the supplied data frame that are `NA` after conversion to
#'   to numeric form
#' @author R.J.B. Goudie
fsheet_nonnumeric <- function(x){
  value <- x$value_as_number
  value_numeric <- suppressWarnings({
    as.numeric(value)
  })
  value_is_nonnumeric <- is.na(value_numeric)
  x[value_is_nonnumeric, ]
}
