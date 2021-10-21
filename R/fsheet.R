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
#' @param value_as_logical_fn A function specifying how to generate the
#'   `value_as_logical` column
#' @param unit_rescale_fn A function specifying how to rescale the flowsheet
#'   values. By default, does not rescale
#' @param unit_relabel_fn A function specifying how to relabel the units of the
#'   the flowsheet values. By default, does not rescale
#' @param coalesce_fn A function specifying how to combine measurements from
#'   exactly the same time
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
                         if_else(type == "character",
                                 case_when(TRUE ~ value_as_character),
                                 case_when(TRUE ~ NA_character_)),
                       value_as_logical_fn =
                         if_else(type == "logical",
                                 case_when(TRUE ~ value_as_logical),
                                 case_when(TRUE ~ as.logical(NA))),
                       unit_rescale_fn = case_when(TRUE ~ value_as_number),
                       unit_relabel_fn = case_when(TRUE ~ NA_character_),
                       coalesce_fn = identity,
                       expect_before = TRUE,
                       expect_after = TRUE,
                       range_mainly_low = NA,
                       range_mainly_high = NA,
                       range_discard_below = NA,
                       range_discard_above = NA){
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
              names = names,
              search_pattern = search_pattern,
              search_exclude = search_exclude,
              type = type,
              silently_exclude_na_when = silently_exclude_na_when,
              silently_exclude_when = silently_exclude_when,
              censoring_fn = censoring_fn,
              value_as_number_fn = value_as_number_fn,
              value_as_character_fn = value_as_character_fn,
              value_as_logical_fn = value_as_logical_fn,
              unit_rescale_fn = unit_rescale_fn,
              unit_relabel_fn = unit_relabel_fn,
              coalesce_fn = coalesce_fn,
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
#' `person_id`, `symbol`, `value_as_character`, `value_as_number`,
#' `value_as_logical`, `censoring`, `comment`, `measurement_datetime`, `name`,
#' `title`, `data_id`, `measurement_id`, `line_id`, `template`, `form`, `type`,
#' `unit`
#' @author R.J.B. Goudie
fsheet_extract <- function(x, fsheet_def, errors = stop){
  if (length(fsheet_def) == 1 & "symbol" %in% names(fsheet_def)){
    fsheet_extract_single(x, fsheet_def)
  } else {
    out <- bind_rows(lapply(fsheet_def, function(y){
      fsheet_extract_single(x, y, errors = errors)
    }))
    out %>% arrange(symbol, measurement_datetime)
  }
}


fsheet_extract_single <- function(x, fsheet_def, errors = stop){
  out <- x %>%
    filter(name %in% fsheet_def$names)

  inform(format_error_bullets(c(
    glue("\nExtracting {fsheet_def$title} ({fsheet_def$symbol}) from ",
         "{nrow(out)} raw rows"))))

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
      value_as_logical = suppressWarnings({
        as.logical(value_original)
      }),
      value_as_character = !!fsheet_def$value_as_character_fn,
      value_as_number = !!fsheet_def$value_as_number_fn,
      value_as_logical = !!fsheet_def$value_as_logical_fn,
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

  coalesce_out <- function(x){
    x %>%
      group_by(person_id, measurement_datetime) %>%
      (fsheet_def$coalesce_fn)() %>%
      ungroup()
  }
  # Handle coalesce
  out <- fn_inform(out, coalesce_out, since = "due to coalescing")

  # Check expect_after condition
  check_that_all(out, !!fsheet_def$expect_after, "expect_after")

  # Return result
  inform(format_error_bullets(c(i = glue("{nrow(out)} rows extracted"))))
  out %>%
    select(-will_silently_exclude, -will_silently_exclude_na,
           -value_original, -is_too_high, -is_too_low) %>%
    arrange(measurement_datetime)
}

fsheet_infer_fio2 <- function(x){
  Cannula <- c("Nasal cannula")
  Mask <- c("Humidified mask (heated)", "Humidified mask(cold)", "Simple mask")
  MaskRes <- c("Non-rebreather mask")

  fio2_app_cannula <- approxfun(x = 0:6,
                                y = c(0.21, 0.24, 0.28, 0.32, 0.36, 0.40, 0.44),
                                method = "linear",
                                rule = 2)
  fio2_app_mask <- approxfun(x = c(0, 5, 6.5, 7.5),
                             y = c(0.21, 0.4, 0.5, 0.6),
                             method = "linear",
                             rule = 2)
  fio2_app_maskres <- approxfun(x = c(0, 6, 7, 8, 9, 10),
                                y = c(0.21, 0.6, 0.7, 0.8, 0.9, 0.95),
                                method = "linear",
                                rule = 2)

  fio2_app_venturi <- function(o2_flow_rate){
    case_when(o2_flow_rate == 0 ~ 0.21,
              o2_flow_rate == 2 ~ 0.24,
              o2_flow_rate == 4 ~ 0.28,
              o2_flow_rate == 6 ~ 0.31,
              o2_flow_rate == 8 ~ 0.35,
              o2_flow_rate == 10 ~ 0.4,
              o2_flow_rate == 15 ~ 0.6,
              TRUE ~ NA_real_)
  }

  feasible_venturi_fio2 <- c(0.21, 0.24, 0.28, 0.31, 0.35, 0.4, 0.6)

  infer_fio2 <- function(fio2, o2_device, o2_flow_rate){
    case_when(
      # Venturi mask
      # (a) if recorded fio2 is feasible, then use it
      o2_device == "Venturi Mask" &
        !is.na(fio2) &
        fio2 %in% feasible_venturi_fio2 ~ fio2,

      # (b) if recorded fio2 not feasible, and then convert flow rate
      o2_device == "Venturi Mask"  ~ fio2_app_venturi(o2_flow_rate),

      # Except for Venturi (since only 7 feasible values), use recorded fio2
      # wherever available
      o2_device != "Venturi Mask" & !is.na(fio2) ~ fio2,

      # Convert various masks using
      # https://www.intensive.org/epic2/Documents/Estimation%20of%20PO2%20and%20FiO2.pdf
      is.na(fio2) & o2_device %in% Cannula ~ fio2_app_cannula(o2_flow_rate),
      is.na(fio2) & o2_device %in% Mask ~ fio2_app_mask(o2_flow_rate),
      is.na(fio2) & o2_device %in% MaskRes ~ fio2_app_maskres(o2_flow_rate),
      is.na(fio2) & o2_device == "None (Room air)" ~ 0.21,

      # If not o2_device data, then assume cannula if plausible o2_flow_rate
      # (this will not overestimate fio2)
      is.na(fio2) & is.na(o2_device) &
        o2_flow_rate <= 6 ~ fio2_app_cannula(o2_flow_rate),

      # Otherwise discard, since unclear what it means
      is.na(fio2) & is.na(o2_device) &
        o2_flow_rate > 6 ~ NA_real_
    )
  }

  x %>%
    mutate(fio2_inferred = infer_fio2(fio2, o2_device, o2_flow_rate))
}


# Largely copied from summarise_pivot_wider(), need to unify all this
fsheet_pivot_wider <- function(x,
                               id_cols = c("person_id", "measurement_datetime"),
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
  values_from <- if ("datetime" %in% colnames(x)){
    values_from
  } else {
    setdiff(values_from, "datetime")
  }

  names_glue <- paste0(magic_prefix,  "{",
                       names_from, "}",
                       names_suffix,
                       "_{.value}")

  out_split <- split_by_type(x)
  anything <-
    out_split$any_numeric ||
    out_split$any_character ||
    out_split$any_logical

  out_numeric <- NULL
  out_character <- NULL
  out_logical <- NULL

  if (out_split$any_numeric){
    out_numeric <- out_split$numeric %>%
      pivot_wider(
        id_cols = all_of(id_cols),
        names_from = all_of(names_from),
        values_from = all_of(setdiff(values_from, "value_as_character")),
        names_glue = names_glue)
  }

  if (out_split$any_character){
    out_character <- out_split$character %>%
      pivot_wider(
        id_cols = all_of(id_cols),
        names_from = all_of(names_from),
        values_from = all_of(setdiff(values_from, "value_as_number")),
        names_glue = names_glue)
  }

  if (out_split$any_logical){
    out_logical <- out_split$logical %>%
      pivot_wider(
        id_cols = all_of(id_cols),
        names_from = all_of(names_from),
        values_from = all_of(setdiff(values_from, "value_as_logical")),
        names_glue = names_glue)
  }

  if (anything){
    out <- list(out_numeric, out_character, out_logical) %>%
      compact %>%
      reduce(full_join)
  } else {
    stop("Neither numeric or character output from slice/summary found")
  }

  relocate_and_clean_new_cols(out, magic_prefix = magic_prefix)
}

fsheet_pivot_longer <- function(x){
  x %>%
    tidyr:::pivot_longer(cols = !c(person_id, measurement_datetime) & where(is.numeric),
                         names_to = "symbol",
                         values_to = "value_as_number")
}

fsheet_sf_ratio <- function(x, shape = "long"){
  out <- x %>%
    filter(symbol %in% c("fio2", "o2_device", "o2_flow_rate", "spo2")) %>%
    fsheet_pivot_wider(values_from = c("value_as_number", "value_as_character")) %>%
    fsheet_infer_fio2()

  out <- out %>%
    mutate(fio2_inferred =
             case_when(!is.na(fio2_inferred) ~ fio2_inferred,
                       is.na(fio2_inferred) & spo2 >= 88 ~ 0.21),
           spo2_fio2_inferred_ratio = spo2/fio2_inferred,
           spo2_fio2_ratio = spo2/fio2)

  if (shape == "long"){
    out %>%
      select(person_id,
             measurement_datetime,
             spo2_fio2_inferred_ratio,
             spo2_fio2_ratio) %>%
      pivot_longer(cols = !c(person_id, measurement_datetime) & where(is.numeric),
                   names_to = "symbol",
                   values_to = "value_as_number") %>%
      mutate(type = "numeric")
  } else {
    out
  }
}
