
#' Add a new definition of a radiology item
#'
#' @param radiology_def A list of existing radiology definitions to add this to
#' @param symbol A short name for the radiology item. This should be a
#'   character vector that is suitable to use as an R object name
#' @param title The full name for the radiology item, suitable for using in
#'   figure titles etc
#' @param names The names of the procedure names (`Proc_Name` in raw format)
#' @param search_pattern A charater vector of potential synonymns for this
#'   radiology item, in case new radiology names (`Proc_Name` in raw format)
#'   for the same radiology item are added in future.
#' @param search_exclude A character vector of radiology names
#'   (`Proc_Name` in raw format) that are known NOT to be relevant.
#' @param value_as_character_fn A function specifying how to generate the
#'   `value_as_character` column
#' @author R.J.B. Goudie
#' @rdname radiology_extract
#' @export
radiology_add <- function(radiology_def,
                          symbol,
                          title,
                          names = NA,
                          search_pattern,
                          search_exclude = NA,
                          value_as_character_fn = narrative){
  value_as_character_fn <- enquo(value_as_character_fn)

  new <- list(symbol = symbol,
              title = title,
              names = names,
              search_pattern = search_pattern,
              search_exclude = search_exclude,
              value_as_character_fn = value_as_character_fn)
  radiology_def <- append(radiology_def, list(new))
  names(radiology_def)[length(radiology_def)] <- symbol
  radiology_def
}

#' Extract radiology data into tidy format
#'
#' @param x Radiology data in tidy-column-name format (after
#'   applying `radiology_rename`)
#' @param radiology_def A radiology definition
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
#' @rdname radiology_extract
#' @export
radiology_extract <- function(x, radiology_def, errors = stop){
  if (length(radiology_def) == 1 & "symbol" %in% names(radiology_def)){
    radiology_extract_single(x, radiology_def)
  } else {
    out <- bind_rows(lapply(radiology_def, function(y){
      cli::cli_alert_info("Extracting {y$title} ({y$symbol})")
      radiology_extract_single(x, y, errors = errors)
    }))
    out %>% arrange(symbol, procedure_datetime)
  }
}

radiology_extract_single <- function(x, radiology_def, errors = stop){
  ## stop("This is not properly implemented yet")
  ## possible_new <- radiology_check_for_new(x, radiology_def)
  ## if (nrow(possible_new) > 0){
  ##   possible_new_names <- str_flatten(possible_new$name, "; ")
  ##   cli::cli_alert_warning(
  ##     c("{nrow(possible_new)} possible new radiology names: ",
  ##     "{possible_new_names}"))
  ## }

  # Filter to only CUH radiology
  out <- x %>%
    filter(name %in% radiology_def$names)

  # Add symbol and title
  out <- out %>%
    mutate(symbol = radiology_def$symbol, .after = person_id) %>%
    mutate(title = radiology_def$title, .after = code)

  # Remove duplicate rows
  out <- out %>%
    distinct_inform

  out <- out %>%
    group_by(name) %>%
    mutate(
      value_as_character = !!radiology_def$value_as_character_fn,
      .after = procedure_datetime) %>%
    ungroup

  # Return result
  cli::cli_alert_info("{nrow(out)} rows extracted")
  out %>%
    arrange(procedure_datetime)
}
