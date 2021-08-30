condition_raw <- function(x, y){
  bind_rows(x, y)
}

#' Add a new definition of a Diagnosis PL item
#'
#' @param condition_def A list of existing condition definitions to add
#'   this to
#' @param symbol A short name for the condition item. This should be a
#'   character vector that is suitable to use as an R object name
#' @param title The full name for the condition item, suitable for using in
#'   figure titles etc
#' @param icd_10 A vector of ICD-10 codes to use.
#' @param silently_exclude_deleted When TRUE, diagnoses with
#'   `status = "Deleted"` will be removed from the data frame. In theory, only
#'   diagnoses made in error should be deleted, but in practice it is thought
#'   that clinical staff often delete diagnoses that are resolved rather than
#'   mark them as "Resolved".
#' @author R.J.B. Goudie
condition_add <- function(condition_def,
                       symbol,
                       title,
                       icd10 = NA,
                       condition_start_datetime_fn =
                         case_when(!is.na(diagnosis_datetime) ~ diagnosis_datetime,
                                   !is.na(entered_datetime) ~ entered_datetime,
                                   TRUE ~ ymd_hms(NA)),
                       condition_end_datetime_fn =
                         case_when(!is.na(resolved_datetime) ~ resolved_datetime,
                                   TRUE ~ ymd_hms(NA)),
                       # Need to allow filtering by description
                       # description_search,
                       # description_exclude = NA,
                       silently_exclude_deleted_when = TRUE,
                       silently_exclude_resolved_when = FALSE){
  condition_start_datetime_fn <- enquo(condition_start_datetime_fn)
  condition_end_datetime_fn <- enquo(condition_end_datetime_fn)
  silently_exclude_deleted_when <- enquo(silently_exclude_deleted_when)
  silently_exclude_resolved_when <- enquo(silently_exclude_resolved_when)

  new <- list(symbol = symbol,
              title = title,
              icd10 = icd10,
              silently_exclude_deleted_when = silently_exclude_deleted_when,
              silently_exclude_resolved_when = silently_exclude_resolved_when,
              condition_start_datetime_fn = condition_start_datetime_fn,
              condition_end_datetime_fn = condition_end_datetime_fn)
  condition_def <- append(condition_def, list(new))
  names(condition_def)[length(condition_def)] <- symbol
  condition_def
}

#' Extract condition data into tidy format
#'
#' @param x condition data in tidy-column-name format (after applying
#'   `condition_rename`)
#' @param condition_def A condition definition
#' @param errors A function indicating what to do when an error is found
#'
#' @return
#' A data frame with the following columns:
#' `person_id`, `symbol`, `value_as_number`, `value_as_character`,
#' `censoring`, `ordered_datetime`, `collected_datetime`, `received_datetime`,
#' `result_datetime`, `ordering_department_name`, `range_low`, `range_high`,
#' `unit`, `name`, `title`, `method`, `group`, `type`
#'
#' The result will be sorted by diagnosis_datetime
#' @author R.J.B. Goudie
condition_extract <- function(x, condition_def, errors = stop){
  if (length(condition_def) == 1 & "symbol" %in% names(condition_def)){
    condition_extract_single(x, condition_def)
  } else {
    out <- bind_rows(lapply(condition_def, function(y){
      inform(format_error_bullets(c(
        glue("\nExtracting {y$title} ({y$symbol})"))))
      condition_extract_single(x, y, errors = errors)
    }))
    out %>% arrange(symbol, diagnosis_datetime)
  }
}

condition_extract_single <- function(x, condition_def, errors = stop){

  #  possible_new <- condition_check_for_new(x, condition_def)
  # if (nrow(possible_new) > 0){
  #   possible_new_names <- str_flatten(possible_new$name, "; ")
  #   warning(format_error_bullets(c(
  #     i = glue("{nrow(possible_new)} possible new condition names: ",
  #              "{possible_new_names}"))),
  #     immediate. = TRUE)
  # }

  # Filter to only ICD-10s
  out <- x %>%
    filter(str_detect(icd10_list, condition_def$icd10))

  # Add symbol and title
  out <- out %>%
    mutate(symbol = condition_def$symbol, .after = person_id) %>%
    mutate(title = condition_def$title, .after = snomed) %>%
    mutate(value_as_logical = TRUE)

  # Remove duplicate rows
  out <- out %>%
    distinct_inform

  # Exclude Deleted when requested
  out <- out %>%
    mutate(will_silently_exclude_deleted =
             (type == "problem_list") &
             (!! condition_def$silently_exclude_deleted_when) &
             (status == "Deleted")) %>%
    filter_inform(!will_silently_exclude_deleted,
                  since = "since diagnoses were \"Deleted\"")

  out <- out %>%
    filter(!will_silently_exclude_deleted)

  # Exclude Resolved when requested
  out <- out %>%
    mutate(will_silently_exclude_resolved =
             (type == "problem_list") &
             (!! condition_def$silently_exclude_resolved_when) &
             (status == "Resolved")) %>%
    filter_inform(!will_silently_exclude_resolved,
                  since = "since diagnoses were \"Resolved\"")

  # Impute start and end dates
  out <- out %>%
  mutate(
    condition_start_datetime = !! condition_def$condition_start_datetime_fn,
    condition_end_datetime = !! condition_def$condition_end_datetime_fn,
    .after = status)

  # Return result
  inform(format_error_bullets(c(i = glue("{nrow(out)} rows extracted"))))
  out %>%
    select(-will_silently_exclude_deleted) %>%
    arrange(diagnosis_datetime)
}
