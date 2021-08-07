#' Tidy raw diagnosis_pl colnames
#' @param x A data frame of raw diagnosis_pl data
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
diagnosis_pl_rename <- function(x){
  x %>%
    rename(person_id = "STUDY_SUBJECT_DIGEST",
           description = "DX_DESCRIPTION",
           description_displayed = "DX_DESC_DISPLAYED",
           comment = "PROBLEM_CMT",
           entered_date = "DIAGNOSIS_ENTERED_DATE",
           diagnosis_date = "DIAGNOSIS_DATE",
           resolved_date = "RESOLVED_DATE",
           status = "DIAGNOSIS_STATUS",
           icd10_1 = "ICD10_1",
           icd10_2 = "ICD10_2",
           icd10_3 = "ICD10_3",
           icd10_4 = "ICD10_4",
           icd10_list = "ICD10_LIST",
           snomed = "SNOMED_CONCEPTS")
}

#' Untidy raw diagnosis_pl colnames
#' @param x A data frame of raw diagnosis_pl data with tidy names
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
diagnosis_pl_unrename <- function(x){
  x %>%
    rename(STUDY_SUBJECT_DIGEST = "person_id",
           DX_DESCRIPTION = "description",
           DX_DESC_DISPLAYED = "description_displayed",
           PROBLEM_CMT = "comment",
           DIAGNOSIS_ENTERED_DATE = "entered_date",
           DIAGNOSIS_DATE = "diagnosis_date",
           RESOLVED_DATE = "resolved_date",
           DIAGNOSIS_STATUS = "status",
           ICD10_1 = "icd10_1",
           ICD10_2 = "icd10_2",
           ICD10_3 = "icd10_3",
           ICD10_4 = "icd10_4",
           ICD10_LIST = "icd10_list",
           SNOMED_CONCEPTS = "snomed")
}

#' Add a new definition of a Diagnosis PL item
#'
#' @param diagnosis_pl_def A list of existing diagnosis_pl definitions to add
#'   this to
#' @param symbol A short name for the diagnosis_pl item. This should be a
#'   character vector that is suitable to use as an R object name
#' @param title The full name for the diagnosis_pl item, suitable for using in
#'   figure titles etc
#' @param icd_10 A vector of ICD-10 codes to use.
#' @param silently_exclude_deleted When TRUE, diagnoses with
#'   `status = "Deleted"` will be removed from the data frame. In theory, only
#'   diagnoses made in error should be deleted, but in practice it is thought
#'   that clinical staff often delete diagnoses that are resolved rather than
#'   mark them as "Resolved".
#' @author R.J.B. Goudie
diagnosis_pl_add <- function(diagnosis_pl_def,
                      symbol,
                      title,
                      icd10 = NA,
                      # Need to allow filtering by description
                      # description_search,
                      # description_exclude = NA,
                      silently_exclude_deleted = TRUE){

  new <- list(symbol = symbol,
              title = title,
              icd10 = icd10,
              silently_exclude_deleted = silently_exclude_deleted)
  diagnosis_pl_def <- append(diagnosis_pl_def, list(new))
  names(diagnosis_pl_def)[length(diagnosis_pl_def)] <- symbol
  diagnosis_pl_def
}

#' Extract diagnosis_pl data into tidy format
#'
#' @param x Diagnosis_pl data in tidy-column-name format (after applying
#'   `diagnosis_pl_rename`)
#' @param diagnosis_pl_def A diagnosis_pl definition
#' @param errors A function indicating what to do when an error is found
#'
#' @return
#' A data frame with the following columns:
#' `person_id`, `symbol`, `value_as_number`, `value_as_character`,
#' `censoring`, `ordered_datetime`, `collected_datetime`, `received_datetime`,
#' `result_datetime`, `ordering_department_name`, `range_low`, `range_high`,
#' `unit`, `name`, `title`, `method`, `group`, `type`
#'
#' The result will be sorted by diagnosis_date
#' @author R.J.B. Goudie
diagnosis_pl_extract <- function(x, diagnosis_pl_def, errors = stop){
  if (length(diagnosis_pl_def) == 1 & "symbol" %in% names(diagnosis_pl_def)){
    diagnosis_pl_extract_single(x, diagnosis_pl_def)
  } else {
    out <- bind_rows(lapply(diagnosis_pl_def, function(y){
      inform(format_error_bullets(c(
        glue("\nExtracting {y$title} ({y$symbol})"))))
      diagnosis_pl_extract_single(x, y, errors = errors)
    }))
    out %>% arrange(symbol, diagnosis_date)
  }
}

diagnosis_pl_extract_single <- function(x, diagnosis_pl_def, errors = stop){

#  possible_new <- diagnosis_pl_check_for_new(x, diagnosis_pl_def)
  # if (nrow(possible_new) > 0){
  #   possible_new_names <- str_flatten(possible_new$name, "; ")
  #   warning(format_error_bullets(c(
  #     i = glue("{nrow(possible_new)} possible new diagnosis_pl names: ",
  #              "{possible_new_names}"))),
  #     immediate. = TRUE)
  # }

  # Filter to only ICD-10s
  out <- x %>%
    filter(str_detect(icd10_list, diagnosis_pl_def$icd10))

  # Add symbol and title
  out <- out %>%
    mutate(symbol = diagnosis_pl_def$symbol, .after = person_id) %>%
    mutate(title = diagnosis_pl_def$title, .after = snomed)

  # Remove duplicate rows
  nrow_data_original <- nrow(out)
  out <- out %>%
    distinct
  nrow_data_distinct <- nrow(out)
  if (nrow_data_original != nrow_data_distinct){
    inform(format_error_bullets(c(
      i = glue("{nrow_data_original - nrow_data_distinct}",
               "duplicate rows discarded"))))
  }

  # Exclude Deleted when requested
  if (silently_exclude_deleted){
    out <- out %>%
      mutate(will_silently_exclude_deleted = status == "Resolved")

      n_silently_exclude_na <- nrow(out) - sum(!out$will_silently_exclude_na)
      if (n_silently_exclude_na > 0){
        inform(format_error_bullets(c(
          i = glue("{n_silently_exclude_na} NAs excluded"))))
      }

    out <- out %>%
      filter(!will_silently_exclude_deleted)
  }

  # Return result
  inform(format_error_bullets(c(i = glue("{nrow(out)} rows extracted"))))
  out %>%
    select(-will_silently_exclude_deleted) %>%
    arrange(diagnosis_date)
}
