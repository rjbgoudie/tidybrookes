#' Tidy raw radiology colnames
#' @param x A data frame of raw radiology data
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
radiology_rename <- function(x){
  x %>%
    rename(person_id = "STUDY_SUBJECT_DIGEST",
           name = "Proc_Name",
           procedure_datetime = "Proc_Date",
           code = "Proc_Code",
           narrative = "Proc_Narrative",
           impression = "Proc_Impression",
           addenda = "Proc_Addenda",
           assessment = "Proc_Assessment") %>%
    relocate(person_id,
             name,
             procedure_datetime,
             narrative,
             impression,
             addenda,
             assessment,
             code)
}

#' Untidy raw radiology colnames
#' @param x A data frame of raw radiology data with tidy names
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
radiology_unrename <- function(x){
  x %>%
    rename(STUDY_SUBJECT_DIGEST = "person_id",
           Proc_Name = "name",
           Proc_Date = "procedure_datetime",
           Proc_Code = "code",
           Proc_Narrative = "narrative",
           Proc_Impression = "impression",
           Proc_Addenda = "addenda",
           Proc_Assessment = "assessment")
}


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
radiology_extract <- function(x, radiology_def, errors = stop){
  if (length(radiology_def) == 1 & "symbol" %in% names(radiology_def)){
    radiology_extract_single(x, radiology_def)
  } else {
    out <- bind_rows(lapply(radiology_def, function(y){
      inform(format_error_bullets(c(
        glue("\nExtracting {y$title} ({y$symbol})"))))
      radiology_extract_single(x, y, errors = errors)
    }))
    out %>% arrange(symbol, administered_datetime)
  }
}

radiology_extract_single <- function(x, radiology_def, errors = stop){
  ## stop("This is not properly implemented yet")
  ## possible_new <- radiology_check_for_new(x, radiology_def)
  ## if (nrow(possible_new) > 0){
  ##   possible_new_names <- str_flatten(possible_new$name, "; ")
  ##   warning(format_error_bullets(c(
  ##     i = glue("{nrow(possible_new)} possible new radiology names: ",
  ##              "{possible_new_names}"))),
  ##     immediate. = TRUE)
  ## }

  # Filter to only CUH radiology
  out <- x %>%
    filter(name %in% radiology_def$names)

  # Add symbol and title
  out <- out %>%
    mutate(symbol = radiology_def$symbol, .after = person_id) %>%
    mutate(title = radiology_def$title, .after = code)

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

  out <- out %>%
    group_by(name) %>%
    mutate(
      value_as_character = !!radiology_def$value_as_character_fn,
      .after = procedure_datetime) %>%
    ungroup

  # Return result
  inform(format_error_bullets(c(i = glue("{nrow(out)} rows extracted"))))
  out %>%
    arrange(procedure_datetime)
}
