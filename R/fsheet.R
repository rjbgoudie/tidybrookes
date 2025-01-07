#' Add and extract flowsheet items
#'
#' `fsheet_add` creates a new "fsheet definition", which is a specification of
#' how the raw flowsheet data should be extracted, filtered, cleaned and
#' standardised. A list of such specifications can be used on raw `fsheet` data
#' using `fsheet_extract`.
#'
#' @param fsheet_def A list of existing fsheet definitions to add this to
#' @param symbol A short name for the flowsheet item. This should be a character
#'   vector that is suitable to use as an R object name
#' @param title The full name for the flowsheet item, suitable for using in
#'   figure titles etc
#' @param names A character vector of display names (`disp_name` in raw format)
#'   of the flowsheet items to include
#' @param search_pattern A character vector of potential synonymns for this
#'   flowsheet item, in case new names (`disp_name` in raw format) for the
#'   same item are added in future. These will be searched for in the raw data
#'   and if any potential new names are identified, the user will be alerted to
#'   the presence of potential new flowsheet names.
#' @param search_exclude A character vector of flowsheet item names
#'   (`disp_name` in raw format) that are known NOT to be relevant to this
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
#' @param coalesce_fn0 A function specifying how to combine measurements from
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
#'
#' @param x Flowsheet data in renamed format (after applying `fsheet_rename`)
#' @param fsheet_def A list of fsheet definitions
#' @param errors A function indicating what to do when an error is found
#'
#' @return
#' `fsheet_add` returns a list of fsheet definitions, with the new definition
#' added to the end of the supplied fsheet_def
#'
#' `fsheet_extract` returns a data frame with the following columns:
#' `person_id`, `symbol`, `value_as_character`, `value_as_number`,
#' `value_as_logical`, `censoring`, `comment`, `measurement_datetime`, `name`,
#' `title`, `data_id`, `measurement_id`, `line_id`, `template`, `form`, `type`,
#' `unit`
#'
#' @details
#' Flowsheet defintiions describe how the raw data should be extracted,
#' filtered, cleaned and standardised as follows:
#'
#' 1. **Labelling the flowsheet item**. Two names/labels are required each
#'   fsheet item:
#'     - a `symbol`, which serves as the "R variable name" for the item. To make
#'       the symbol quick and easy to type, and work with in R, it is
#'       recommended that these are lowercase, ideally single words (e.g.
#'       `"news2"` or "`spo2`)
#'     - a `title`, which serves as the label the of the flowsheet item in
#'       plots and figures. This should be capitalised as you would want the
#'       item to be labelled in formal documents (e.g. `"NEWS2"` or `"SpO2`).
#'
#' 1. **Extracting relevant flowsheet items** The specification of which
#'   rows should be extracted is via:
#'     - `names`, a vector listing the labels of the fsheet items in the raw
#'       data. This corresponds to column labelled as `name` by
#'       [`fsheet_rename`], which is labelled as `disp_name` in the raw
#'       extracts from Epic).
#'
#'    Additionally, to guard against the potential for new names for flowsheet
#'    items being introduced through the evolution of the EHR data, search terms
#'    for potential other names for this flowsheet items (and known exclusions)
#'    can be provided using:
#'
#'     - `search_pattern`, a vector of search terms of potential synonyms for
#'       the flowsheet item. This list of search terms will be searched
#'       whenever the flowsheet item is extracted, and if a possible new data
#'       field is identified, then this will be alerted to the user.
#'     - `search_exclude`, a vector of names of flowsheet items that are known
#'       _not_ to be relevant. This list of names will be removed prior to
#'       searching the data for the search patterns in `search_pattern`.
#'       This helps to avoid alerting the user about numerous flowsheet items
#'       that are known to be irrelevant.
#'
#' 1. **Excluding rows**. Rows containing `NA` in the `value` column may be
#'    always irrelevant, so it may be helpful to remove these by setting
#'    `silently_exclude_na_when` to `TRUE`.
#'
#'    Alternatively, an expression that describes when row should be excluded
#'    can be provided using `silently_exclude_when`. For example, setting this
#'    to `(value_original == "Other (Comment)")` will exclude all rows where the
#'    value is `"Other (Comment)"`.
#'
#'    For numerical flowsheet items,`range_discard_below` and
#'    `range_discard_above` can be specified so that values outside this range
#'    are excluded (see below for details).
#'
#' 1. **Converting values to numbers/characters/logicals**. Raw flowsheet data
#'   are a complex mix of numerical data (sometimes containing censoring, and
#'   other non-numerical data), categorical data (coded as characters) and
#'   logical data (usually coded as characters). Standardising the raw data
#'   into data of a single type makes downstream analysis easier, so several
#'   aspects of the definition specify how this is done:
#'     - `type`: one of `numeric`, `character` or `logical`. This is used to
#'       set sensible defaults for other aspects of the specification, and is
#'       added as a column to the output, so downstream handling of the data
#'       can use this column to determine handling of the data (e.g. default
#'       plots might be a barplot for `character` items and a lineplot for
#'       `numeric` items)
#'     - `value_as_number_fn`, `value_as_character_fn` and `value_as_logical_fn`
#'       enable functions that handle conversion of the raw data.
#'       **TODO add more details**
#'     - `unit_rescale_fn` and `unit_relabel_fn` enable functions that standardise
#'       the units to be provided
#'     - `coalesce_fn` enables a function that combines together data to be
#'       provided. For instance, this can be used to specify how simultaneous data
#'       should be combined: e.g. how should two blood pressure records at
#'       the exact same time be handled? The function can describe how to combine
#'       the information by e.g. taking the mean or max, or arbitrarily choosing
#'       one of the records.
#'
#' 1. **Validating that data conform to expectations**. Two parameters enable
#'    checking that the data comply with known restrictions. For example, we
#'    may know a finite range exists for a flowsheet item, and we want to check
#'    that all data fall within this range.
#'
#'    Alternatively, we may need to map each value of the raw data to a smaller
#'    range of values, and we need to be sure that our function for doing this
#'    handles all the values that are present in the raw data (i.e. no new
#'    values have been added).
#'
#'    Checks that should be performed on the raw data should be supplied as
#'    `expect_before`, whereas checks that should be performed on the cleaned
#'    data should be supplied as `expect_after`.
#'
#' 1. **Specification of ranges**. Two types of ranges can be specified:
#'     - `range_discard_below` and `range_discard_above`: these are intended to
#'       describe _hard_ limits: values outside this range will be _excluded_.
#'     - `range_mainly_low` and `range_mainly_high`: these are intended to
#'       describe the range of values we typically expect most of the data to
#'       fall within, which can be useful for setting default ranges for e.g.
#'       default plots. Values outside this range are _not excluded_.
#'
#'
#' @examples
#' # A definition for the NEWS2 score:
#' # - Includes the value of the flowsheet called "NEWS 2 score"
#' # - Excludes all NA values, and also " " and "E".
#' # - The value_as_number_fn will convert "3mm" to 3, since this is probably
#' #   a typo
#' # - Checks that all output values are in integers between 0 and 17.
#' fsheet_def <- list() %>%
#'   fsheet_add(
#'     symbol = "news2",
#'     title = "NEWS2",
#'     names = c("NEWS2 score"),
#'     search_pattern = c("news2", "news"),
#'     search_exclude = c(),
#'     silently_exclude_na_when = TRUE,
#'     silently_exclude_when =
#'       (value_original == " " |
#'          value_original == "E" # some kind of typo
#'       ),
#'     value_as_number_fn =
#'       case_when(value_original == "3mm" ~ 3,
#'                 TRUE ~ value_as_number),
#'     expect_after =
#'       (value_as_number %in% 0:17))
#'
#' # Note that person_id == "BB" has a NEWS2 with value="3mm", and that
#' # person_id == "CC" has a NEWS2 with NA value
#' fsheet_raw_example %>% print(n = Inf)
#'
#' # Note that the NA row is excluded, and that "3mm" is converted to 3
#' fsheet_extract(fsheet_raw_example, fsheet_def)
#' @export
#' @rdname fsheet_extract
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
                       coalesce_fn0 = identity,
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
              coalesce_fn0 = coalesce_fn0,
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

#' Convert fsheet_def to data frame
fsheet_info <- function(fsheet_def, exclude_lists = FALSE){
  needs_wrapping_in_list <- function(x){
    inherits(x, "quosure") |
      inherits(x, "function") |
      is.null(x) |
      length(x) > 1
  }

  # need to wrap anything of length 2 etc in a list before
  # converting into a tibble. which items are length 2 etc
  # may differ between defs, so need to take union of all such
  # so the types are constant in columns of the tibble
  needs_wrapping_by_symbol <- map(fsheet_def, ~names(keep(., needs_wrapping_in_list)))
  needs_wrapping <- unique(unlist(needs_wrapping_by_symbol))
  fsheet_def2 <- map(fsheet_def, ~map_at(., needs_wrapping, ~list(.)))

  out <- map_dfr(fsheet_def2, ~as_tibble(.))

  if (exclude_lists){
    out %>%
      select(where(\(x) !is_list(x) & length(x) == 1))
  } else{
    out
  }
}

#' @rdname fsheet_extract
#' @export
fsheet_extract <- function(x,
                           fsheet_def,
                           errors = stop,
                           rds_only = FALSE,
                           rds_filepath_fn = NULL){
  fsheet_def <- wrap_def_if_single(fsheet_def)

  if (!rds_only){
    out <- bind_rows(lapply(fsheet_def, function(y){
      fsheet_extract_single(x, y, errors = errors)
    }))
    out %>% arrange(symbol, measurement_datetime)
  } else if (rds_only){
    lapply(fsheet_def, function(y){
      symbol <- y$symbol
      out <- fsheet_extract_single(x, y, errors = errors) %>%
        arrange(symbol, measurement_datetime)
      saveRDS(out,
              file = rds_filepath_fn(symbol))
    })
  }
}

fsheet_extract_single <- function(x, fsheet_def, errors = stop){
  out <- tribble(~person_id, ~symbol, ~name, ~measurement_datetime, ~value)

  if (inherits(x, "data.frame")){
    out <- x %>%
      filter(name %in% fsheet_def$names)
  } else if (inherits(x, "character")){
    symbol <- fsheet_def$symbol
    path <- x[symbol]
    if (!file.exists(path)){
    } else {
    if (str_ends(x,".csv") || str_ends(x,".csv.gz")){
      out <- read_csv(path, show_col_types = FALSE)
    } else {
      out <- readRDS(path)
    }
    }
  } else if (inherits(x, "tbl_sql")){
    out <- x %>%
      filter(name %in% fsheet_def$names) %>%
      collect()
  }

  cli::cli_alert_info(
    c("Extracting {fsheet_def$title} ",
      "({fsheet_def$symbol}) from {nrow(out)} raw rows"))

  # Add symbol and title
  out <- out %>%
    mutate(symbol = fsheet_def$symbol, .after = person_id) %>%
    mutate(title = fsheet_def$title, .after = measurement_datetime) %>%
    relocate(name, .after = measurement_datetime) %>%
    rename(value_original = value) %>%
    mutate(type = fsheet_def$type)

  # Check expect_before condition
  check_that_all(out,
                 !!fsheet_def$expect_before,
                 "expect_before",
                 summary = function(x){
                   x %>% count(value_original)
                 })

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
      (fsheet_def$coalesce_fn0)() %>%
      ungroup()
  }
  # Handle coalesce
  out <- fn_inform(out, coalesce_out, since = "due to coalescing")

  # Check expect_after condition
  check_that_all(out, !!fsheet_def$expect_after, "expect_after")

  # Return result
  cli::cli_alert_info("{nrow(out)} rows extracted")
  out %>%
    select(-will_silently_exclude, -will_silently_exclude_na,
           -value_original, -is_too_high, -is_too_low) %>%
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
    arrange(measurement_datetime)
}

#' Add annotations to fsheet data
#'
#' Annotate fsheet data to note rows that should be excluded etc.
#'
#' Annotations can either be added as additional columns (when
#' `annotation_db = NULL`), or stored in a separate database table (by
#' setting `annotation_db` to a database `tbl`).
#'
#' Note that when adding the annotations as additiona columns,
#' the result will not be sorted, since it is left in the original
#' order of `x`
#'
#' @param x Flowsheet data in renamed format (after applying `fsheet_rename`)
#' @param fsheet_def A fsheet definition, or list of fsheet definitions
#' @param annotation_db Either `NULL` or a `tbl` reference to the annotations
#'   database table that the annotations should be stored in. If `NULL` the
#'   annotations are added to `x` as additional columns.
#' @author R.J.B. Goudie
fsheet_annotate <- function(x,
                            fsheet_def,
                            annotation_db = NULL){
  fsheet_def <- wrap_def_if_single(fsheet_def)

  out <- lapply(fsheet_def, function(y){
    symbol <- y$symbol
    x_annotated <- fsheet_annotate_single(x, y, errors = errors)

    return_or_write_to_annotation_db(x,
                                     x_annotated,
                                     annotation_db,
                                     id_cols = "fsheet_id")
  })
  if (is.null(annotation_db)){
    x |>
      left_join(bind_rows(out))
  }
}


fsheet_annotation_schema <- function(){
  tibble(
    fsheet_id = integer(0),
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
    exclude_is_coalesced = logical(0),
    exclude_is_too_high = logical(0),
    exclude_is_too_low = logical(0),
    range_mainly_low = numeric(0),
    range_mainly_high = numeric(0),
    range_discard_below = numeric(0),
    range_discard_above = numeric(0),
    )
}

#' @export
fsheet_annotate_single <- function(x, fsheet_def, errors = stop){
  out <- x %>%
    filter(name %in% fsheet_def$names)

  if (inherits(x, "tbl_sql")){
    out <- collect(out)
  }
  out <- out %>%
    bind_rows(fsheet_annotation_schema())

  cli::cli_alert_info(
    c("Extracting {fsheet_def$title} ",
      "({fsheet_def$symbol}) from {nrow(out)} raw rows"))

  info <- fsheet_info(list(fsheet_def), exclude_lists = TRUE) |>
    select(symbol,
           title,
           type,
           range_mainly_low,
           range_mainly_high, range_discard_below, range_discard_above)
  # Add symbol and title
  out <- out %>%
    mutate(symbol = fsheet_def$symbol, .after = person_id) %>%
    rows_update(info, by = "symbol") %>%
    relocate(title, .after = measurement_datetime) %>%
    relocate(name, .after = measurement_datetime) %>%
    rename(value_original = value)

  # Check expect_before condition
  out <- label_check_that_all(out,
                 !!fsheet_def$expect_before,
                 label = "expect_before",
                 summary = function(x){
                   x %>% count(value_original)
                 })

  # Remove duplicate rows
  out <- out %>%
    exclusion_label_duplicates_inform(ignore_columns = fsheet_id)

  # Exclude NAs when requested
  out <- out %>%
    exclusion_label_condition_inform(
      exclude_is_silently_exclude_na,
      (is.na(value_original) & !!fsheet_def$silently_exclude_na_when),
      since = "since value was NA")

  # Exclude other rows when requested
  out <- out %>%
    exclusion_label_condition_inform(
      exclude_is_silently_exclude,
      (!!fsheet_def$silently_exclude_when),
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
    out <- out <- label_check_that_all(out,
                   suppressWarnings({!is.na(as.numeric(value_as_number))}),
                   label = "all_numeric")
  }

  # Rescale units
  out <- out %>%
    mutate(value_as_number = !!fsheet_def$unit_rescale_fn,
           unit = !!fsheet_def$unit_relabel_fn)

  # Discard too high values
  if (!is.na(fsheet_def$range_discard_above)){
    out <- out %>%
      exclusion_label_condition_inform(
        exclude_is_too_high,
        value_as_number > fsheet_def$range_discard_above,
        since = glue("since >{fsheet_def$range_discard_above}"))
  } else {
    out <- out %>%
      mutate(exclude_is_too_high = FALSE)
  }

  # Discard too low values
  if (!is.na(fsheet_def$range_discard_below)){
    out <- out %>%
      exclusion_label_condition_inform(
        exclude_is_too_low,
        value_as_number < fsheet_def$range_discard_below,
        since = glue("since <{fsheet_def$range_discard_below}"))
  } else {
    out <- out %>%
      mutate(exclude_is_too_low = FALSE)
  }

  coalesce_out <- function(x){
    x %>%
      mutate(exclude_is_coalesced = FALSE) %>%
      group_by(person_id, measurement_datetime) %>%
      (fsheet_def$coalesce_fn)() %>%
      ungroup()
  }
  # Handle coalesce
  out <- label_fn_inform(out,
                         coalesce_out,
                         inform_col = exclude_is_coalesced,
                         since = "due to coalescing")

  out <- out %>%
    mutate(exclude = if_any(starts_with("exclude_") & where(is_logical)))

  # Check expect_after condition
  out <- label_check_that_all(out,
                              exclude | (!!fsheet_def$expect_after),
                              label = "expect_after")

  # Return result
  out %>%
    select(-value_original) %>%
    arrange(measurement_datetime)
}

#' Infer FiO2
#'
#' @export
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

#' Pivot data wider, with multiple value type
#'
#' Pivot into `symbol` in columns, with each row a unique measurement time
#'
#' @param x A data frame
#' @param id_cols A set of columns that uniquely identifies each observation.
#' @param ... Passed to `pivot_value_wider`
#' @return A data frame
#'
#' @author R.J.B. Goudie
#' @export
fsheet_pivot_wider_datetime <-
  function(x,
           id_cols = c("person_id", "measurement_datetime"),
           ...){
  pivot_value_wider(x, id_cols, ...)
}

#' Pivot fsheet data longer
#'
#' @export
fsheet_pivot_longer <- function(x){
  x %>%
    tidyr::pivot_longer(cols = !c(person_id, measurement_datetime) & where(is.numeric),
                         names_to = "symbol",
                         values_to = "value_as_number")
}


#' Calculate SpO2/FiO2 ratio
#'
#' @export
fsheet_sf_ratio <- function(x, shape = "long"){
  out <- x %>%
    filter(symbol %in% c("fio2", "o2_device", "o2_flow_rate", "spo2")) %>%
    fsheet_pivot_wider_datetime(values_from = c("value_as_number", "value_as_character")) %>%
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
      tidyr::pivot_longer(cols = !c(person_id, measurement_datetime) & where(is.numeric),
                   names_to = "symbol",
                   values_to = "value_as_number") %>%
      mutate(type = "numeric") %>%
      filter(!is.na(value_as_number))
  } else {
    out
  }
}

#' Extract peripheral oxygen saturations (SpO2) on room air
#'
#' Excludes SpO2 measurements that are not from the same time as room air
#' @param x A fsheet_raw dataframe
#' @param shape Either "long" or "wide" format dataframe output
#' @export
fsheet_spo2_on_room_air <- function(x, shape = "long"){
  out <- x %>%
    filter(symbol %in% c("room_air", "spo2")) %>%
    fsheet_pivot_wider() %>%
    filter(room_air) %>%
    mutate(spo2_room_air = spo2)

  if (shape == "long"){
    out %>%
      select(person_id,
             measurement_datetime,
             spo2_room_air) %>%
      tidyr::pivot_longer(cols = !c(person_id, measurement_datetime) & where(is.numeric),
                   names_to = "symbol",
                   values_to = "value_as_number") %>%
      mutate(type = "numeric")
  } else {
    out
  }
}

#' Timepoints within timeranges
#'
#' @export
fsheet_timepoints_within_timeranges <-function(timepoints, timeranges){
  timepoints <- timepoints %>%
    select(person_id,
           measurement_datetime,
           symbol,
           value_as_character,
           value_as_number,
           value_as_logical
    )

  timeranges <- timeranges %>%
    select(person_id,
           #measurement_datetime,
           #symbol,
           start_datetime,
           end_datetime)

  full_join(timeranges, timepoints) %>%
    group_by(person_id) %>%
    arrange(measurement_datetime) %>%
    fill(#timepoints
      # value_as_character,
      # value_as_number,
      # value_as_logical,

      # timeranges
      start_datetime,
      end_datetime) %>%
    filter(measurement_datetime >= start_datetime &
             measurement_datetime <= end_datetime)
}

#' Label timepoints before timepoint
#'
#' @export
fsheet_label_timepoints_before_timepoint <- function(timepoints,
                                               before_timepoints){
  timepoints <- timepoints %>%
    select(person_id,
           measurement_datetime,
           symbol,
           value_as_character,
           value_as_number,
           value_as_logical
    )

  before_timepoints <- before_timepoints %>%
    select(person_id,
           datetime)

  left_join(timepoints, before_timepoints) %>%
    group_by(person_id) %>%
    arrange(measurement_datetime) %>%
    mutate(before_timepoint = case_when(
      is.na(datetime) ~ TRUE,
      !is.na(datetime) ~ measurement_datetime < datetime
    ))
}
