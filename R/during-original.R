v1_all_during <- function(x,
                       y,
                       datetime,
                       during,
                       names_from = "symbol",
                       join = "left"){
  y <- y %>%
    rename(datetime = {{datetime}}) %>%
    arrange(datetime)

  inform_if_all_times_are_midnight(pull(y, datetime))

  common_cols <- intersect(colnames(x), colnames(y))
  if (!"person_id" %in% common_cols){
    stop("`person_id` not in both data frames")
  } else if (length(common_cols) > 1) {
    stop(paste0("More common columns than just `person_id` in data frames: ",
                format_as_argument(setdiff(common_cols, "person_id"))))
  }

  if (join == "left"){
    join_fn <- v1_left_join_filter
  } else if (join == "inner"){
    join_fn <- v1_inner_join_filter
  }
  if (during == "anytime"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          TRUE)
  } else if (during == "during_visit"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime &
                      datetime <= visit_end_datetime,
                    is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime))

    if (inherits(out, "tbl_dbi")){

    } else {
      out <- out %>%
        mutate(days_since_visit_start = interval(visit_start_datetime, datetime)/days(1))
    }
  } else if (during == "during_visit_initial_24h"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime &
                      datetime <= visit_start_datetime + dhours(24) &
                      datetime <= visit_end_datetime,
                    is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime &
                      datetime <= visit_start_datetime + dhours(24)))
  } else if (during == "during_visit_initial_72h"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime &
                      datetime <= visit_start_datetime + dhours(72) &
                      datetime <= visit_end_datetime,
                    is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime &
                      datetime <= visit_start_datetime + dhours(72)))
  } else if (during == "during_department"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", "department_visit_index", names_from),
        filter_condition =
          case_when(!is.na(department_end_datetime) ~
                      datetime >= department_start_datetime &
                      datetime <= department_end_datetime,
                    is.na(department_end_datetime) ~
                      datetime >= department_start_datetime))
  } else if (during == "during_department_after_48h"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", "department_visit_index", names_from),
        filter_condition =
          case_when(!is.na(department_end_datetime) ~
                      datetime >= department_start_datetime + dhours(48) &
                      datetime <= department_end_datetime,
                    is.na(department_end_datetime) ~
                      datetime >= department_start_datetime + dhours(48)))
  } else if (during == "during_icu_visit"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", "icu_visit_id", names_from),
        filter_condition =
          case_when(!is.na(icu_visit_end_datetime) ~
                      datetime >= icu_visit_start_datetime &
                      datetime <= icu_visit_end_datetime,
                    is.na(icu_visit_start_datetime) ~
                      datetime >= icu_visit_start_datetime))
  } else if (during == "during_icu_visit_initial_24h"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", "icu_visit_id", names_from),
        filter_condition =
          case_when(!is.na(icu_visit_end_datetime) ~
                      datetime >= icu_visit_start_datetime &
                      datetime <= icu_visit_start_datetime + dhours(24) &
                      datetime <= icu_visit_end_datetime,
                    is.na(icu_visit_start_datetime) ~
                      datetime >= icu_visit_start_datetime &
                      datetime <= icu_visit_start_datetime + dhours(24)))
  } else if (during == "during_icu_visit_after_24h"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", "icu_visit_id", names_from),
        filter_condition =
          case_when(!is.na(icu_visit_end_datetime) ~
                      datetime >= icu_visit_start_datetime + dhours(24) &
                      datetime <= icu_visit_end_datetime,
                    is.na(icu_visit_start_datetime) ~
                      datetime >= icu_visit_start_datetime + dhours(24)))
  } else if (during == "before_visit_initial_24h"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(visit_end_datetime) ~
                      datetime <= visit_start_datetime + dhours(24) &
                      datetime <= visit_end_datetime,
                    is.na(visit_end_datetime) ~
                      datetime <= visit_start_datetime + dhours(24)))
  } else if (during == "14_days_before_visit_until_visit_end"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime - ddays(14) &
                      datetime <= visit_end_datetime,
                    is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime - ddays(14)))
  } else if (during == "year_before_visit_until_visit_end"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime - dyears(1) &
                      datetime <= visit_end_datetime,
                    is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime - dyears(1)))
  } else if (during == "before_visit_end"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(visit_end_datetime) ~
                      datetime <= visit_end_datetime,
                    is.na(visit_end_datetime) ~
                      TRUE))
  } else if (during == "30_days_before_visit_start"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime - ddays(30) &
                      datetime <= visit_start_datetime,
                    is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime - ddays(30) &
                      datetime <= visit_start_datetime))
  } else if (during == "year_before_initial_24h"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime - dyears(1) &
                      datetime <= visit_start_datetime + dhours(24) &
                      datetime <= visit_end_datetime,
                    is.na(visit_end_datetime) ~
                      datetime >= visit_start_datetime - dyears(1) &
                      datetime <= visit_start_datetime + dhours(24)))
  } else if (during == "during_value"){
    out <- x %>%
      join_fn(
        y,
        join_by = "person_id",
        filter_by = c("person_id", "visit_id", names_from),
        filter_condition =
          case_when(!is.na(value_end_datetime) ~
                      datetime >= value_start_datetime &
                      datetime <= value_end_datetime,
                    is.na(value_end_datetime) ~
                      datetime >= value_start_datetime))
  } else {
    stop("all_during does not know how to hangle this `during` value:",
         during)
  }
  out
}

v1_left_join_filter <- function(x, y, ...){
  out <- inner_join_filter(x = x,
                           y = y,
                           ...)

  # join again to x on common columns, so that anyone WITHOUT a y is included
  by <- intersect(colnames(x), colnames(out))
  left_join(x, out, by = by)
}

v1_inner_join_filter <- function(x,
                              y,
                              join_by,
                              filter_by,
                              filter_condition){
  filter_condition <- enquo(filter_condition)
  formula <- enquo(formula)

  filter_by <- as.list(filter_by)
  filter_by_symbol <- rlang::syms(filter_by)

  out <- x %>%
    left_join(y, by = join_by, na_matches = "never") %>%
    group_by(!!! filter_by_symbol) %>%
    mutate(satisfies_filter_condition = !! filter_condition) %>%
    filter(satisfies_filter_condition) %>%
    group_by(!!! filter_by_symbol) %>%
    select(-satisfies_filter_condition)
}

v1_summarise_during <- function(x,
                             y,
                             datetime,
                             during,
                             type = "none",
                             formula,
                             names_from = "symbol",
                             group_by = c(c("person_id", "visit_id"),
                                          names_from),
                             values_from = c("value_as_number",
                                             "value_as_character",
                                             "value_as_logical",
                                             "datetime"),
                             names_suffix){
  formula <- enquo(formula)
  datetime <- enquo(datetime)

  cli::cli_alert_info("Extracting all during")
  out <- x %>%
    v1_all_during(y,
               datetime = !! datetime,
               during = during,
               names_from = names_from,
               join = "inner")

  cli::cli_alert_info("Summarising/slicing")
  out <- out %>%
    v1_grouped_summarise_or_slice(
      type = type,
      formula = !! formula,
      group_by = group_by)

  cli::cli_alert_info("Pivoting wider")
  out <- out %>%
    v1_pivot_value_wider(id_cols = setdiff(group_by, names_from),
                      names_from = names_from,
                      values_from = values_from,
                      names_suffix = glue("{names_suffix}_{during}"))

  # join again to x on common columns, so that anyone WITHOUT a y is included
  by <- intersect(colnames(x), colnames(out))
  left_join(x, out, by = by)
}

v1_pivot_value_wider <- function(x,
                              id_cols = c("person_id", "visit_id"),
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
  # this handles cases like "ever_during" where no datetime is available
  values_from <- if ("datetime" %in% colnames(x)){
    values_from
  } else {
    setdiff(values_from, "datetime")
  }

  names_glue <- paste0(magic_prefix,  "{",
                       names_from, "}",
                       names_suffix,
                       "_{.value}")

  x_split <- v1_split_by_type(x)
  anything <-
    x_split$any_numeric ||
    x_split$any_character ||
    x_split$any_logical

  x_numeric <- NULL
  x_character <- NULL
  x_logical <- NULL

  if (x_split$any_numeric){
    values_from_numeric <- setdiff(values_from,
                                   c("value_as_character", "value_as_logical"))
    x_numeric <- x_split$numeric %>%
      pivot_wider(
        id_cols = all_of(id_cols),
        names_from = all_of(names_from),
        values_from = all_of(values_from_numeric),
        names_glue = names_glue)
  }

  if (x_split$any_character){
    values_from_character <- setdiff(values_from,
                                     c("value_as_number", "value_as_logical"))
    x_character <- x_split$character %>%
      pivot_wider(
        id_cols = all_of(id_cols),
        names_from = all_of(names_from),
        values_from = all_of(values_from_character),
        names_glue = names_glue)
  }

  if (x_split$any_logical){
    values_from_logical <- setdiff(values_from,
                                   c("value_as_number", "value_as_character"))
    x_logical <- x_split$logical %>%
      pivot_wider(
        id_cols = all_of(id_cols),
        names_from = all_of(names_from),
        values_from = all_of(values_from_logical),
        names_glue = names_glue)
  }

  if (anything){
    x <- list(x_numeric, x_character, x_logical) %>%
      compact %>%
      reduce(full_join)
  } else {
    stop("Neither numeric or character output from slice/summary found")
  }

  v1_relocate_and_clean_new_cols(x, magic_prefix = magic_prefix)
}

v1_grouped_summarise_or_slice <- function(x,
                                       type = "summarise",
                                       formula,
                                       group_by){
  formula <- enquo(formula)

  group_by <- as.list(group_by)
  group_by_symbol <- rlang::syms(group_by)

  x <- x %>%
    group_by(!!! group_by_symbol)

  if (type == "slice"){
    out <- x %>%
      slice(!! formula)
  } else if (type == "slice_min"){
    out <- x %>%
      slice_min(!! formula)
  } else if (type == "summarise"){
    out <- x %>%
      summarise(!! formula)
  } else if (type == "mutate"){
    # for debugging
    out <- x %>%
      mutate(output = !! formula)
  }
}

v1_split_by_type <- function(x){
  has_value_as_number_col <- "value_as_number" %in% colnames(x)
  has_value_as_character_col <- "value_as_character" %in% colnames(x)
  has_value_as_logical_col <- "value_as_logical" %in% colnames(x)
  has_type_col <- "type" %in% colnames(x)

  if (has_type_col){
    possible_types <- c("numeric", "character", "logical")
    type_col_is_possible <- all(unique(pull(x, type)) %in% possible_types)
  }

  clearly_specifies_types <-
    has_type_col && type_col_is_possible
  has_only_value_as_number_col <-
    has_value_as_number_col &
    !has_value_as_character_col &
    !has_value_as_logical_col
  has_only_value_as_character_col <-
    !has_value_as_number_col &
    has_value_as_character_col &
    !has_value_as_logical_col
  has_only_value_as_logical_col <-
    !has_value_as_number_col &
    !has_value_as_character_col &
    has_value_as_logical_col

  if (clearly_specifies_types){
    x_numeric <- x %>%
      filter(type == "numeric")
    x_character <- x %>%
      filter(type == "character")
    x_logical <- x %>%
      filter(type == "logical")
    any_numeric <- nrow(x_numeric) > 0
    any_character <- nrow(x_character) > 0
    any_logical <- nrow(x_logical) > 0
  } else if (has_only_value_as_number_col){
    any_numeric <- TRUE
    any_character <- FALSE
    any_logical <- FALSE
    x_numeric <- x
    x_character <- NULL
    x_logical <- NULL
  } else if (has_only_value_as_character_col){
    any_numeric <- FALSE
    any_character <- TRUE
    any_logical <- FALSE
    x_character <- x
    x_numeric <- NULL
    x_logical <- NULL
  } else if (has_only_value_as_logical_col){
    any_numeric <- FALSE
    any_character <- FALSE
    any_logical <- TRUE
    x_character <- NULL
    x_logical <- x
    x_numeric <- NULL
  } else {
    stop("Could not establish whether to use numeric or character values")
  }
  list(any_numeric = any_numeric,
       any_character = any_character,
       any_logical = any_logical,
       numeric = x_numeric,
       character = x_character,
       logical = x_logical)
}

#' @noRd
v1_relocate_and_clean_new_cols <- function(x, magic_prefix){
  colnames_strip_new <- function(colname){
    case_when(str_starts(colname, magic_prefix) ~
                str_remove(colname, glue("^", magic_prefix)),
              TRUE ~ colname)
  }

  # remove these default names to produce cleaner column names
  suffix_to_remove_number <- "_value_as_number"
  suffix_to_remove_character <- "_value_as_character"
  suffix_to_remove_logical <- "_value_as_logical"

  x %>%
    select(!starts_with(magic_prefix), sort(colnames(.))) %>%
    rename_with(.fn = function(colname){
      case_when(str_ends(colname, suffix_to_remove_character) ~
                  str_remove(colname, glue("{suffix_to_remove_character}$")),
                str_ends(colname, suffix_to_remove_number) ~
                  str_remove(colname, glue("{suffix_to_remove_number}$")),
                str_ends(colname, suffix_to_remove_logical) ~
                  str_remove(colname, glue("{suffix_to_remove_logical}$")),
                TRUE ~ colname)
    }) %>%
    rename_with(.fn = colnames_strip_new)
}

v1_first_during <- function(...){
  v1_summarise_during(...,
                   type = "slice_min",
                   formula = datetime,
                   names_suffix = "first")
}

v1_last_during <- function(...){
  v1_summarise_during(...,
                   type = "slice",
                   formula = which.max(datetime),
                   names_suffix = "last")
}

v1_first_during_after_event <- function(..., event_datetime, names_suffix = ""){
  event_datetime <- enquo(event_datetime)
  v1_summarise_during(
    ...,
    type = "slice",
    formula =
      interval(!! event_datetime, datetime) %>%
      int_length() %>%
      purrr:::map(~ case_when(.x < 0 ~ NA,
                              TRUE ~ .x)) %>%
      which.min(),
    names_suffix = glue("first_after_{names_suffix}"))
}

v1_last_during_before_event <- function(..., event_datetime, names_suffix = ""){
  event_datetime <- enquo(event_datetime)
  v1_summarise_during(
    ...,
    type = "slice",
    formula =
      interval(!! event_datetime, datetime) %>%
      int_length() %>%
      purrr:::map(~ case_when(.x > 0 ~ NA,
                              TRUE ~ .x)) %>%
      which.max(),
    names_suffix = glue("last_before_{names_suffix}"))
}

v1_nearest_to_event_during <- function(..., event_datetime, names_suffix = ""){
  event_datetime <- enquo(event_datetime)
  v1_summarise_during(
    ...,
    type = "slice",
    formula =
      interval(!! event_datetime, datetime) %>%
      int_length() %>%
      map(~abs(.x)) %>%
      which.min(),
    names_suffix = glue("nearest_to_{names_suffix}"))
}

v1_nearest_visit_start_during <- function(...){
  v1_summarise_during(
    ...,
    type = "slice",
    formula =
      interval(visit_start_datetime, datetime) %>%
      int_length() %>%
      abs() %>%
      which.min(),
    names_suffix = "nearest_visit_start")
}

v1_max_during <- function(...){
  v1_summarise_during(...,
                   type = "slice",
                   formula = which.max(value_as_number),
                   names_suffix = "max")
}

v1_min_during <- function(...){
  v1_summarise_during(...,
                   type = "slice",
                   formula = which.min(value_as_number),
                   names_suffix = "min")
}

v1_ever_during <- function(...){
  v1_summarise_during(...,
                   type = "summarise",
                   formula =
                     tibble(value_as_logical = any(value_as_logical),
                            type = "logical"),
                   names_suffix = "ever")
}
