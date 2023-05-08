# infusion or bolus
# for infusion - name, concentration, [dose], mg/hr, start time, end time
# for bolus - name, concentration, dose, mg


#' Tidy raw med_admin colnames
#'
#' The standard data format from Clinical Informatics is handled by default. If
#' variations from this format occur, custom renaming can be performed
#' using the `names` argument
#'
#' @param x A data frame of raw med_admin data
#' @param names A vector of new_name = old_name pairs
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
med_admin_rename <- function(x,
                             names = default_rename("med_admin")){
  relocate_ignoring_missing(x, names)
}

#' Untidy raw med_admin colnames
#'
#' The standard data format from Clinical Informatics is handled by default. If
#' variations from this format occur, custom renaming can be performed
#' using the `names` argument
#'
#' @param x A data frame of raw med_admin data with tidy names
#' @param names A vector of new_name = old_name pairs
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
med_admin_unrename <- function(x,
                               names =
                                 c(STUDY_SUBJECT_DIGEST = "person_id",
                                   TimeAdministered = "administered_datetime",
                                   DrugName = "name",
                                   DoseAsLabelled = "dose",
                                   InfusionRate = "rate",
                                   DoseUnitAbbreviated = "unit",
                                   RouteOfMedicationAbbreviated = "route",
                                   DepartmentName = "department",
                                   MAR_ENC_CSN = "visit_id",
                                   MARAction = "action")){
  relocate_ignoring_missing(x, names)
}


#' Add a new definition of a test item
#'
#' @param med_admin_def A list of existing test definitions to add this to
#' @param symbol A short name for the flowsheet item. This should be a
#'   character vector that is suitable to use as an R object name
#' @param title The full name for the test item, suitable for using in
#'   figure titles etc
#' @param names The names of the Medications (`DrugName` in raw format)
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
med_admin_add <- function(med_admin_def,
                          note,
                          symbol,
                          title,
                          names = NA,
                          search_pattern,
                          search_exclude = NA,
                          silently_exclude_na_routes = FALSE,
                          route_class = "intravenous",
                          route_include = default_route_include(route_class),
                          route_exclude = default_route_exclude(route_class),
                          action_class = c("bolus", "infusion", "pca"),
                          action_include = default_action_include(action_class),
                          action_exclude = default_action_exclude(action_class),
                          concentration_fn = case_when(TRUE ~ NA_real_),
                          dose_range_discard_above_fn = case_when(TRUE ~ Inf),
                          dose_range_mainly_below_fn = NA,
                          expect_after = TRUE){
  expect_after <- enquo(expect_after)
  concentration_fn <- enquo(concentration_fn)
  dose_range_discard_above_fn <- enquo(dose_range_discard_above_fn)
  dose_range_mainly_below_fn <- enquo(dose_range_mainly_below_fn)

  new <- list(symbol = symbol,
              title = title,
              names = names,
              search_pattern = search_pattern,
              search_exclude = search_exclude,
              silently_exclude_na_routes = silently_exclude_na_routes,
              route_include = route_include,
              route_exclude = route_exclude,
              action_include = action_include,
              action_exclude = action_exclude,
              concentration_fn = concentration_fn,
              dose_range_discard_above_fn = dose_range_discard_above_fn,
              dose_range_mainly_below_fn = dose_range_mainly_below_fn,
              expect_after = expect_after)
  med_admin_def <- append(med_admin_def, list(new))
  names(med_admin_def)[length(med_admin_def)] <- symbol
  med_admin_def
}

#' Extract med_admin data into tidy format
#'
#' @param x Medication administration data in tidy-column-name format (after
#'   applying `med_admin_rename`)
#'
#' @param med_admin_def A med_admin definition
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
med_admin_extract <- function(x,
                              med_admin_def,
                              med_admin_units,
                              errors = stop){
  if (length(med_admin_def) == 1 & "symbol" %in% names(med_admin_def)){
    med_admin_extract_single(x,
                             med_admin_def,
                             med_admin_units = med_admin_units)
  } else {
    out <- bind_rows(lapply(med_admin_def, function(y){
      inform(format_error_bullets(c(
        glue("\nExtracting {y$title} ({y$symbol})"))))
      med_admin_extract_single(x,
                               y,
                               med_admin_units = med_admin_units,
                               errors = errors)
    }))
    out %>% arrange(symbol, administered_datetime)
  }
}

med_admin_extract_single <- function(x,
                                     med_admin_def,
                                     med_admin_units = med_admin_units,
                                     errors = stop){

  # med_admin name
  ################

  # Check for possible new med_admin names
  possible_new <- med_admin_check_for_new_names(x, med_admin_def)
  if (nrow(possible_new) > 0){
    possible_new_names <- format_as_argument(possible_new$name)
    warning(format_error_bullets(c(
      i = glue("{nrow(possible_new)} possible new medication names: ",
               "{possible_new_names}"))),
      immediate. = TRUE)
  }

  # Filter to only CUH med_admin
  out <- x %>% filter(name %in% med_admin_def$names)

  # ROUTES
  ########
  possible_new_route <- med_admin_check_for_new_route(out, med_admin_def)
  if (nrow(possible_new_route) > 0){
    possible_new_routes <- format_as_argument(possible_new_route$route)
    warning(format_error_bullets(c(
      i = glue("{nrow(possible_new_route)} possible new medication routes: ",
               "{possible_new_routes}"))),
      immediate. = TRUE)
  }

  # Exclude NAs when requested
  out <- out %>%
    mutate(will_silently_exclude_na_routes =
             (is.na(route) & med_admin_def$silently_exclude_na_routes)) %>%
    filter_inform(!will_silently_exclude_na_routes,
                  since = "since route was NA")

  if (is.list(med_admin_def$route_exclude)){
    walk2(med_admin_def$route_exclude,
          names(med_admin_def$route_exclude),
          function(x, y){
            # this is not nice code
            out <<- out %>%
              filter_inform(
                !(route %in% x) |
                  (!med_admin_def$silently_exclude_na_routes & is.na(route)),
                since = glue("since route is {y}"))
          })
  } else {
    out <- out %>%
      filter_inform(route %in% med_admin_def$route_include |
                      (!med_admin_def$silently_exclude_na_routes & is.na(route)),
                    since = glue("route is {med_admin_def$route_include}"))
  }

  # ACTIONS
  #########
  possible_new_action <- med_admin_check_for_new_action(out,
                                                        med_admin_def)
  if (nrow(possible_new_action) > 0){
    possible_new_actions <- format_as_argument(possible_new_action$action)
    warning(format_error_bullets(c(
      i = glue("{nrow(possible_new_action)} possible new medication actions: ",
               "{possible_new_actions}"))),
      immediate. = TRUE)
  }

  med_admin_exclude <- map(med_admin_def$action_exclude,
                           ~ setdiff(.x, unlist(med_admin_def$action_include)))
  if (is.list(med_admin_exclude)){
    walk2(med_admin_exclude,
          names(med_admin_exclude),
          function(x, y){
            out <<- out %>%
              filter_inform(
                !(action %in% x),
                since = glue("since action is {y}"))
          })

  } else {
    out <- out %>%
      filter_inform(!(action %in% med_admin_exclude),
                    since = glue("since action is in {med_admin_exclude}"))
  }

  out <- out %>%
    mutate(
      bolus_from_action =
        case_when(action %in% med_admin_def$action_include$bolus ~ TRUE,
                  TRUE ~ FALSE),
      infusion_from_action =
        case_when(action %in% med_admin_def$action_include$infusion ~ TRUE,
                  TRUE ~ FALSE),
      pca_from_action =
        case_when(action %in% med_admin_def$action_include$pca ~ TRUE,
                  TRUE ~ FALSE),
      feed_from_action =
        case_when(action %in% med_admin_def$action_include$feed ~ TRUE,
                  TRUE ~ FALSE)
    )

  # Add symbol and title
  out <- out %>%
    mutate(symbol = med_admin_def$symbol, .after = person_id) %>%
    mutate(title = med_admin_def$title, .after = dose_unit) %>%
    relocate(name, .after = dose_unit) %>%
    mutate(value_as_logical = TRUE) %>%
    rename(dose_original = dose,
           dose_unit_original = dose_unit,
           rate_ml_per_hour_original = rate) %>%
    mutate(administered_date = as.Date(administered_datetime))

  # Remove duplicate rows
  out <- out %>%
    distinct_inform

  # Concentration
  out <- out %>%
    mutate(concentration = !!med_admin_def$concentration_fn)

  out <- out %>%
    group_by(name) %>%
    mutate(
      dose_original_as_number = suppressWarnings({
        as.numeric(dose_original)
      }),

      rate_ml_per_hour = suppressWarnings({
        as.numeric(rate_ml_per_hour_original)
      }),

      rate_mg_per_hour = rate_ml_per_hour * concentration,

      .after = dose_original) %>%
    ungroup

  # CONVERT TO CANONICAL DOSE UNIT
  ################################
  out <- out %>%
    left_join(med_admin_units, by = "dose_unit_original") %>%
    med_admin_map_units_to_canonical()

  possible_new_dose_unit <- med_admin_check_for_new_dose_unit(
    # TODO IS THIS OK TO AVOID REPORTING NA dose_unit?
    out %>%
      filter(is.na(dose_unit_original))
  )
  if (nrow(possible_new_dose_unit) > 0){
    possible_new_dose_unit <- format_as_argument(possible_new_dose_unit$dose_unit_original)
    warning(format_error_bullets(c(
      i = glue("{nrow(possible_new_dose_unit)} possible new dose_unit: ",
               "{possible_new_dose_unit}"))),
      immediate. = TRUE)
  }

  # bolus/infusion inference from dose_unit
  infusion_dose_units <- c("mg/hr", "mg/kg/hr", "ml/hr", "ml/kg/hr")

  out <- out %>%
    mutate(infusion_from_dose_unit =
             case_when(is.na(dose_unit) ~ NA,
                       dose_unit %in% infusion_dose_units ~ TRUE,
                       TRUE ~ FALSE),
           bolus_from_dose_unit = !infusion_from_dose_unit)

  # Rate
  out <- out %>%
    mutate(infusion_from_rate =
             if_else(!is.na(rate_ml_per_hour),
                     true = TRUE,
                     false = FALSE),
           bolus_from_rate = !infusion_from_rate)

  # Convert dose to rate
  ######################
  if ("info_rate" %in% colnames(out)){
    out <- out %>%
      med_admin_info_rate_Weight()
  }

  # Discard non-bolus or infusions by action
  out <- out %>%
    mutate(bolus = bolus_from_action,
           infusion = infusion_from_action,
           pca = pca_from_action,
           feed = feed_from_action)

  # # Discard too high doses
  # out <- out %>%
  #   mutate(is_too_high = dose > !!med_admin_def$dose_range_discard_above_fn) %>%
  #   filter_inform(!is_too_high,
  #                 since = glue("since above dose_range_discard_above_fn"))

  # TODO exclude rows where no dose or rate?
  # check_that_all(out,
  #                suppressWarnings({!is.na(as.numeric(dose_original_as_number))}),
  #                name = "all doses being numeric")

  # Check expect_after condition
  check_that_all(out, !!med_admin_def$expect_after, "expect_after")

  # Return result
  inform(format_error_bullets(c(i = glue("{nrow(out)} rows extracted"))))
  out %>%
    select(-will_silently_exclude_na_routes) %>%
    arrange(administered_datetime)
}

med_admin_check_for_new_names <- function(x,
                                          med_admin_def){
  if (!all(is.na(med_admin_def$search_exclude))){
    med_admin_def$search_exclude <-
      c(med_admin_def$names, med_admin_def$search_exclude)
  } else {
    med_admin_def$search_exclude <-
      med_admin_def$names
  }
  med_admin_def$search_pattern <- paste0(med_admin_def$search_pattern, collapse = "|")
  x %>%
    filter(str_detect(name, regex(med_admin_def$search_pattern, ignore_case = TRUE)) &
             (!name %in% c(med_admin_def$names, med_admin_def$search_exclude))) %>%
    count(name)
}

med_admin_check_for_new_route <- function(x,
                                          med_admin_def){
  handled_routes <- c(unlist(med_admin_def$route_exclude),
                      unlist(med_admin_def$route_include))
  x %>%
    filter(!route %in% handled_routes & !is.na(route)) %>%
    count(route)
}

med_admin_check_for_new_dose_unit <- function(x,
                                              med_admin_def){
  x %>%
    filter((is.na(dose_unit_is_checked) | !dose_unit_is_checked) &
             !is.na(dose_unit_original)) %>%
    count(dose_unit_original)
}


med_admin_check_for_new_action <- function(x,
                                           med_admin_def){
  x %>%
    filter(!action %in%
             c(unlist(med_admin_def$action_exclude),
               unlist(med_admin_def$action_include))) %>%
    count(action)
}

#' Generate a stub definition of medication administration
#'
#' Returns the code needed to define a med_admin_def object.
#'
#' Typically, you may wish to run with `search` set to the drug name, and
#' then rerun once you have filtered the list of medications down. This
#' ensures that the `route` and `action` listed only includes those found
#' in the data for your medications of interest.
#'
#' @param x Medication data in tidy-column-name format (after applying
#'   `med_admin_rename`)
#' @param symbol A short name for the medication administration item. This
#'   should be a character vector that is suitable to use as an R object name
#' @param title The full name for the medication administration item, suitable
#'   for using in figure titles etc
#' @param search A charater vector of potential synonymns for this
#'   test, in case new names/variants of the same medication added in future.
#'   If `NULL`, then `names` is used to define a specific list of medication
#'   names to include.
#' @param names The names of the Medications (`DrugName` in raw format). Only
#'   used if `search = NULL`.
med_admin_stub_def <- function(x,
                               symbol,
                               title,
                               search = NULL,
                               names = NULL,
                               route_class = NULL,
                               action_class = NULL){
  if (!is.null(search)){
    possible_includes <- filter_med_admin(x, search)
  } else {
    possible_includes <- x %>%
      filter(name %in% names)
  }

  route_include <- default_route_include(route_class)
  route_exclude <- default_route_exclude(route_class)
  action_include <- default_action_include(action_class)
  action_exclude <- default_action_exclude(action_class)

  possible_includes_pre_route_action_excludes <- possible_includes
  walk2(route_exclude,
        names(route_exclude),
        function(x, y){
          # this is not nice code
          possible_includes <<- possible_includes %>%
            filter_inform(
              !(route %in% x),
              since = glue("since route is {y}"))
        })

  action_exclude <- map(action_exclude,
                        ~ setdiff(.x, unlist(action_include)))
  walk2(action_exclude,
        names(action_exclude),
        function(x, y){
          # this is not nice code
          possible_includes <<- possible_includes %>%
            filter_inform(
              !(action %in% x),
              since = glue("since action is {y}"))
        })

  possible_includes_names <- possible_includes %>%
    count(name) %>%
    arrange(name) %>%
    pull(name)

  possible_includes_pre_route_action_excludes_names <-
    possible_includes_pre_route_action_excludes %>%
    count(name, sort = TRUE) %>%
    pull(name)

  possible_excludes_names <-
    setdiff(possible_includes_pre_route_action_excludes_names,
            possible_includes_names)

  possible_includes_routes <- possible_includes %>%
    count(route, sort = TRUE) %>%
    pull(route)

  possible_exclude_na_routes <- ""
  if (any(is.na(possible_includes_routes))){
    possible_exclude_na_routes <- "silently_exclude_na_routes = FALSE,\n  "
    possible_includes_routes <-
      possible_includes_routes[!is.na(possible_includes_routes)]
  }

  possible <- format_as_argument(possible_includes_names)
  excludes <- format_as_argument(possible_excludes_names)
  search <- format_as_argument(search)
  possible_includes_routes <- format_as_argument(possible_includes_routes)

  glue("med_admin_defs <- med_admin_defs %>%
  med_admin_add(
    symbol = \"{symbol}\",
    title = \"{title}\",
    names = {possible},
    search_pattern = {search},
    search_exclude = {excludes},
    {possible_exclude_na_routes}route_class = {format_as_argument(route_class)},
    action_class = {format_as_argument(action_class)},
    concentration_fn =
       case_when(TRUE ~ NA_real_))")
}

med_admin_map_units_to_canonical <- function(x){
  x %>%
    mutate(mass_multiplier =
             case_when(
               dose_unit_original_mass == "g" ~ 1/1000,
               dose_unit_original_mass == "mg" ~ 1, # Canonical unit
               dose_unit_original_mass == "mcg" ~ 1000,
               dose_unit_original_mass == "ng" ~ 1000 * 1000,
               is.na(dose_unit_original_mass) ~ NA_real_# in mg
             ),
           volume_multiplier =
             case_when(
               dose_unit_original_volume == "l" ~ 1/1000,
               dose_unit_original_volume == "ml" ~ 1 # Canonical unit
             ),
           weight_multiplier =
             case_when(
               dose_unit_original_weight == "kg" ~ 1, # Canonical unit
               #dose_unit_original_weight == "g" ~ 1000,
               is.na(dose_unit_original_weight) ~ NA_real_
             ),
           time_multiplier =
             case_when(
               dose_unit_original_time == "day" ~ 1/24,
               dose_unit_original_time == "hr" ~ 1, # Canonical unit
               dose_unit_original_time == "min" ~ 60,

               is.na(dose_unit_original_time) ~ NA_real_
             )) %>%
    mutate(
      dose =
        case_when(
          dose_unit_original_type == "mass" ~
            (dose_original_as_number / mass_multiplier),
          dose_unit_original_type == "mass/time" ~
            (dose_original_as_number / mass_multiplier) *
            time_multiplier,
          dose_unit_original_type == "mass/weight/time" ~
            (dose_original_as_number / mass_multiplier) *
            weight_multiplier *
            time_multiplier,

          dose_unit_original_type == "volume" ~
            (dose_original_as_number / volume_multiplier),
          dose_unit_original_type == "volume/time" ~
            (dose_original_as_number / volume_multiplier) *
            time_multiplier,
          dose_unit_original_type == "volume/weight/time" ~
            (dose_original_as_number / volume_multiplier) *
            weight_multiplier *
            time_multiplier
        ),

      dose_unit =
        case_when(
          dose_unit_original_type == "mass" ~ "mg",
          dose_unit_original_type == "mass/time" ~ "mg/hr",
          dose_unit_original_type == "mass/weight/time" ~ "mg/kg/hr",

          dose_unit_original_type == "volume" ~ "ml",
          dose_unit_original_type == "volume/time" ~ "ml/hr",
          dose_unit_original_type == "volume/weight/time" ~ "ml/kg/hr"
        ))
}

med_admin_info_rate_Weight <- function(x){
  x %>%
    mutate(
      weight_info_rate = as.numeric(str_match(info_rate, "([0-9.]+) kg")[,2])
    )
}

med_admin_unweight_adjust <- function(x){
  x %>%
    mutate(rate_mg_per_hour_from_dose =
             case_when(dose_unit == "mg/kg/hr" ~ dose * weight_info_rate,
                       dose_unit == "mg/hr" ~ dose,
                       dose_unit == "ml/hr" ~ dose * concentration,
                       TRUE ~ NA_real_),
           rate_ml_per_hour_from_dose =
             case_when(dose_unit == "ml/kg/hr" ~ dose * weight_info_rate,
                       dose_unit == "ml/hr" ~ dose,
                       dose_unit == "mg/hr" ~ dose / concentration,
                       TRUE ~ NA_real_))
}


default_route_include <- function(route_class){
  med_admin_routes  %>%
    tidyr:::pivot_longer(-c(route, comment, classified),
                         names_to = "type",
                         values_to = "include") %>%
    filter(include) %>%
    filter(type %in% route_class)  %>%
    unstack(route ~ type)
}

default_route_exclude <- function(route_class){
  med_admin_routes %>%
    tidyr:::pivot_longer(-c(route, comment, classified),
                         names_to = "type",
                         values_to = "include") %>%
    filter(include) %>%
    filter(!(type %in% route_class))  %>%
    unstack(route ~ type)
}

default_action_include <- function(action_class){
  med_admin_action %>%
    tidyr:::pivot_longer(-c(action),
                         names_to = "type",
                         values_to = "include") %>%
    filter(include) %>%
    filter(type %in% action_class)  %>%
    unstack(action ~ type)
}

default_action_exclude <- function(action_class){
  med_admin_action %>%
    tidyr:::pivot_longer(-c(action),
                         names_to = "type",
                         values_to = "include") %>%
    filter(include) %>%
    filter(!(type %in% action_class))  %>%
    unstack(action ~ type)
}
