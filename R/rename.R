#' Rename to tidy column names
#'
#' Rename columns from the default names in the standard data format from
#' Clinical Informatics into standardised, shorter names (e.g. all lowercase,
#' underscore-separated, with `_datetime` suffix etc).
#'
#' The default renaming map can be viewed using `default_rename()`.
#'
#' If variations from this format occur, custom renaming can be performed
#' using the `names` argument.
#'
#' @param x A data frame of raw adm data
#' @param names A vector of new_name = old_name pairs
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
#' @seealso [unrename] to reverse this process
#' @name rename
#' @examples
#' # The raw columns names are messy: long names, mixed capitalisation, etc!
#' fsheet_imported_example <-
#'   read_tidybrookes_csv(
#'     file = tidybrookes_example("fsheet.csv"),
#'     col_types = "fsheet"
#'   )
#' colnames(fsheet_imported_example)
#'
#' # Default renaming can be viewed with
#' default_rename("fsheet")
#'
#' colnames(fsheet_rename(fsheet_imported_example))
#'
#' # A warning is issued if columns are missing
#' fsheet_imported_example %>%
#'   select(-MEASURE_TIME) %>%
#'   fsheet_rename
NULL

#' Rename from tidy to raw column names
#'
#' The standard data format from Clinical Informatics is handled by default. If
#' variations from this format occur, custom renaming can be performed
#' using the `names` argument
#'
#' @param x A data frame of raw adm data with tidy names
#' @param names A vector of new_name = old_name pairs
#' @return The supplied data frame, with column names in tidy-column-name format
#' @author R.J.B. Goudie
#' @seealso [rename] to reverse this process
#' @name unrename
#' @examples
#' fsheet_imported_example <-
#'   read_tidybrookes_csv(
#'     file = tidybrookes_example("fsheet.csv"),
#'     col_types = "fsheet"
#'   )
#' colnames(fsheet_imported_example)
#' colnames(fsheet_rename(fsheet_imported_example))
#' colnames(fsheet_unrename(fsheet_rename(fsheet_imported_example)))
NULL

#' @export
#' @rdname rename
default_rename <- function(x){
  if (x == "tests"){
    c(person_id = "STUDY_SUBJECT_DIGEST",
      name = "TestName",
      value = "ResultValue",
      ordered_datetime = "ORDERED_DATETIME",
      collected_datetime = "COLLECTED_DATETIME",
      received_datetime = "RECEIVED_DATETIME",
      result_datetime = "ResultDate",
      ordering_department = "ORDERING_DEPARTMENT_NAME",
      range_low = "ReferenceLow",
      range_high = "ReferenceHigh",
      unit = "ResultUnit",
      method = "Method",
      group = "TestGroupName",
      order_id = "OrderProcId")
  } else if (x == "fsheet"){
    c(person_id = "STUDY_SUBJECT_DIGEST",
      name = "disp_name",
      value = "measured_value",
      comment = "meas_comment",
      measurement_datetime = "MEASURE_TIME",
      data_id = "fsd_id",
      measurement_id = "flo-meas_id",
      line_id = "line",
      template = "template",
      form = "form")
  } else if (x == "adt"){
    c(person_id = "STUDY_SUBJECT_DIGEST",
      visit_id = "PAT_ENC_CSN",
      event_type = "EVENT_TYPE",
      start_datetime = "IN_DTTM",
      end_datetime = "OUT_DTTM",
      discharge_datetime = "HOSP_DISCH_TIME",
      department  = "ADT_DEPARTMENT_NAME",
      room = "ROOM_NAME",
      bed = "BED_LABEL",
      service_area = "ADT_SERV_AREA_NAME",
      service_name = "HOSP_SERV_NAME",
      event_type_c = "EVENT_TYPE_C")
  } else if (x == "adm"){
    c(person_id = "STUDY_SUBJECT_DIGEST",
      visit_id = "PAT_ENC_CSN",
      visit_start_datetime = "IN_DTTM",
      visit_end_datetime = "HOSP_DISCH_TIME",
      gender = "GENDER_DESC",
      ethnicity = "ETHNIC_GROUP_GROUPED",
      death_date = "DATE_OF_DEATH",
      age_at_visit_start = "AGE_AT_ADM",
      adm_service = "ADM_SERVICE",
      ward = "ADT_DEPARTMENT_NAME",
      discharge_destination = "DISCH_DEST",
      discharged_deceased = "DISCH_DECEASED",
      readmitted_within_30_days = "READMIT_WITHIN_30")
  } else if (x == "demogs"){
    c(person_id = "STUDY_SUBJECT_DIGEST",
      gender = "GENDER_DESC")
  } else if (x == "diagnosis_pl"){
    c(person_id = "STUDY_SUBJECT_DIGEST",
      icd10_list = "ICD10_LIST",
      description = "DX_DESCRIPTION",
      description_displayed = "DX_DESC_DISPLAYED",
      status = "DIAGNOSIS_STATUS",
      entered_datetime = "DIAGNOSIS_ENTERED_DATE",
      diagnosis_datetime = "DIAGNOSIS_DATE",
      resolved_datetime = "RESOLVED_DATE",
      icd10_1 = "ICD10_1",
      icd10_2 = "ICD10_2",
      icd10_3 = "ICD10_3",
      icd10_4 = "ICD10_4",
      snomed = "SNOMED_CONCEPTS",
      comment = "PROBLEM_CMT")
  } else if (x == "fsheet_io"){
    c(person_id = "STUDY_SUBJECT_DIGEST",
      name = "FLO_MEAS_NAME",
      description = "disp_name",
      value = "measured_value",
      comment = "meas_comment",
      measurement_datetime = "MEASURE_TIME",
      data_id = "fsd_id",
      measurement_id = "flo-meas_id",
      line_id = "line",
      template = "template",
      form = "form")
  } else if (x == "med_admin"){
    c(person_id = "STUDY_SUBJECT_DIGEST",
      administered_datetime = "TimeAdministered",
      name = "DrugName",
      dose = "DoseAsLabelled",
      rate = "InfusionRate",
      dose_unit = "DoseUnitAbbreviated",
      route = "RouteOfMedicationAbbreviated",
      department = "DepartmentName",
      visit_id = "MAR_ENC_CSN",
      action = "MARAction",
      mpp_code = "AMPP_VMPP_CODE",
      mpp_description = "AMPP_VMPP_DESC",
      info_concentration = "CALC_DOSE_INFO",
      info_rate = "RATE_CALC_INFO",
      info_conversion = "CONCENTRATION")
  } else if (x == "med_hist"){
    c(person_id = "STUDY_SUBJECT_DIGEST",
      icd10_list = "CURRENT_ICD10_LIST",
      description = "DX_NAME",
      entered_datetime = "CONTACT_DATE",
      medical_history_datetime_freetext = "MEDICAL_HX_DATE",
      visit_id = "HX_LNK_ENC_CSN",
      comment = "COMMENTS",
      comment_annotation = "MED_HX_ANNOTATION")
  } else if (x == "med_prescr"){
    c(person_id = "STUDY_SUBJECT_DIGEST",
      name = "DrugName",
      start_datetime = "StartDate",
      end_datetime = "EndDate",
      dose = "Dose",
      unit = "DoseUnit",
      strength = "Strength",
      dose_frequency = "DoseFrequency",
      route = "RouteOfMedication",
      in_or_out_patient = "InOrOutPatient",
      form = "FormOfMedication",
      thera_class = "THERA_CLASS",
      pharm_class = "PHARM_CLASS",
      pharm_subclass = "PHARM_SUBCLASS",
      visit_id = "PAT_ENC_CSN_ID",
      order_status_category = "OrderStatusCat",
      order_status = "ORDERSTATUS",
      provider_type = "ProviderType",
      order_class = "Order_Class",
      status_discontinuous = "DISCONTINUOUS_STATUS",
      mpp_code = "AMPP_VMPP_CODE",
      mpp_description = "AMPP_VMPP_DESC")
  } else if (x == "radiology"){
    c(person_id = "STUDY_SUBJECT_DIGEST",
      name = "Proc_Name",
      procedure_datetime = "Proc_Date",
      narrative = "Proc_Narrative",
      impression = "Proc_Impression",
      addenda = "Proc_Addenda",
      assessment = "Proc_Assessment",
      code = "Proc_Code")
  } else if (x == "procedures"){
    c(person_id = "STUDY_SUBJECT_DIGEST",
      procedure_datetime = "PROCEDUREDATE",
      description = "PROCEDUREDESC",
      opcs = "OPCSCODE")
  }
}


#' @rdname rename
#' @export
adm_rename <- function(x,
                       names = default_rename("adm")){
  relocate_ignoring_missing(x, names)
}

#' @rdname unrename
#' @export
adm_unrename <- function(x,
                         names =
                           flip_names_and_values(default_rename("adm"))){
  relocate_ignoring_missing(x, names)
}

#' @export
#' @rdname rename
adt_rename <- function(x,
                       names = default_rename("adt")){
  relocate_ignoring_missing(x, names)
}

#' @rdname unrename
#' @export
adt_unrename <- function(x,
                         names =
                           flip_names_and_values(default_rename("adt"))){
  relocate_ignoring_missing(x, names)
}

#' @export
#' @rdname rename
demogs_rename <- function(x,
                          names = default_rename("demogs")){
  relocate_ignoring_missing(x, names)
}

#' @export
#' @rdname unrename
demogs_unrename <- function(x,
                            names =
                              flip_names_and_values(default_rename("demogs"))){
  relocate_ignoring_missing(x, names)
}

#' @export
#' @rdname rename
diagnosis_pl_rename <- function(x,
                                names = default_rename("diagnosis_pl")){
  x %>%
    relocate_ignoring_missing(names) %>%
    mutate(source = "problem_list", .after = resolved_datetime)
}

#' @export
#' @rdname unrename
diagnosis_pl_unrename <-
  function(x,
           names = flip_names_and_values(default_rename("diagnosis_pl"))){
  relocate_ignoring_missing(x, names)
}

#' @export
#' @rdname rename
fsheet_io_rename <- function(x,
                             names = default_rename("fsheet_io")){
  relocate_ignoring_missing(x, names)
}

#' @export
#' @rdname unrename
fsheet_io_unrename <-
  function(x,
           names = flip_names_and_values(default_rename("fsheet_io"))){
  relocate_ignoring_missing(x, names)
}

#' @export
#' @rdname rename
fsheet_rename <- function(x,
                          names = default_rename("fsheet")){
  relocate_ignoring_missing(x, names)
}

#' @export
#' @rdname unrename
fsheet_unrename <-
  function(x,
           names = flip_names_and_values(default_rename("fsheet"))){
  relocate_ignoring_missing(x, names)
}

#' @export
#' @rdname rename
med_admin_rename <- function(x,
                             names = default_rename("med_admin")){
  relocate_ignoring_missing(x, names)
}

#' @export
#' @rdname unrename
med_admin_unrename <-
  function(x,
           names = flip_names_and_values(default_rename("med_admin"))){
  relocate_ignoring_missing(x, names)
}

#' @export
#' @rdname rename
med_hist_rename <- function(x,
                            names = default_rename("med_hist")){
  x %>%
    relocate_ignoring_missing(names) %>%
    mutate(source = "past_medical_history", .after = entered_datetime)
}

#' @export
#' @rdname rename
med_prescr_rename <- function(x,
                              names = default_rename("med_prescr")){
  relocate_ignoring_missing(x, names)
}

#' @export
#' @rdname unrename
med_prescr_unrename <-
  function(x,
           names = flip_names_and_values(default_rename("med_prescr"))){
  relocate_ignoring_missing(x, names)
}

#' @export
#' @rdname rename
radiology_rename <- function(x,
                             names = default_rename("radiology")){
  relocate_ignoring_missing(x, names)
}

#' @export
#' @rdname unrename
radiology_unrename <-
  function(x,
           names = flip_names_and_values(default_rename("radiology"))){
  relocate_ignoring_missing(x, names)
}

#' @export
#' @rdname rename
procedures_rename <- function(x,
                             names = default_rename("procedures")){
  relocate_ignoring_missing(x, names)
}

#' @export
#' @rdname unrename
procedures_unrename <- function(x,
                               names =
                                 c(STUDY_SUBJECT_DIGEST = "person_id",
                                   PROCEDUREDATE = "procedure_datetime",
                                   PROCEDUREDESC = "description",
                                   OPCSCODE = "opcs")){
  relocate_ignoring_missing(x, names)
}

#' @export
#' @rdname rename
tests_rename <- function(x,
                         names = default_rename("tests")){
  relocate_ignoring_missing(x, names)
}

#' @export
#' @rdname unrename
tests_unrename <- function(x,
                           names =
                             c(STUDY_SUBJECT_DIGEST = "person_id",
                               TestName = "name",
                               ResultValue = "value",
                               ORDERED_DATETIME = "ordered_datetime",
                               COLLECTED_DATETIME = "collected_datetime",
                               RECEIVED_DATETIME = "received_datetime",
                               ResultDate = "result_datetime",
                               ORDERING_DEPARTMENT_NAME = "ordering_department",
                               ReferenceLow = "range_low",
                               ReferenceHigh = "range_high",
                               ResultUnit = "unit",
                               Method = "method",
                               TestGroupName = "group",
                               OrderProcId = "order_id")){
  relocate_ignoring_missing(x, names)
}
