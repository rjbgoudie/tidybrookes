#' Load data from Epic in CSV format
#'
#' @param file Either a path to a file, a connection, or literal data (either
#'   a single string or a raw vector), as per [readr::read_delim()]
#' @param col_types Either a string specifying the name of the table that the
#'   data comes from, for which a standard format can be used. Available
#'   value are `adm`, `adt`, `demogs`, `fsheet`, `fsheet_io`, `tests`,
#'   `med_admin`, `med_prescr`, `diagnosis_pl`, `med_hist`, `radiology`.
#'
#'   Or a [readr::cols()] format list of column names and types, which
#'   can be used where a nonstandard data format are supplied.
#' @param n_max Maximum number of lines to read.
#' @param na Character vector of strings to interpret as missing values.
#'   Set this option to `character()` to indicate no missing values.
#' @importFrom readr read_csv cols col_character col_integer col_double locale col_datetime
read_tidybrookes_csv <- function(file, col_types, n_max = Inf, na = c("", "NA"),
                                 quote = "\""){
  col_types <- extract_col_types(col_types = col_types)
  read_csv(file = file,
           col_types = col_types,
           locale = locale(tz = "Europe/London"),
           n_max = n_max,
           quote = quote,
           na = na) %>%
    as_tibble
}

#' Load data from Epic in CSV format
#'
#' @param file Either a path to a file, a connection, or literal data (either
#'   a single string or a raw vector), as per [readr::read_delim()]
#' @param col_types Either a string specifying the name of the table that the
#'   data comes from, for which a standard format can be used. Available
#'   value are `adm`, `adt`, `demogs`, `fsheet`, `fsheet_io`, `tests`,
#'   `med_admin`, `med_prescr`, `diagnosis_pl`, `med_hist`, `radiology`.
#'
#'   Or a [readr::cols()] format list of column names and types, which
#'   can be used where a nonstandard data format are supplied.
#' @param n_max Maximum number of lines to read.
#' @param na Character vector of strings to interpret as missing values.
#'   Set this option to `character()` to indicate no missing values.
#' @importFrom readr read_csv_chunked cols col_character col_integer col_double locale col_datetime
#' @importFrom DBI dbWriteTable
db_write_tidybrookes_csv <- function(file,
                                     connection,
                                     table_name,
                                     col_types,
                                     #n_max = Inf,
                                     na = c("", "NA"),
                                     quote = "\"",
                                     progress = TRUE){
  col_types <- extract_col_types(col_types = col_types)
  cat("Loading..\n")
  read_csv_chunked(
    file = file,
    callback = function(chunk, index) {
      dbWriteTable(connection,
                   chunk,
                   name = table_name,
                   append = TRUE)
    },
    col_types = col_types,
    locale = locale(tz = "Europe/London"),
    quote = quote,
    na = na,
    progress = progress)
}

#' Load data from Epic in delim format
#'
#' @param file Either a path to a file, a connection, or literal data (either
#'   a single string or a raw vector), as per [readr::read_delim()]
#' @param col_types Either a string specifying the name of the table that the
#'   data comes from, for which a standard format can be used. Available
#'   value are `adm`, `adt`, `demogs`, `fsheet`, `fsheet_io`, `tests`,
#'   `med_admin`, `med_prescr`, `diagnosis_pl`, `med_hist`, `radiology`.
#'
#'   Or a [readr::cols()] format list of column names and types, which
#'   can be used where a nonstandard data format are supplied.
#' @param n_max Maximum number of lines to read.
#' @param na Character vector of strings to interpret as missing values.
#'   Set this option to `character()` to indicate no missing values.
#' @importFrom readr read_csv_chunked cols col_character col_integer col_double locale col_datetime
#' @importFrom DBI dbWriteTable
db_write_tidybrookes_delim <- function(file,
                                       connection,
                                       table_name,
                                       col_types,
                                       #n_max = Inf,
                                       na = c("", "NA"),
                                       quote = "\"",
                                       ...){
  col_types <- extract_col_types(col_types = col_types)
  read_delim_chunked(
    file = file,
    callback = function(chunk, index) {
      dbWriteTable(connection,
                   chunk,
                   name = table_name,
                   append = TRUE)
    },
    col_types = col_types,
    locale = locale(tz = "Europe/London"),
    quote = quote,
    na = na,
    ...)
}

#' Load data from Epic in delimited format
#'
#' @param file Either a path to a file, a connection, or literal data (either
#'   a single string or a raw vector), as per [readr::read_delim()].
#' @param col_types Either a string specifying the name of the table that the
#'   data comes from, for which a standard format can be used. Available
#'   value are `adm`, `adt`, `demogs`, `fsheet`, `fsheet_io`, `tests`,
#'   `med_admin`, `med_prescr`, `diagnosis_pl`, `med_hist`, `radiology`.
#'
#'   Or a [readr::cols()] format list of column names and types, which
#'   can be used where a nonstandard data format are supplied.
#' @param delim Single character used to separate fields within a record.
#' @param n_max Maximum number of lines to read.
#' @param na Character vector of strings to interpret as missing values.
#'   Set this option to `character()` to indicate no missing values.
#' @importFrom readr read_delim cols col_character col_integer col_double locale col_datetime
read_tidybrookes_delim <- function(file,
                                   col_types,
                                   delim = "|",
                                   n_max = Inf,
                                   na = c("", "NA"),
                                   quote = "\""){
  col_types <- extract_col_types(col_types = col_types)
  read_delim(file = file,
             delim = delim,
             col_types = col_types,
             locale = locale(tz = "Europe/London"),
             n_max = n_max,
             quote = quote,
             na = na) %>%
    as_tibble
}

#' List of standard column types for Epic data
#'
#' @param col_types Either a string specifying the name of the table that the
#'   data comes from, for which a standard format can be used. Available
#'   value are `adm`, `adt`, `demogs`, `fsheet`, `fsheet_io`, `tests`,
#'   `med_admin`, `med_prescr`, `diagnosis_pl`, `med_hist`, `radiology`.
#'
#'   Or a [readr::cols()] format list of column names and types, which
#'   can be used where a nonstandard data format are supplied.
extract_col_types <- function(col_types){
  if (inherits(col_types, "col_spec")){
    col_types <- col_types
  } else if (col_types == "adm"){
    col_types <- cols(
      STUDY_SUBJECT_DIGEST = col_character(),
      PAT_ENC_CSN = col_integer(),
      IN_DTTM = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      HOSP_DISCH_TIME = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      GENDER_DESC = col_character(),
      ETHNIC_GROUP_GROUPED = col_character(),
      DATE_OF_DEATH = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      AGE_AT_ADM = col_integer(),
      ADM_SERVICE = col_character(),
      ADT_DEPARTMENT_NAME = col_character(),
      DISCH_DEST = col_character(),
      DISCH_DECEASED = col_character(),
      READMIT_WITHIN_30 = col_character())
  } else if (col_types == "adt"){
    col_types <- cols(
      STUDY_SUBJECT_DIGEST = col_character(),
      EVENT_TYPE_C = col_integer(),
      EVENT_TYPE = col_character(),
      IN_DTTM = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      HOSP_DISCH_TIME = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      ADT_DEPARTMENT_NAME = col_character(),
      ROOM_NAME = col_character(),
      BED_LABEL = col_character(),
      ADT_SERV_AREA_NAME = col_character(),
      HOSP_SERV_NAME = col_character(),
      PAT_ENC_CSN = col_integer())
  } else if (col_types == "demogs"){
    col_types <- cols(
      STUDY_SUBJECT_DIGEST = col_character(),
      GENDER_DESC = col_character())
  } else if (col_types == "fsheet"){
    col_types <- cols(
      STUDY_SUBJECT_DIGEST = col_character(),
      fsd_id = col_integer(),
      line = col_integer(),
      `flo-meas_id` = col_character(),
      MEASURE_TIME = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      disp_name = col_character(),
      measured_value = col_character(),
      meas_comment = col_character(),
      template = col_character(),
      form = col_character())
  } else if (col_types == "fsheet_io"){
    col_types <- cols(
      STUDY_SUBJECT_DIGEST = col_character(),
      fsd_id = col_integer(),
      line = col_integer(),
      `flo-meas_id` = col_character(),
      MEASURE_TIME = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      disp_name = col_character(),
      measured_value = col_character(),
      meas_comment = col_character(),
      template = col_character(),
      form = col_character(),
      FLO_MEAS_NAME = col_character())
  } else if (col_types == "tests"){
    col_types <- cols(
      STUDY_SUBJECT_DIGEST = col_character(),
      TestGroupName = col_character(),
      TestName = col_character(),
      ResultValue = col_character(),
      ReferenceLow = col_character(),
      ReferenceHigh = col_character(),
      ResultUnit = col_character(),
      ResultDate = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      Method = col_character(),
      ORDERING_DEPARTMENT_NAME = col_character(),
      COLLECTED_DATETIME = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      ORDERED_DATETIME = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      RECEIVED_DATETIME = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      OrderProcId = col_character())
  } else if (col_types == "med_admin"){
    col_types <- cols(
      STUDY_SUBJECT_DIGEST = col_character(),
      TimeAdministered = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      DrugName = col_character(),
      DoseAsLabelled = col_character(),
      InfusionRate = col_character(),
      DoseUnitAbbreviated = col_character(),
      RouteOfMedicationAbbreviated = col_character(),
      DepartmentName = col_character(),
      MAR_ENC_CSN = col_integer(),
      MARAction = col_character())
  } else if (col_types == "med_prescr"){
    col_types <- cols(
      "STUDY_SUBJECT_DIGEST" = col_character(),
      "DrugName" = col_character(),
      "StartDate" = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      "EndDate" = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      "Dose" = col_character(),
      "Strength" = col_character(),
      "DoseUnit" = col_character(),
      "DoseFrequency" = col_character(),
      "RouteOfMedication" = col_character(),
      "InOrOutPatient" = col_character(),
      "FormOfMedication" = col_character(),
      "THERA_CLASS" = col_character(),
      "PHARM_CLASS" = col_character(),
      "PHARM_SUBCLASS" = col_character(),
      "PAT_ENC_CSN_ID" = col_integer(),
      "OrderStatusCat" = col_character(),
      "ProviderType" = col_character(),
      "Order_Class" = col_character())
  } else if (col_types == "diagnosis_pl"){
    col_types <- cols(
      STUDY_SUBJECT_DIGEST = col_character(),
      DX_DESCRIPTION = col_character(),
      DX_DESC_DISPLAYED = col_character(),
      PROBLEM_CMT = col_character(),
      DIAGNOSIS_ENTERED_DATE = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      DIAGNOSIS_DATE = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      RESOLVED_DATE = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      DIAGNOSIS_STATUS = col_character(),
      ICD10_1 = col_character(),
      ICD10_2 = col_character(),
      ICD10_3 = col_character(),
      ICD10_4 = col_character(),
      ICD10_LIST = col_character(),
      SNOMED_CONCEPTS = col_character())
  }  else if (col_types == "med_hist"){
    col_types <- cols(
      STUDY_SUBJECT_DIGEST = col_character(),
      CONTACT_DATE = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      DX_NAME = col_character(),
      MEDICAL_HX_DATE = col_character(),
      COMMENTS = col_character(),
      MED_HX_ANNOTATION = col_character(),
      HX_LNK_ENC_CSN = col_integer(),
      CURRENT_ICD10_LIST = col_character())
  } else if (col_types == "radiology"){
    col_types <- cols(
      STUDY_SUBJECT_DIGEST = col_character(),
      Proc_Name = col_character(),
      Proc_Date = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      Proc_Code = col_character(),
      Proc_Narrative = col_character(),
      Proc_Impression = col_character(),
      Proc_Addenda = col_character(),
      Proc_Assessment = col_character())
  }
  col_types
}

#' @export
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
  }
}
