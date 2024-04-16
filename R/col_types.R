#' Standard column specifications
#'
#' Maps table names to standard [`readr::cols()`] specifications that can be
#' used for importing data using [`read_tidybrookes_csv()`] or
#' [`read_tidybrookes_delim()`].
#'
#' @param col_types Either a string specifying the name of the table that the
#'   data comes from, for which a standard format can be used. Available
#'   value are `"adm"`, `"adt"`, `"demogs"`, `"fsheet"`, `"fsheet_io"`,
#'   `"tests"`, `"med_admin"`, `"med_prescr"`, `"diagnosis_pl"`, `"med_hist"`,
#\   `"radiology"`.
#'
#'   Or a [readr::cols()] format list of column names and types, which
#'   can be used where a nonstandard data format are supplied.
#' @returns A [`readr::cols()`] specification.
#' @seealso [read_tidybrookes_csv()], [read_tidybrookes_csv()]
#' @examples
#' # the format when col_types = "adm"
#' default_col_types("adm")
#'
#' default_col_types("tests")
#' @export
default_col_types <- function(col_types){
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

col_is_datetime <- function(x){
  class(x)[1] == "collector_datetime"
}

col_types_use_clock <- function(nonexistent, ambiguous){
  !is.null(nonexistent) || !is.null(ambiguous)
}

col_types_rewrite_datetime_as_character <- function(x){
  x$cols <- sapply(x$cols, function(y){
    if (col_is_datetime(y)){
      col_character()
    } else {
      y
    }
  })
  x
}

col_extract_datetime_format <- function(x){
  is_datetime <- vapply(x$cols, col_is_datetime, logical(1))
  out <- sapply(x$cols[is_datetime],
                function(y){
                  y$format
                })
  names(out) <- names(x$cols)[is_datetime]
  out
}

# @importFrom dplyr cur_column
parse_datetime_cols_if_clock <- function(x,
                                         col_types,
                                         tz,
                                         nonexistent,
                                         ambiguous){
  if (col_types_use_clock(nonexistent, ambiguous)){
    cols_datetime_formats <- col_extract_datetime_format(col_types)
    x %>%
      mutate(across(names(cols_datetime_formats),
                    ~ clock::date_time_parse(
                      .x,
                      format = cols_datetime_formats[dplyr::cur_column()],
                      zone = tz,
                      nonexistent = nonexistent,
                      ambiguous = ambiguous)))
  } else {
    x
  }
}

col_types_rewrite_if_clock <- function(col_types,
                                       ambiguous,
                                       nonexistent){
  if (col_types_use_clock(nonexistent, ambiguous)){
    col_types <- col_types_rewrite_datetime_as_character(col_types)
  }
  col_types
}
