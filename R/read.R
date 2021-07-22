#' @importFrom readr read_csv cols col_character col_integer col_double locale col_datetime
read_tidybrookes_csv <- function(file, col_types){
  if (col_types == "adm"){
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
  } else if (col_types == "fsheet"){
    col_types <- cols(
      STUDY_SUBJECT_DIGEST = col_character(),
      fsd_id = col_integer(),
      line = col_integer(),
      flo.meas_id = col_double(),
      MEASURE_TIME = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      disp_name = col_character(),
      measured_value = col_character(),
      meas_comment = col_character(),
      template = col_character(),
      form = col_character())
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
  }
  read_csv(file = file,
           col_types = col_types,
           locale = locale(tz = "Europe/London")) %>%
    as_tibble
}
