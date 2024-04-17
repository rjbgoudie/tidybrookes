#' QC report
#'
#' @export
qc_report <- function(adm_data,
                      adt_data,
                      tests_data,
                      tests_def,
                      fsheet_data,
                      fsheet_def,
                      output_dir = getwd(),
                      ...){
  path <- rmarkdown::render(
    input = paste0(system.file(package = "tidybrookes"), "/rmd/qc_report.Rmd"),
    params = list(adm_data = adm_data,
                  adt_data = adt_data,
                  tests_data = tests_data,
                  tests_def = tests_def,
                  fsheet_data = fsheet_data,
                  fsheet_def = fsheet_def),
    output_dir = output_dir,
    ...
  )
  viewer <- getOption("viewer")
  if (!is.null(viewer)){
    rstudioapi::viewer(path)
  } else {
    utils::browseURL(path)
  }
}
