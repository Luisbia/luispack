#' A function to convert xlsx files to csv files
#'
#' @param dir
#'
#' @return xlsx files are converted with rio The filenames are the same but replacing xlsx by csv.
#' @export
#'
#' @examples
#'
#' csv_to_parquet("where_my_csv_are_stored")
xlsx_to_csv<- function(dir){
  pkg <- c("rio","stringr")
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      paste0("Package" ,pkg, " must be installed to use this function."),
      call. = FALSE
    )
  }
  list_xlsx <- list.files (path = dir,
                          pattern = glob2rx("*xlsx$"),
                          full.names = TRUE)

  xlsx_to_csv <- function(file) {
    data <- rio::import (file)
    rio::export(data,stringr::str_replace(paste0(file),".xlsx$",".csv" ))
  }

  purrr::walk(list_xlsx, xlsx_to_csv)
}
