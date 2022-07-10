#' A function to convert xlsx files to other formats
#'
#' @param dir
#'
#' @return xlsx files are converted with rio. The filenames are the same but replacing xlsx by the chosen format.
#' @export
#'
#' @examples
#'
#' xlsx_to_anything("E:/test","csv",recursive = TRUE)
xlsx_to_anything<- function(dir, extension,recursive = FALSE){
  pkg <- c("rio","stringr")
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      paste0("Package " ,pkg, " must be installed to use this function."),
      call. = FALSE
    )
  }
  list_xlsx <- list.files (path = dir,
                          pattern = glob2rx("*.xlsx$"),
                          full.names = TRUE,
                          recursive = recursive)

  xlsx_to_anything <- function(file) {
    data <- rio::import (file)
    rio::export(data,stringr::str_replace(paste0(file),".xlsx$",paste0(extension) ))
  }

  purrr::walk(list_xlsx, xlsx_to_anything)
}
