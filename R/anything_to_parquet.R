#' A function to convert most common files to parquet files
#'
#' @param dir folder in which the files to be converted are stored
#' @param extension extension of the files to be converted, for example (.xlsx)
#' @param recursive whether lo kook recursively in sub-folders, FALSE by default
#'
#' @return csv files are read with rio and converted to parquet with arrow. The file names are the same but replacing the extention
#' @export
#'
#' @examples
#'
#' anything_to_parquet("E:/test",".csv", recursive = TRUE)
anything_to_parquet<- function(dir, extension, recursive = FALSE){
  pkg <- c("rio","arrow","stringr")
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      paste0("Package " ,pkg, " must be installed to use this function."),
      call. = FALSE
    )
  }
  list_files <- list.files (path = dir,
                          pattern = glob2rx(paste0("*",extension,"$")),
                          full.names = TRUE,
                          recursive=recursive)

  anything_to_parquet <- function(file) {
    data <- rio::import(file)
    arrow::write_parquet(data,stringr::str_replace(paste0(file),paste0(extension,"$"),".parquet"))
  }

  purrr::walk(list_files, anything_to_parquet)
}
