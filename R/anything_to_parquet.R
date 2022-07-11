#' Convert most common files to parquet files
#'
#' @description
#'
#' A function to convert most common formats to parquet. Specially useful for big files with several columns.
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
  # Function to Install and Load R Packages
  Install_And_Load <- function(Required_Packages)
  {
    Remaining_Packages <- Required_Packages[!(Required_Packages %in% installed.packages()[,"Package"])];

    if(length(Remaining_Packages))
    {
      install.packages(Remaining_Packages);
    }
    for(package_name in Required_Packages)
    {
      library(package_name,character.only=TRUE,quietly=TRUE);
    }
  }

  # Specify the list of required packages to be installed and load
  Required_Packages=c("arrow","rio","purrr")

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
