#' Convert xlsx files to other formats
#'
#' @description
#'
#'Convert xlsx files to almost any other format. Works in sub-folders if recursive is set to TRUE. Original files are not deleted in case they want to be kept and for precautionary reasons.
#'
#' @param dir older in which the excel files to be converted are stored
#' @param extension extension to which the files will be converted (for example,".csv")
#' @param recursive whether lo kook recursively in sub-folders, FALSE by default
#'
#' @return xlsx files are converted with rio. The filenames are the same but replacing xlsx by the chosen format.
#' @export
#'
#' @examples
#'
#' xlsx_to_anything("E:/test",".csv",recursive = TRUE)
xlsx_to_anything<- function(dir, extension,recursive = FALSE){
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
  Required_Packages=c("rio", "purrr")
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
