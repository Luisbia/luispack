#' Convert most common file formats
#'
#' @description
#' A function to convert most common formats. Specially useful when we need to compress to a certain format a lot of files.
#'
#' @param dir folder in which the files to be converted are stored
#' @param origin extension of the files to be converted, for example ("xlsx")
#' @param dest extension to which the files will be converted, for example ("csv").
#' @param recursive whether lo kook recursively in sub-folders, FALSE by default
#'
#' @return files are read/written with rio, the same name is kept
#' @export convert_anything
#' @examples
#' # convert all csv files in folder E:/test and sub-folders to parquet
#' convert_anything("E:/test",
#'                  "csv",
#'                  "parquet",
#'                  recursive = TRUE)
#'
convert_anything<- function(dir, origin, dest, recursive = FALSE){

  # Specify the list of required packages to be installed and load
  Required_Packages=c("arrow","rio","purrr")
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

  Install_And_Load(Required_Packages)


  list_files <- list.files (path = dir,
                            pattern = glob2rx(paste0("*",origin,"$")),
                            full.names = TRUE,
                            recursive=recursive)

  convert <- function(file) {
    data <- rio::import(file)
    export(data,file=stringr::str_replace(paste0(file),paste0(origin,"$"),paste0(dest)))
  }

  purrr::walk(list_files, convert)
}
