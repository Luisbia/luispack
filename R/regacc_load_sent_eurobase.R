#' Get files sent to Eurobase
#'
#' @description
#'
#' This function looks at the folder where the files sent to Eurobase are stored, loads them and puts them together in a data frame.
#' @param folder specifies the folder where the files are (zip files)
#' @param table the Eurobase table to look for. It only accepts a single argument (nama_10r_2gdp, nama_10r_3gdp, nama_10r_3popgdp, nama_10r_3gva, nama_10r_3empers, nama_10r_2coe, nama_10r_2gfcf, nama_10r_2emhrw, nama_10r_2hhinc, nama_10r_2gvavr)
#' @param time_min date of publication from where to start looking (yyyy-mm-dd)
#' @param time_max date of publication from where to stop looking (yyyy-mm-dd)
#' @param sel_country country or countries to look for
#'
#' @return a data frame
#' @export
#'
#' @examples
#' df<- regacc_load_sent_eurobase(folder = "D:/data/REGACC/sent/source",
#' table="nama_10r_2gvagr",
#' time_min = "2020-01-01",
#' time_max = "2021-03-16",
#' sel_country =c("LU","EE"))



regacc_load_sent_eurobase<- function(folder,table, time_min,time_max="2022-07-11", sel_country){

  tables <- c("nama_10r_2gdp", "nama_10r_3gdp", "nama_10r_3popgdp", "nama_10r_3gva", "nama_10r_3empers", "nama_10r_2coe", "nama_10r_2gfcf", "nama_10r_2emhrw", "nama_10r_2hhinc", "nama_10r_2gvavr")

  if (table %in% tables){

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
  Required_Packages=c("rio", "tidyr","magrittr", "data.table", "stringr")

  Install_And_Load(Required_Packages)
  # import gdp, pop
  import_file_gdp <- function(file) {
  import(file,
         sep = " ",
         col.names = c("time", "geo", "unit", "values"),
         colClasses = list(time = "character", geo = "character", unit = "character", values = "character"),
         skip = 3
  ) %>%
    setDT() %>%
    .[, time := as.integer(stringr::str_sub(time, 1, 4))] %>%
    .[, country := substr(geo, start = 1, stop = 2)] %>%
    .[country %in% sel_country,] %>%
    .[, NUTS := stringr::str_length(geo)-2] %>%
    .[, c("values", "flag") := tstrsplit(values, "~")] %>%
    .[, values := as.numeric(values)]
}
#import gva
import_file_gva <- function(file) {
  import(file,
         sep = " ",
         col.names = c("time", "geo", "nace_r2", "unit", "values"),
         colClasses = list(time = "character", geo = "character", nace_r2 = "character", unit = "character", values = "character"),
         skip = 3
  ) %>%
    setDT() %>%
    .[, time := as.integer(stringr::str_sub(time, 1, 4))] %>%
    .[, country := substr(geo, start = 1, stop = 2)] %>%
    .[country %in% sel_country,] %>%
    .[, NUTS := stringr::str_length(geo)-2] %>%
    .[, c("values", "flag") := tstrsplit(values, "~")] %>%
    .[, values := as.numeric(values)]
}

# import file for emp
import_file_emp <- function(file) {
  import(file,
         sep = " ",
         col.names = c("time", "geo", "nace_r2", "na_item", "unit", "values"),
         colClasses = list(time = "character", geo = "character", nace_r2 = "character", na_item = "character", unit = "character", values = "character"),
         skip = 3
  ) %>%
    setDT() %>%
    .[, time := as.integer(stringr::str_sub(time, 1, 4))] %>%
    .[, country := substr(geo, start = 1, stop = 2)] %>%
    .[country %in% sel_country,] %>%
    .[, NUTS := stringr::str_length(geo)-2] %>%
    .[, c("values", "flag") := tstrsplit(values, "~")] %>%
    .[, values := as.numeric(values)]
}

# import file for hh
import_file_hh <- function(file) {
  import(file,
         sep = " ",
         col.names = c("time", "geo", "na_item", "acc", "unit", "values"),
         colClasses = list(time = "character", geo = "character", na_item = "character", acc = "character", unit = "character", values = "character"),
         skip = 3
  ) %>%
    setDT() %>%
    .[, time := as.integer(stringr::str_sub(time, 1, 4))] %>%
    .[, country := substr(geo, start = 1, stop = 2)] %>%
    .[country %in% sel_country,] %>%
    .[, NUTS := stringr::str_length(geo)-2] %>%
    .[, c("values", "flag") := tstrsplit(values, "~")] %>%
    .[, values := as.numeric(values)]
}

# ggvagr
import_file_gvagr <- function(file) {
  import(file,
         sep = " ",
         col.names = c("time", "geo", "unit", "values"),
         colClasses = list(time = "character", geo = "character", unit = "character", values = "character"),
         skip = 3
  ) %>%
    setDT() %>%
    .[, time := as.integer(stringr::str_sub(time, 1, 4))] %>%
    .[, country := substr(geo, start = 1, stop = 2)] %>%
    .[country %in% sel_country,] %>%
    .[, NUTS := stringr::str_length(geo)-2] %>%
    .[, c("values", "flag") := tstrsplit(values, "~")] %>%
    .[, values := as.numeric(values)]
}

# look for files
df_list<- list.files(path = folder,
           full.names = TRUE,
           pattern = glob2rx(paste0(table,"*")),
           recursive = TRUE
) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(date=file.mtime(value)) %>%
  dplyr::filter(date>= time_min & date<=time_max) %>%
  as.data.table()

##sort files
if(table == "nama_10r_2gdp"){
   df_list<-df_list[,data:=purrr::map(value,import_file_gdp)] %>%
  .[,rbindlist(data),.(date)]

 } else if (table == "nama_10r_3gdp") {
    df_list<-df_list[,data:=purrr::map(value,import_file_gdp)] %>%
      # bind the dataframes. like tidyr::unnest but faster
      .[,rbindlist(data),.(date)]
 } else if (table == "nama_10r_3popgdp") {
   df_list<-df_list[,data:=purrr::map(value,import_file_gdp)] %>%
     # bind the dataframes. like tidyr::unnest but faster
     .[,rbindlist(data),.(date)]
 } else if (table == "nama_10r_3gva") {
   df_list<-df_list[,data:=purrr::map(value,import_file_gva)] %>%
     # bind the dataframes. like tidyr::unnest but faster
     .[,rbindlist(data),.(date)]
 } else if (table == "nama_10r_3empers") {
   df_list<-df_list[,data:=purrr::map(value,import_file_emp)] %>%
     # bind the dataframes. like tidyr::unnest but faster
     .[,rbindlist(data),.(date)]
 } else if (table == "nama_10r_2coe") {
   df_list<-df_list[,data:=purrr::map(value,import_file_gva)] %>%
     # bind the dataframes. like tidyr::unnest but faster
     .[,rbindlist(data),.(date)]
 } else if (table == "nama_10r_2gfcf") {
   df_list<-df_list[,data:=purrr::map(value,import_file_gva)] %>%
     # bind the dataframes. like tidyr::unnest but faster
     .[,rbindlist(data),.(date)]
 } else if (table == "nama_10r_2wmhrw") {
   df_list<-df_list[,data:=purrr::map(value,import_file_emp)] %>%
     # bind the dataframes. like tidyr::unnest but faster
     .[,rbindlist(data),.(date)]
 } else if (table == "nama_10r_2hhinc") {
   df_list<-df_list[,data:=purrr::map(value,import_file_hh)] %>%
     # bind the dataframes. like tidyr::unnest but faster
     .[,rbindlist(data),.(date)]
 } else if (table == "nama_10r_2gvagr") {
   df_list<-df_list[,data:=purrr::map(value,import_file_gvagr)] %>%
     # bind the dataframes. like tidyr::unnest but faster
     .[,rbindlist(data),.(date)]
  } else{
    print("no such table")
  }

  }else {
    print("Wrong table: tables should be one of these:nama_10r_2gdp, nama_10r_3gdp, nama_10r_3popgdp, nama_10r_3gva, nama_10r_3empers, nama_10r_2coe, nama_10r_2gfcf, nama_10r_2emhrw, nama_10r_2hhinc, nama_10r_2gvavr" )
  }
}




