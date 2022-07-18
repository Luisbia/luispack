#' Load files sent to Eurobase
#'
#' This function looks at the folder where the files sent to Eurobase are stored, loads them and puts them together in a data frame.
#' @param folder specifies the folder where the files are (zip files)
#' @param table_sel the Eurobase table to look for. It only accepts a single argument (nama_10r_2gdp, nama_10r_3gdp, nama_10r_3popgdp, nama_10r_3gva, nama_10r_3empers, nama_10r_2coe, nama_10r_2gfcf, nama_10r_2emhrw, nama_10r_2hhinc, nama_10r_2gvavr)
#' @param country_sel country or countries to look for
#' @param time_min date of publication from where to start looking (yyyy-mm-dd)
#' @param time_max date of publication from where to stop looking (yyyy-mm-dd)
#' @param consolidate TRUE to remove duplicated values, FALSE (default) to keep them all
#' @export load_sent_eurobase
#'
#' @return a data frame
#' @export
#'
#' @examples
#' df<- load_sent_eurobase(folder = "E:/data/REGACC/sent/source",
#' table_sel="nama_10r_2gvagr",
#' country_sel = c("ES","PT"),
#' time_min = "2020-01-01",
#' time_max = "2021-03-16",
#' country_sel =c("LU","EE"),
#' consolidate = TRUE)



load_sent_eurobase<- function(folder,table_sel, country_sel,time_min= "2019-01-01",time_max="2022-07-11", consolidate = FALSE){

  tables <- c("nama_10r_2gdp", "nama_10r_3gdp", "nama_10r_3popgdp", "nama_10r_3gva", "nama_10r_3empers", "nama_10r_2coe", "nama_10r_2gfcf", "nama_10r_2emhrw", "nama_10r_2hhinc", "nama_10r_2gvagr")

  if (table_sel %in% tables){

    luispack::check_packages()

    if(missing(country_sel)) {
      country_sel<- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR",
                      "HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI",
                      "SK","NO", "ME", "MK","TR","AL","RS","UK","CH")}

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
        .[country %in% country_sel,] %>%
        .[, NUTS := stringr::str_length(geo)-2] %>%
        .[, c("values", "flag") := tstrsplit(values, "~")] %>%
        .[, values := as.numeric(values)] %>%
        .[country %in% country_sel,]
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
        .[country %in% country_sel,] %>%
        .[, NUTS := stringr::str_length(geo)-2] %>%
        .[, c("values", "flag") := tstrsplit(values, "~")] %>%
        .[, values := as.numeric(values)] %>%
        .[country %in% country_sel,]
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
        .[country %in% country_sel,] %>%
        .[, NUTS := stringr::str_length(geo)-2] %>%
        .[, c("values", "flag") := tstrsplit(values, "~")] %>%
        .[, values := as.numeric(values)] %>%
        .[country %in% country_sel,]
    }

    # import file for hh
    import_file_hh <- function(file) {
      import(file,
             sep = " ",
             col.names = c("time", "geo", "na_item", "direct", "unit", "values"),
             colClasses = list(time = "character", geo = "character", na_item = "character", acc = "character", unit = "character", values = "character"),
             skip = 3
      ) %>%
        setDT() %>%
        .[, time := as.integer(stringr::str_sub(time, 1, 4))] %>%
        .[, country := substr(geo, start = 1, stop = 2)] %>%
        .[country %in% country_sel,] %>%
        .[, NUTS := stringr::str_length(geo)-2] %>%
        .[, c("values", "flag") := tstrsplit(values, "~")] %>%
        .[, values := as.numeric(values)] %>%
        .[country %in% country_sel,]
    }

    # gvagr
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
        .[country %in% country_sel,] %>%
        .[, NUTS := stringr::str_length(geo)-2] %>%
        .[, c("values", "flag") := separate(values, "~")] %>%
        .[, values := as.numeric(values)] %>%
        .[country %in% country_sel,]
    }

    # look for files
    df_list<- list.files(path = folder,
                         full.names = TRUE,
                         pattern = glob2rx(paste0(table_sel,"*")),
                         recursive = TRUE
    ) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(date=file.mtime(value)) %>%
      dplyr::filter(date>= time_min & date<=time_max) %>%
      as.data.table()

    ##sort files
    if(table_sel == "nama_10r_2gdp"){
      df_list<-df_list[,data:=purrr::map(value,import_file_gdp)] %>%
        .[,rbindlist(data),.(date)]

    } else if (table_sel == "nama_10r_3gdp") {
      df_list<-df_list[,data:=purrr::map(value,import_file_gdp)] %>%
        # bind the dataframes. like tidyr::unnest but faster
        .[,rbindlist(data),.(date)]
    } else if (table_sel == "nama_10r_3popgdp") {
      df_list<-df_list[,data:=purrr::map(value,import_file_gdp)] %>%
        # bind the dataframes. like tidyr::unnest but faster
        .[,rbindlist(data),.(date)]
    } else if (table_sel == "nama_10r_3gva") {
      df_list<-df_list[,data:=purrr::map(value,import_file_gva)] %>%
        # bind the dataframes. like tidyr::unnest but faster
        .[,rbindlist(data),.(date)]
    } else if (table_sel == "nama_10r_3empers") {
      df_list<-df_list[,data:=purrr::map(value,import_file_emp)] %>%
        # bind the dataframes. like tidyr::unnest but faster
        .[,rbindlist(data),.(date)]
    } else if (table_sel == "nama_10r_2coe") {
      df_list<-df_list[,data:=purrr::map(value,import_file_gva)] %>%
        # bind the dataframes. like tidyr::unnest but faster
        .[,rbindlist(data),.(date)]
    } else if (table_sel == "nama_10r_2gfcf") {
      df_list<-df_list[,data:=purrr::map(value,import_file_gva)] %>%
        # bind the dataframes. like tidyr::unnest but faster
        .[,rbindlist(data),.(date)]
    } else if (table_sel == "nama_10r_2wmhrw") {
      df_list<-df_list[,data:=purrr::map(value,import_file_emp)] %>%
        # bind the dataframes. like tidyr::unnest but faster
        .[,rbindlist(data),.(date)]
    } else if (table_sel == "nama_10r_2hhinc") {
      df_list<-df_list[,data:=purrr::map(value,import_file_hh)] %>%
        # bind the dataframes. like tidyr::unnest but faster
        .[,rbindlist(data),.(date)]
    } else if (table_sel == "nama_10r_2gvagr") {
      df_list<-df_list[,data:=purrr::map(value,import_file_gvagr)] %>%
        # bind the dataframes. like tidyr::unnest but faster
        .[,rbindlist(data),.(date)]
    } else{
      print("no such table")
    }

  }else {
    print("Wrong table: tables should be one of these:nama_10r_2gdp, nama_10r_3gdp, nama_10r_3popgdp, nama_10r_3gva, nama_10r_3empers, nama_10r_2coe, nama_10r_2gfcf, nama_10r_2emhrw, nama_10r_2hhinc, nama_10r_2gvavr" )
  }
  if(consolidate == TRUE){
    df_list <- df_list %>%
      arrange(date) %>%
      group_by(across(-c(values,date))) %>%
      slice_head(n=1)}

  return(df_list)

}




