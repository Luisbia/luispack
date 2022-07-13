#' Load all xml cvs files
#'
#' @description
#'
#' The objective of this function is to load several *xml.csv* files stored in [fame2prod.cc.cec.eu.int/fame-estat/econ/REGACC/INSPACE]("//fame2prod.cc.cec.eu.int/fame-estat/econ/REGACC/INSPACE"). They are created after xml files are loaded with FameFeed.
#' The function needs these packages: `{data.table}`, `{tidyverse}` and `{janitor}`.
#' The function has several arguments:
#'
#' - folder: the path to the files. Normally it will be [fame2prod.cc.cec.eu.int/fame-estat/econ/REGACC/INSPACE]("//fame2prod.cc.cec.eu.int/fame-estat/econ/REGACC/INSPACE") and is set as the default option that is used if no argument is provided.
#'
#' - geo: the countries to look at. It accepts one ("LU") or several c("LU","MT") arguments. No argument is provided by default.
#'
#' - tables: the table ("T1001") or tables c("T1200","T1300") to look for. All tables are selected by default,
#'
#' - time_min: the function looks at the time stamp of the files. We can filter for files created after a certain date. The default option is ""2020-01-01".
#'
#' - time_max: We can filter for files created before a certain date. The default option is ""2099-12-31".
#'
#' @param folder path to the xml.csv files.
#' @param geo country(ies) to look for.
#' @param tables tables to look for.
#' @param time_min file creation should be above this limit(yyyy-mm-dd).
#' @param time_max file creation should be below this limit(yyyy-mm-dd).
#' @return a data frame
#' @export
#'
#' @examples
#' regacc_load_all_csv(folder ="E:/data/REGACC/csv",
#'                    geo = c("MT", "LU"),
#'                    time_min = "2021-12-01")
regacc_load_all_csv <- function(folder= "//fame2prod.cc.cec.eu.int/fame-estat/econ/REGACC/INSPACE",
                                geo,
                                tables=c("T1001","T1002","T1200","T1300"),
                                time_min = "2020-01-01",
                                time_max = "2099-12-31" ){
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
  Required_Packages=c("data.table", "tidyverse", "janitor")

  # Call the Function
  Install_And_Load(Required_Packages)

  df_list <-list.files (path = folder,
                        full.names = TRUE,
                        pattern = glob2rx("*xml.csv$"),
                        recursive = TRUE)

    df_list<-tibble::as_tibble(df_list) %>%
    dplyr::mutate(table = stringr::str_sub(value,-34,-30),
            country = stringr::str_sub(value,-26,-25),
            version = stringr::str_sub(value,-11,-9),
            version = as.numeric(version)) %>%
     dplyr::filter(!is.na(version)) %>%
     dplyr::filter(country %in% c(geo)) %>%
      dplyr::filter(table %in% c(tables)) %>%
     dplyr::mutate(date=purrr::map(value,file.mtime)) %>%
      tidyr::unnest(date) %>%
      dplyr::select(value,table,country,version,date) %>%
      dplyr::filter(date >= time_min) %>%
      dplyr::filter(date <= time_max)


    import_file <- function (file) {
      data.table::fread(file, sep = ";") %>%
        janitor::clean_names() %>%
        dplyr::select(table_identifier,ref_area,accounting_entry,sto,activity,unit_measure,time_period,obs_value, obs_status, conf_status) %>%
        dplyr::mutate_if(is.character,as.factor) %>%
        dplyr::mutate (obs_value = as.numeric(obs_value))
    }

  df_list<-df_list %>%
    dplyr::mutate(data=purrr::map(value,import_file)) %>%
    tidyr::unnest(cols = c(data)) %>%
    select(-value,-table) %>%

return(df_list)
    }
