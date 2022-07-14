#' Load xlm files
#'
#' @description
#'
#' This functions loads xml files into R. We may want to do that to look at files
#' before loading them with FameFeed. The function will look by default on the server
#' folder for all countries and all tables but that can be changed by the user using
#' the arguments of the function. The unit multiplier is changed, if needed, to be
#' consistent with the one used by default elsewhere.
#'
#' @param folder specifies the folder where the files are. By default is the server folder.
#' @param country_sel Country or countries to look for.
#' @param table_sel table or tables to look for.
#' @param min_time Date from where to look for.
#' @param consolidate TRUE to remove duplicated values, FALSE (default) to keep them all
#'
#'
#' @return a data frame
#' @export regacc_load_xml
#' @import dplyr janitor readsdmx tidyr stringr purrr
#'
#' @examples
#' df<- regacc_load_xml(folder= "E:/regacc/data/input/xml",
#'                 country_sel = "PL")
#'
regacc_load_xml <- function(folder = "//fame2prod.cc.cec.eu.int/fame-estat/econ/REGACC/INPUT",
                            country_sel,
                            table_sel,
                            min_time ="2021-01-01",
                            consolidate=FALSE){
  if(missing(country_sel)) {
    country_sel<- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR",
                    "HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI",
                    "SK","NO", "ME", "MK","TR","AL","RS","UK","CH")}

  if(missing(table_sel)) {
    table_sel<- c("T1001","T1002","T1200","T1300")}


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
  Required_Packages=c("tidyverse", "janitor", "readsdmx")

  Install_And_Load(Required_Packages)
  df<-list.files(path= folder,
                 pattern = glob2rx("*xml"),
                 full.names = TRUE,
                 recursive=FALSE) %>%
    as_tibble() %>%
    mutate(date=map(value,file.mtime)) %>%
    unnest(cols=c(date)) %>%
    filter(date > min_time) %>%
    mutate(country = str_sub(value,-22,-21),
           table = str_sub(value,-30,-26)) %>%
    filter(country %in% country_sel &
             table %in% table_sel) %>%
    mutate(data=map(value,readsdmx::read_sdmx)) %>%
    unnest(cols=c(data)) %>%
    janitor::clean_names() %>%
    mutate(obs_value=as.numeric(obs_value),
           time_period=as.integer(time_period),
           unit_mult=as.numeric(unit_mult),
           obs_value= if_else(unit_measure %in% c("PS","HW") & unit_mult=="6",obs_value*1000,obs_value),
           obs_value= if_else(unit_measure %in% c("PS","HW") & unit_mult=="0",obs_value/1000,obs_value)) %>%
    mutate(unit_mult= if_else(unit_measure %in% c("PS","HW"),3,unit_mult))

  if(consolidate == TRUE){
     df <- df %>%
      arrange(date) %>%
      group_by(across(-c(value,date,last_update))) %>%
      slice_head(n=1)

     return (df)
  } else {
    return(df)
  }

}
