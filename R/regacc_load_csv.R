#' Load csv files
#'
#' @description
#'
#' This function loads csv files into R. The function will look by default on the server
#' folder for all countries and all tables but that can be changed by the user using
#' the arguments of the function. The unit multiplier is changed, if needed, to be
#' consistent with the one used by default elsewhere. Not all columns in the original file are kept
#' I selected the most useful ones for numerical analysis. For qualitative info (flags, comments) it
#' is recommended to use the related function regacc_load_xml.
#'
#' @param folder specifies the folder where the files are. By default is the server folder.
#' @param country_sel Country or countries to look for.
#' @param table_sel table or tables to look for.
#' @param sto_sel NA item to look for ("B1G", c("B1G","EMP"))
#' @param unit_sel Unit to look for ("XDC", c("XDC","PC"))
#' @param min_time Date from where to look for.
#' @param max_time Date where to stop looking for.
#' @param consolidate TRUE to remove duplicated values, FALSE (default) to keep them all
#' @export regacc_load_csv
#'
#'
#' @return a data frame
#'
#' @examples
#' # Load all the files in the folder "//fame2prod.cc.cec.eu.int/fame-estat/econ/REGACC/INSPACE"
#' df<- regacc_load_csv()
#'
#' # Load only table T1001.
#' df <- regacc_load_csv(table_sel = "T1001")
#'
#' # Load all tables T1001 and T1300 for Slovenia and Luxembourg.
#' df <- regacc_load_xml(table_sel = c("T1001","T1300"),
#'                       country_sel = c("SI", "LU"))
#'
#' # Load all files loaded between 2021-12-22 2022-01-02 and consolidate them.
#' df <- regacc_load_xml(min_time = "2021-12-22",
#'                       max_time = "2022-01-02"
#'                       consolidate = TRUE)
#'
regacc_load_csv <- function(folder = "E:/data/REGACC/csv",
                            country_sel,
                            table_sel,
                            min_time ="2021-10-01",
                            max_time ="2099-01-01",
                            consolidate=FALSE){
  if(missing(country_sel)) {
    country_sel<- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR",
                    "HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI",
                    "SK","NO", "ME", "MK","TR","AL","RS","UK","CH")}

  if(missing(table_sel)) {
    table_sel<- c("T1001","T1002","T1200","T1300")}

  if(missing(sto_sel)) {
    table_sel<- c("B1G","EMP","POP","D1","P51G","SAL","B2A3N","D4","B5N","D62","D7","D5","D61","B6N","D63","B7N","P51C","P3")}

  if(missing(unit_sel)) {
    table_sel<- c("XDC","PC","PS","HW")}


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
  Required_Packages=c("tidyverse", "janitor", "data.table")

  Install_And_Load(Required_Packages)
  df<-list.files(path= folder,
                 pattern = glob2rx("*csv$"),
                 full.names = TRUE,
                 recursive=FALSE) %>%
    as_tibble() %>%
    mutate(date=map(value,file.mtime)) %>%
    unnest(cols=c(date)) %>%
    filter(date > min_time &
           date < max_time) %>%
    mutate(country = str_sub(value,-26,-25),
           table = str_sub(value,-34,-30)) %>%
    filter(country %in% country_sel &
             table %in% table_sel) %>%
    mutate(data=map(value,data.table::fread)) %>%
    unnest(cols=c(data)) %>%
    janitor::clean_names() %>%
    select(value,date,country,table_identifier,ref_area,accounting_entry,sto,activity,prices,unit_measure,time_period,obs_value,obs_status, unit_mult) %>%
    mutate(obs_value=as.numeric(obs_value),
           time_period=as.integer(time_period),
           unit_mult=as.numeric(unit_mult),
           obs_value= if_else(unit_measure %in% c("PS","HW") & unit_mult=="6",obs_value*1000,obs_value),
           obs_value= if_else(unit_measure %in% c("PS","HW") & unit_mult=="0",obs_value/1000,obs_value)) %>%
    mutate(unit_mult= if_else(unit_measure %in% c("PS","HW"),3,unit_mult)) %>%
    select(-unit_mult)%>%
    filter(sto %in% sto_sel & unit_measure %in% unit_sel )

  if(consolidate == TRUE){
     df <- df %>%
      arrange(date) %>%
      group_by(across(-c(value,date))) %>%
      slice_head(n=1)

     return (df)
  } else {
    return(df)
  }

}
