#' Load files from MDT
#'
#'Loads the historical files that we keep from the internal copy of MDT.
#'The location of the files in the server is:
#' @param folder Path to the location of the files
#' @param table (Unique) table to load (nama_10r_2gdp, nama_10r_3gdp, nama_10r_3popgdp, nama_10r_3gva, nama_10r_3empers, nama_10r_2coe, nama_10r_2gfcf, nama_10r_2emhrw, nama_10r_2hhinc, nama_10r_2gvavr).
#'
#' @return a data frame/data.table object.
#' @export
#'
#' @examples
#'
#'df <- regacc_load_MDT(folder = "E:/data/REGACC/MDT/source",
#'                      table = "nama_10r_2gp")

regacc_load_MDT <- function(folder, table){

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
  Required_Packages=c("tidyverse", "data.table")

  Install_And_Load(Required_Packages)

  tables <- c("nama_10r_2gdp", "nama_10r_3gdp", "nama_10r_3popgdp", "nama_10r_3gva", "nama_10r_3empers", "nama_10r_2coe", "nama_10r_2gfcf", "nama_10r_2emhrw", "nama_10r_2hhinc", "nama_10r_2gvavr")


import_MDT<- function(file) {
  df<-data.table::fread(file,fill = TRUE)%>%
 .[,obs_decimals :=NULL] %>%
  .[, country := substr(geo, start = 1, stop = 2)] %>%
  .[, NUTS := as.factor(stringr::str_length(geo)-2)] %>%
  .[, obs_value := as.numeric(obs_value)] %>%
  .[, time := as.integer(time)] %>%
  .[, obs_status := as.factor(obs_status)]
}


df <- list.files(
  path = folder,
  pattern = glob2rx(paste0("*",table,".dat")),
  recursive = TRUE,
  full.names = TRUE) %>%
as.data.table() %>%
setnames(".","value") %>%
.[,data:=purrr::map(value,import_MDT)]%>%
.[,rbindlist(data),.(value)] %>%
.[,vintage:= as.integer(str_sub(value,-28,-25))] %>%
.[,value:=NULL]

return(df)

} else {
  print("Wrong table: tables should be one of these:nama_10r_2gdp, nama_10r_3gdp, nama_10r_3popgdp, nama_10r_3gva, nama_10r_3empers, nama_10r_2coe, nama_10r_2gfcf, nama_10r_2emhrw, nama_10r_2hhinc, nama_10r_2gvavr" )
}
}


