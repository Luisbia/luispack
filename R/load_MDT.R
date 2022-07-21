#' Load files from MDT
#'
#'Loads the historical files that we keep from the internal copy of MDT.
#'The location of the files in the server is:
#' @param folder Path to the location of the files
#' @param table_sel (Unique) table to load (nama_10r_2gdp, nama_10r_3gdp, nama_10r_3popgdp, nama_10r_3gva, nama_10r_3empers, nama_10r_2coe, nama_10r_2gfcf, nama_10r_2emhrw, nama_10r_2hhinc, nama_10r_2gvavr).
#' @param country_sel Filter for a particular country/ies.
#' @return a data frame/data.table object.
#' @export load_MDT
#'
#' @examples
#'
#'df <- load_MDT(folder = "E:/data/REGACC/MDT/source",
#'                        table_sel = "nama_10r_2gdp",
#'                        country_sel = c("ES","PT"))

load_MDT <- function(folder, table_sel, country_sel){
  luispack::check_packages()

    if(missing(country_sel)) {
    country_sel<- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR",
                    "HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI",
                    "SK","NO", "ME", "MK","TR","AL","RS","UK","CH")}

  tables <- c("nama_10r_2gdp", "nama_10r_3gdp", "nama_10r_3popgdp", "nama_10r_3gva", "nama_10r_3empers", "nama_10r_2coe", "nama_10r_2gfcf", "nama_10r_2emhrw", "nama_10r_2hhinc", "nama_10r_2gvavr")

  if (table_sel %in% tables){

      import_MDT<- function(file) {
       data.table::fread(file,fill = TRUE)%>%
        .[,obs_decimals :=NULL] %>%
        .[, country := substr(geo, start = 1, stop = 2)] %>%
        .[, NUTS := as.factor(stringr::str_length(geo)-2)] %>%
        .[, obs_value := as.numeric(obs_value)] %>%
        .[, time := as.integer(time)] %>%
        .[, obs_status := as.factor(obs_status)] %>%
        .[country %in% country_sel,]
    }


    df <- list.files(
      path = folder,
      pattern = glob2rx(paste0("*",table_sel,".dat")),
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
