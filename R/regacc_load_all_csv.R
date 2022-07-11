#' Load all xml cvs files
#'
#' @description
#'
#'Loads the csv.xml files crated by Famefeed in a data frame.
#'
#' @param folder path to the xml.csv files.
#' @param geo country(ies) to look for.
#' @param time date from which to take the files.
#'
#' @return a data frame
#' @export
#'
#' @examples
#' df<- regacc_load_all_csv(folder ="F:/data/REGACC/csv",
#'                          geo = c("MT", "LU"),
#'                          time = "2021-12-01")
regacc_load_all_csv <- function(folder,geo,time){
  library(magrittr)
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
     dplyr::mutate(date=purrr::map(value,file.mtime)) %>%
      tidyr::unnest(date) %>%
      dplyr::select(value,table,country,version,date) %>%
      dplyr::filter(date >= time)


    import_file <- function (file) {
      data.table::fread(file, sep = ";") %>%
        janitor::clean_names() %>%
        dplyr::select(table_identifier,ref_area,accounting_entry,sto,activity,unit_measure,time_period,obs_value, obs_status, conf_status) %>%
        dplyr::mutate_if(is.character,as.factor) %>%
        dplyr::mutate (obs_value = as.numeric(obs_value))
    }

  df_list<-df_list %>%
    dplyr::mutate(data=purrr::map(value,import_file)) %>%
    tidyr::unnest(cols = c(data))
return(df_list)
    }


