#' Find Eurostat datasets codes
#'
#' @description
#'This function looks at eurostat data sets codes and finds data sets that match the string provided
#'
#' @param x a string with *'s
#'
#' @return a data frame
#' @export
#'
#' @examples
#' df<-find_eurostat_dataset("nama_10r*")
#'
find_eurostat_dataset <- function(x) {
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
  Required_Packages=c("readr","tibble","janitor","dplyr","stringr")

  # Call the Function
  Install_And_Load(Required_Packages)

  df <- readr::read_delim("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&file=table_of_contents_en.txt",
                          show_col_types = FALSE) %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::filter(type == "dataset" &
             stringr::str_detect(code, glob2rx(x))) %>%
    dplyr::select(code) %>%
    dplyr::distinct()

  return(df)
}

#' Find Eurostat datasets codes based on keywords in the description
#'
#' @description
#'This function looks at eurostat data sets descriptions and finds data sets that match the string provided
#'
#' @param x a string with *s
#'
#' @return a data frame
#' @export
#'
#' @examples
#' df <- find_eurostat_desc("*GDP*")

find_eurostat_desc <- function(x) {
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
  Required_Packages=c("readr","tibble","janitor","dplyr","stringr")

  # Call the Function
  Install_And_Load(Required_Packages)

  df <- readr::read_delim("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&file=table_of_contents_en.txt",
                          show_col_types = FALSE) %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::filter(type == "dataset" &
             stringr::str_detect(title, glob2rx(x)))

  return(df)
}

#' A function to get eurostat datasets updated after a certain date
#'
#' @description
#'This function looks at eurostat data sets last updated date and filter those updated after the date provided by the user
#'
#' @param x a date in yyyy-mm-dd format
#'
#' @return a data frame
#' @export
#'
#' @examples
#' july_2022 <-find_eurostat_date("2022-07-01")
find_eurostat_date <- function(x) {
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
  Required_Packages=c("readr","tibble","janitor","dplyr","stringr")

  # Call the Function
  Install_And_Load(Required_Packages)

  df <- readr::read_delim("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&file=table_of_contents_en.txt",
                          show_col_types = FALSE) %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::filter(type == "dataset") %>%
    dplyr::mutate(last_update_of_data = lubridate::dmy(last_update_of_data)) %>%
    dplyr::filter(last_update_of_data >= x)
  return(df)
}

#' get most common national accounts dictionaries
#' Get in a dataframe the labels of the most common dictionaries used in national accounts
#'
#' @description
#'This function collects several Eurostat dictionaries frequently used in National Accounts. The main purpose is to get the labels of the codes.
#' @return a dataframe
#' @export
#'
#' @examples
#' df<- get_eurostat_label_codes()
#'
get_eurostat_label_codes <- function() {
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
  Required_Packages=c("tibble","janitor","dplyr","stringr","eurostat","tidyr")

  # Call the Function
  Install_And_Load(Required_Packages)

  labels<-    tibble(
    dictionary = c("na_item", "nace_r2", "geo", "asset10", "coicop"),
    data = list(
      eurostat::get_eurostat_dic("na_item"),
      eurostat::get_eurostat_dic("nace_r2"),
      eurostat::get_eurostat_dic("geo"),
      eurostat::get_eurostat_dic("asset10"),
      eurostat::get_eurostat_dic("coicop")
    )
  ) %>%
    tidyr::unnest(cols=c(data))

  return(labels)
}
