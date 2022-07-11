#' Get a dataframe with the latest Eurobase annual GDP
#'
#' @description
#'This function downloads from Eurobase the latest GDP data and other variables in the table nama_10_gdp for the unit declared by the user.
#' @param na_item_sel which na_item to download, B1GQ by default
#' @param unit_sel which unit, CP_MEUR by default
#'
#' @return a data.frame/data.table
#' @export
#'
#' @examples
#' dt<- get_annual_GDP(na_item_sel=c("B1GQ", "D1"), unit_sel= c("CP_MEUR","CP_MNAC"))
get_annual_GDP <- function(na_item_sel = "B1GQ",
                             unit_sel ="CP_MEUR")
  {
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
  Required_Packages=c("eurostat")

  # Call the Function
  Install_And_Load(Required_Packages)

   dt<-eurostat::get_eurostat("nama_10_gdp",
                                   filters=list(na_item=na_item_sel,
                                                unit = unit_sel,
                                   label=FALSE))
  dt<- na.omit(dt)
  return(dt)}


#' Get a dataframe with the latest Eurobase quarterly GDP
#'
#' @description
#'This function downloads from Eurobase the latest quarterly GDP data and other variables in the table namq_10_gdp for the unit and adjustment declared by the user.
#' @param na_item_sel which na_item to download, B1GQ by default
#' @param unit_sel which unit, CLV_PCH_PRE by default
#' @param min_date from which year, 2020 by default
#' @param s_adj_sel which adjustment, SCA by default
#'
#'
#' @return a data.frame/data.table
#' @export
#'
#' @examples
#' dt<- get_quarterly_GDP(na_item_sel=c("B1GQ"), unit_sel= c("CP_MEUR","CP_MNAC"), s_adj_sel = "SCA")
get_quarterly_GDP <- function(na_item_sel = "B1GQ",
                                unit_sel ="CLV_PCH_PRE",
                                s_adj_sel="SCA"){
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
  Required_Packages=c("eurostat")
  # Call the Function
  Install_And_Load(Required_Packages)

  dt<-eurostat::get_eurostat("namq_10_gdp",
                                   filters=list(na_item=na_item_sel,
                                                unit = unit_sel,
                                                s_adj = s_adj_sel),
                                   label=FALSE)
  dt<- na.omit(dt)
  return(dt)}

#' Get a dataframe with the latest Eurobase regional GDP
#'
#' @description
#'This function downloads from Eurobase the latest regional GDP data in the table nama_10r_3gdp for the unit declared by the user.
#'
#' @param unit_sel which unit, all by default. Options are "MIO_EUR","EUR_HAB","	EUR_HAB_EU27_2020", "MIO_NAC", "MIO_PPS_EU27_2020", "PPS_EU27_2020_HAB","	PPS_HAB_EU27_2020".
#'
#'
#' @return a data.frame/data.table
#' @export
#'
#' @examples
#' dt<- get_regional_GDP(unit_sel= c("MIO_EUR","EUR_HAB"))
get_regional_GDP <- function(unit_sel =c("MIO_EUR",
                                         "EUR_HAB",
                                         "EUR_HAB_EU27_2020",
                                         "MIO_NAC",
                                         "MIO_PPS_EU27_2020",
                                         "PPS_EU27_2020_HAB",
                                         "PPS_HAB_EU27_2020"))
  {
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
  Required_Packages=c("eurostat","dplyr")
  # Call the Function
  Install_And_Load(Required_Packages)

  dt<-eurostat::get_eurostat("nama_10r_3gdp",
                             time_format = "num",
                                   filters=list(unit = unit_sel),
                                   label=FALSE)
  dt<- dplyr::left_join(luispack::NUTS_2021,dt)
  dt<- na.omit(dt)
  return(dt)}


