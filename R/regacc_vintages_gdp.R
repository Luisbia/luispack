#' Get regional GDP vintages from the dedicated section
#'
#' @description
#'
#' The functions downloads the excel file in the dedicated section with the historical releases of regional GDP and makes it tidy.
#' @return a data frame
#' @export
#'
#' @examples
#' df<- regacc_vintages_gdp()


regacc_vintages_gdp <- function(){

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
Required_Packages=c("rio", "tidyr","magrittr")

Install_And_Load(Required_Packages)

df <- rio::import("https://ec.europa.eu/eurostat/documents/24987/9752807/Release+dataset.xlsx/e098dbcd-3af3-f1bd-5c65-bd51c93a0560?t=1641919415365",
                  sheet ="data") %>%
  tidyr::pivot_longer(cols= starts_with("20"),
                      names_to ="time",
                      values_to = "values") %>%
  na.omit()
return (df)
}


