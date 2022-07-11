#'Export to excel a data frame
#'
#'@description
#'
#'For closer inspection of small data frames this functions exports it to excel and opens the file.
#' @param .data
#'
#' @return an excel file in a temporary folder
#' @export
#'
#' @examples
#'
#' show_in_excel(NUTS_2021)
show_in_excel <- function(.data){
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
  Required_Packages=c("openxlsx")
  Install_And_Load(Required_Packages)

  tmp <- paste0(tempfile(), ".xlsx")
  openxlsx::write.xlsx(.data,tmp)
  browseURL(tmp)
}
