#' Create an HTML interactive table with DT
#'
#' @description
#'
#' An interactive HTML table with some fancy options (filter,sort,export to excel) is generated from a data frame.
#'
#' @param x a data frame
#'
#' @return DT table
#' @export
#'
#' @examples
#'
#' show_DT(NUTS_2021)
#'
show_DT <- function(x){
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
  Required_Packages=c("DT")
  Install_And_Load(Required_Packages)

  DT::datatable(x, filter = "top", class = "stripe hover", extensions = "Buttons",
                options = list(  lengthMenu = list(c(20, -1), c("20", "All")),
                                 pageLength = 20, dom = "Blfrtip", buttons = c("excel"))
  )}
