#' Apply the new Eurostat palette to fill aesthetics in ggplot
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars,aes(cyl,mpg, fill=as.factor(cyl)))+
#' geom_col()+
#' scale_fill_eurostat()
#'
scale_fill_eurostat <- function () {
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
  Required_Packages=c("ggplot2" )
  Install_And_Load(Required_Packages)

  # Call the Function
  Install_And_Load(Required_Packages)
  ec_colours <- c("#0E47CB", #'EU Blue'
                  "#FFCC00", # 'EU Yellow'
                  "#7D8088",# 'EU Grey 60'
                  "#208486",# 'Teal'
                  "#AF155C",# 'Berry'
                  "#AA5F18",# 'Sienna'
                  "#B656BD",# 'Fuchsia'
                  "#672DC4",# 'Dark orchid'
                  "#E04040",# 'Sunset red'
                  "#388AE2",# 'Cornflower'
                  "#B39421", #'Dark Gold'
                  "#2644A7", #'Cobalt'
                  "#33A033",#'Forest Green'
                  "#262B38") #'EU Grey 100'


  ggplot2::scale_fill_manual(values = ec_colours)
}

#' Apply the new Eurostat palette to  colour aesthetics in ggplot
#'
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars,aes(cyl,mpg, colour=as.factor(cyl)))+
#' geom_point(size=3)+
#' scale_colour_eurostat()

scale_colour_eurostat <- function () {
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
  Required_Packages=c("ggplot2" )
  Install_And_Load(Required_Packages)

  # Call the Function
  Install_And_Load(Required_Packages)
  ec_colours <- c("#0E47CB", #'EU Blue'
                  "#FFCC00", # 'EU Yellow'
                  "#7D8088",# 'EU Grey 60'
                  "#208486",# 'Teal'
                  "#AF155C",# 'Berry'
                  "#AA5F18",# 'Sienna'
                  "#B656BD",# 'Fuchsia'
                  "#672DC4",# 'Dark orchid'
                  "#E04040",# 'Sunset red'
                  "#388AE2",# 'Cornflower'
                  "#B39421", #'Dark Gold'
                  "#2644A7", #'Cobalt'
                  "#33A033",#'Forest Green'
                  "#262B38") #'EU Grey 100'

  ggplot2::scale_colour_manual(values = ec_colours)
}



