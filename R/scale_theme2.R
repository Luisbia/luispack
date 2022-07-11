#' Apply Eurostat Theme2 palette to fill aesthetics in ggplot
#'
#' @examples
#'
#' ggplot2::ggplot(mtcars,aes(cyl,mpg, fill=as.factor(cyl)))+
#' geom_col()+
#' scale_fill_theme2()
#'
scale_fill_theme2 <- function () {
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

  # Call the Function
  Install_And_Load(Required_Packages)
  theme2_pal <- c("#af4b91",#theme2
                  "#466eb4",
                  "#b9c337",
                  "#41afaa",
                  "#b93c46",
                  "#00a0e1",
                  "#9b3278", #theme2
                  "#2d50a0",
                  "#a0aa37",
                  "#23969b",
                  "#aa192d",
                  "#0087cd",
                  "#be78aa", #theme2
                  "#6487c3",
                  "#c8cd64",
                  "#73bebe",
                  "#c86e64",
                  "#64b4e6")


  ggplot2::scale_fill_manual(values = theme2_pal)
}

#' Apply Eurostat Theme2 palette to colour aesthetics in ggplot
#'
#'
#' @examples
#' ggplot2::ggplot(mtcars,aes(cyl,mpg, colour=as.factor(cyl)))+
#' geom_point(size=3)+
#' scale_colour_theme2()

scale_colour_theme2 <- function () {
  theme2_pal <- c("#af4b91",#theme2
                  "#466eb4",
                  "#b9c337",
                  "#41afaa",
                  "#b93c46",
                  "#00a0e1",
                  "#9b3278", #theme2
                  "#2d50a0",
                  "#a0aa37",
                  "#23969b",
                  "#aa192d",
                  "#0087cd",
                  "#be78aa", #theme2
                  "#6487c3",
                  "#c8cd64",
                  "#73bebe",
                  "#c86e64",
                  "#64b4e6")

  ggplot2::scale_colour_manual(values = theme2_pal)
}

