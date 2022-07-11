packages = c("rio", "knitr", "DT", "scales","data.table", "openxlsx",
             "dplyr", "tidyr", "tibble","magrittr", "eurostat", "arrow",
             "readr","ggplot2","janitor","lubridate","purrr","stringr")

## Now load or install&load all
check_packages <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    }
  }
)
