#' NUTS 2021 classification
#'
#' A data frame with the NUTS 2021 codes and labels.
#'
#' @format A data.frame / data.table:
#' \describe{
#'   \item{Country}{Country code}
#'   \item{NUTS}{NUTS level:0 and 2}
#'   \item{geo}{Code of the region (AT11,BE100,...)}
#'   \item{na_item}{Code of the transaction (B1G,EMP,D1,B6N)}
#'   \item{vintage}{year the data was received}
#'   \item{time}{reference oeriod}
#'   \item{values}{values}
#' }
#' @source \url{internal}
"NQR_data"
#' library(tidyverse)
#' df<- list.files("D:/03_Regional Accounts/03D_Data Production/2021/revision_NQR/data",
#'                 pattern="*.csv$",
#'                 recursive=FALSE,
#'                 full.names=TRUE) %>%
#'   purrr::map(.,data.table::fread) %>%
#'   data.table::rbindlist(fill=TRUE) %>%
#'   dplyr::mutate(across(starts_with("20"), as.numeric)) %>%
#'   tidyr::pivot_longer(cols=starts_with("20"),
#'                       names_to= "time",
#'                       values_to= "values") %>%
#'   na.omit()

NQR_data<- df
#'
#'
#'
"NUTS_2021"
