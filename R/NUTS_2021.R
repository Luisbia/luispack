#' NUTS 2021 classification
#'
#' The data corresponds to the data released by Eurostat in the year 2022 in the several datasets related to Regional Accounts. It consolidates all of them in a single table to make easier to combine them and adds some useful features like a country code, NUTS level, etc.
#'
#' @format A data.frame / data.table:
#' \describe{
#'   \item{country}{Country code}
#'   \item{NUTS}{NUTS level:0,1,2 and 3}
#'   \item{geo}{Code of the region (AT11,BE100,...)}
#'   \item{label}{Name of the region}
#' }
#' @source \url{https://ec.europa.eu/eurostat}
"NUTS_2021"
