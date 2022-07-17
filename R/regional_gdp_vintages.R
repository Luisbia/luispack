#' Regional gdp vintages
#'
#' A data frame with the vintage data of regional GDP.
#'
#' @format A data frame:
#' \describe{
#'   \item{Country}{Country code}
#'   \item{NUTS}{NUTS level:0 and 2}
#'   \item{geo}{Code of the region (AT11,BE100,...)}
#'   \item{label}{Label of the region}
#'   \item{vintage}{year the data was received}
#'   \item{unit}{unit}
#'   \item{time}{reference period}
#'   \item{values}{values}
#' }
#' @source
#' df <- rio::import("https://ec.europa.eu/eurostat/documents/24987/9752807/Release+dataset.xlsx/e098dbcd-3af3-f1bd-5c65-bd51c93a0560?t=1641919415365",
#'                  sheet ="data") %>%
#'  tidyr::pivot_longer(cols= starts_with("20"),
#'                      names_to ="time",
#'                      values_to = "values") %>%
#'  na.omit()
#' return (df)
#'
"regional_gdp_vintages"
