#' Get a dataframe with the latest Eurobase annual GDP
#'
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
  if (!requireNamespace("restatapi", quietly = TRUE)) {
    stop(
      "Package \"restatapi\" must be installed to use this function.",
      call. = FALSE
    )
  }
  dt<-restatapi::get_eurostat_data("nama_10_gdp",
                                   filters=list(na_item=na_item_sel,
                                                unit = unit_sel,
                                   label=FALSE))
  return(dt)}


#' Get a dataframe with the latest Eurobase quarterly GDP
#'
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
#' dt<- get_quarterly_GDP(na_item_sel=c("B1GQ", "D1"), unit_sel= c("CP_MEUR","CP_MNAC"))
get_quarterly_GDP <- function(na_item_sel = "B1GQ",
                                unit_sel ="CLV_PCH_PRE",
                                s_adj_sel="SCA"){

  if (!requireNamespace("restatapi", quietly = TRUE)) {
    stop(
      "Package \"restatapi\" must be installed to use this function.",
      call. = FALSE
    )
  }
  dt<-restatapi::get_eurostat_data("namq_10_gdp",
                                   filters=list(na_item=na_item_sel,
                                                unit = unit_sel,
                                                s_adj = s_adj_sel),
                                   label=FALSE)
  return(dt)}

