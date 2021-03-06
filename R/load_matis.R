#' Load Matis
#' @description
#' Load excel extractions from REGACC for A_ tables (VAL.T.T1001.A.AT.W2.S1.S1.B.B1G._T.B.V.N.XDC) extracted horizontally and select the columns to keep.
#' It requires `{rio}`, `{tidyverse}` and `{data.table}`.
#'
#' By default it keeps the most useful columns. To keep all columns use: keep_cols= c("VAL","type", "table_identifier", "freq", "ref_area",
#' "counterpart_area","ref_sector","counterpart_sector","accounting_entry", "sto", "activity", "valuation", "prices",
#' "transformation","unit_measure", "time","values","flag) or any subset.
#' @param file The file to load.
#' @param keep_cols The columns to keep.
#'
#' @return a data frame/ data.table
#' @export load_matis
#'
#' @examples
#' df<-load_matis("E:/03_Regional Accounts/03D_Data Production/2021/BE/matis/t1200_BE_01.xlsx",keep_cols= c("type","ref_area","sto","activity","time","values"))
#'
#'
load_matis <- function(file,
                       keep_cols) {
  options(warn = - 1)

  luispack::check_packages()


  df <-rio::import(file) %>%
    pivot_longer(cols= !starts_with("ANNUAL"),
                 names_to = "time",
                 values_to = "values") %>%
    as.data.table

  df<- df[, c("VAL","type", "table_identifier", "freq", "ref_area","counterpart_area","ref_sector","counterpart_sector",
              "accounting_entry", "sto", "activity", "valuation", "prices","transformation","unit_measure") := tstrsplit(ANNUAL, ".", fixed=TRUE)] %>%
    .[,c("values","flag"):= tstrsplit(values, "#")] %>%
    .[,ANNUAL:= NULL] %>%
    .[,VAL:=NULL] %>%
    .[values!="ND",] %>%
    .[,time := as.integer(time)]

  if(missing(keep_cols)){
    keep_cols= c("type","table_identifier", "ref_area","accounting_entry","sto","activity","unit_measure","time","values","flag")
  }

  df<- df %>%
    select(all_of(keep_cols))

  return(df)

  options(warn = 0)
}


