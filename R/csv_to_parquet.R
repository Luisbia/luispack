#' A function to convert csv files to parquet files
#'
#' @param dir
#'
#' @return csv files are read with data.table and converted to parquet with arrow. The filenames are the same but replacing csv with parquet
#' @export
#'
#' @examples
#'
#' csv_to_parquet("where_my_csv_are_stored")
csv_to_parquet<- function(dir){
  list_csv <- list.files (path = dir,
                          pattern = glob2rx("*csv$"),
                          full.names = TRUE)

  csv_to_parquet <- function(file) {
    data <- data.table::fread (file)
    arrow::write_parquet(data,stringr::str_replace(paste0(file),".csv$",".parquet" ) )
  }

  purrr::walk(list_csv, csv_to_parquet)
}
