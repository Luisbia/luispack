#'Export to excel a data frame
#' @param .data
#'
#' @return an excel file in a temporary folder
#' @export
#'
#' @examples
#'
#' show_in_excel(mtcars)
show_in_excel <- function(.data){
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      "Package \"openxlsx\" must be installed to use this function.",
      call. = FALSE
    )
  }
  tmp <- paste0(tempfile(), ".xlsx")
  openxlsx::write.xlsx(.data,tmp)
  browseURL(tmp)
}
