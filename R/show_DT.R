#' Create an HTML interactive table with DT from a data frame
#'
#' @param x a data frame
#'
#' @return DT table
#' @export
#'
#' @examples
#'
#' show_DT(NUTS_2021)
#'
show_DT <- function(x){
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop(
      "Package \"DT\" must be installed to use this function.",
      call. = FALSE
    )
  }
  DT::datatable(x, filter = "top", class = "stripe hover", extensions = "Buttons",
                options = list(  lengthMenu = list(c(20, -1), c("20", "All")),
                                 pageLength = 20, dom = "Blfrtip", buttons = c("excel"))
  )}
