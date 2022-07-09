#' A function to use Eurostat's Theme 2 colour palette in ggplot2
#'
#' @return
#' @export
#'
#' @examples
#'ggplot(
  #'  mtcars,
#'  aes(mpg, wt, fill = factor(cyl))
#'  scale_fill_theme2()
#'
scale_fill_theme2 <- function () {
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
