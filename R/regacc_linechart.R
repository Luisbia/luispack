library(dplyr)
library(ggplot2)

regacc_linechart <- function(.data,
                             hor,
                             ver,
                             grp,
                             clr,
                             leg = TRUE,
                             fac) {
  hor <- enquo(hor)
  ver <- enquo(ver)
  grp <- enquo(grp)
  clr <- enquo(clr)

  plot <- ggplot(.data) +
    geom_line(aes(!!hor, !!ver, group = !!grp, colour = !!clr))+
    theme_light()

  if (leg == FALSE) {
    plot <- plot + theme(legend.position = "none")
  }
  if (leg == TRUE)  {
    plot <- plot + theme(legend.position = "bottom")
  }
  # facet
  if(!missing(fac)){
    plot <- plot + facet_wrap(vars({{fac}}))
  }

  return(plot)
}


