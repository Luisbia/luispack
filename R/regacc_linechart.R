library(dplyr)
library(ggplot2)

regacc_linechart <- function(dat,
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

  theme_linechart <- function(..., base_size = 14, base_family="mono") {
    theme(
      panel.grid.minor = element_blank(),   ## drop minor gridlines
      panel.grid.major.y =  element_line(color = "#7D8088", linetype = 2,),# change horizontalgrid lines to gray
      panel.grid.major.x =  element_blank(),     # remove vertical gridlines
      panel.background = element_rect(fill = "#FAFAFA", color = "#262B38"),      # fill the plot and panel spaces with grey and remove border
      plot.background = element_rect(fill = "#FAFAFA", color = "#262B38"),
      panel.border = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text( colour ="#262B38",size=rel(1.0)),
      legend.key = element_rect(fill= "white",colour = NA),
      legend.key.size = unit(1, 'cm'),
      legend.background = element_rect(fill="white", colour = NA),
      plot.margin = margin(1, 1, 1, 1, unit = "cm"), # set margins
      axis.ticks = element_line(size=rel(1.2)), #size axis ticks
      axis.text = element_text(size = rel(1.0)),
      axis.title = element_blank(), # remove axis titles
      plot.title = element_text (size=rel(1.3)),
      plot.caption = element_text(size = rel(1.1),hjust = 0.05, vjust = 0, face="italic"),
      plot.caption.position = "plot",
      strip.background = element_rect(fill = "#0E47CB"),
      strip.text = element_text(size=rel(1.2), face= "bold", colour ="#FFCC00"),
      ...)}

  plot <- ggplot(dat) +
    geom_line(aes(!!hor, !!ver, group = !!grp, colour = !!clr),size=0.8)+
    theme_linechart()+
    scale_x_continuous(labels = scales::label_number(), breaks = scales::breaks_pretty(n=3))+
    scale_y_continuous(labels = scales::label_number(), breaks = scales::breaks_pretty(n=4))

  if (leg == FALSE) {
    plot <- plot + theme(legend.position = "none")
  }
  if (leg == TRUE)  {
    plot <- plot + theme(legend.position = "bottom")
  }
  # facet
  if(!missing(fac)){
    plot <- plot + facet_wrap(vars({{fac}}),scales="free_y")
  }

  return(plot)
}

# df<- NQR_data %>%
#   filter(Country =="AT" & vintage =="2021" & NUTS =="2")
#
#
# regacc_linechart(dat=df,
#                  hor=time,
#                  ver=values,
#                  grp=geo,
#                  clr=geo,
#                  leg=TRUE,
#                  fac=na_item)
