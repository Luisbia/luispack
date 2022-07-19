#' Create standard line charts
#'
#'This functions makes easy to create standard line charts for different purposes
#' @param dat the data frame with the data
#' @param hor the name of the variable in the horizontal axis
#' @param ver the name of the variable in the vertical axis
#' @param grp the variable to use for grouping the data
#' @param clr the variable mapped to the colour aesthetic
#' @param leg set to FALSE if you do not want to show the legend
#' @param fac the (unique) variable to use if small multiples are wanted
#'
#' @return a ggplot
#' @export regacc_linechart
#'
#' @examples
#' df<- NQR_data %>%
#' filter(Country =="AT" & vintage =="2021" & NUTS =="2") %>%
#'   mutate(time=as.integer(time))
#'
#' regacc_linechart(dat=df,
#'                  hor=time,
#'                  ver=values,
#'                  grp=geo,
#'                  clr=geo,
#'                  leg=TRUE,
#'                  fac=na_item)
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

  check_packages()
  theme_linechart <- function(..., base_size = 14, base_family="mono") {
    theme(
      panel.grid.minor = element_blank(),   ## drop minor gridlines
      panel.grid.major.y =  element_line(color = "#7D8088", linetype = 2,),# change horizontalgrid lines to gray
      panel.grid.major.x =  element_blank(),     # remove vertical gridlines
      panel.background = element_rect(fill = "#FFFFFF", color = "#262B38"),      # fill the plot and panel spaces with grey and remove border
      plot.background = element_rect(fill = "#FFFFFF", color = "#262B38"),
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
      strip.text.x = element_text(  size = rel (1.3), face = "bold", hjust = 0.1, colour = "white" ), #margin = margin()
      strip.text.y = element_text(size = rel (1.3),hjust = 0.1, face= "bold", colour = "white"),
      strip.placement = "outside",
      panel.spacing = unit(0.8, "lines"),
      ...)}

  plot <- ggplot(dat) +
    geom_line(aes(!!hor, !!ver, group = !!grp, colour = !!clr),size=0.8)+
    theme_linechart()+
    scale_x_continuous(labels = scales::label_number(accuracy = 1),
                       breaks = scales::breaks_pretty(n=3),
                       expand = c(0,0))+
    scale_y_continuous(labels = scales::label_number(),
                       breaks = scales::breaks_pretty(n=4))

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


