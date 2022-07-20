#' Create standard scatter charts
#'
#'This functions makes easy to create standard line charts for different purposes
#' @param dat the data frame with the data
#' @param hor the name of the variable in the horizontal axis
#' @param ver the name of the variable in the vertical axis
#' @param clr the variable mapped to the colour aesthetic
#' @param leg set to FALSE if you do not want to show the legend
#' @param fac the (unique) variable to use if small multiples are wanted
#' @param fsz font size
#' @param ffa font family ("mono","sans", "serif) should be available in any EC laptop
#' @param psz point size
#' @param hbr parameter for the number of horizontal breaks, input for scales::break_pretty
#' @param vbr parameter for the number of vertical breaks, input for scales::break_pretty
#' @param acc1 number of decimals for X axis (0.1 means one decimal)
#' @param acc2 number of decimals for Y axis (0.1 means one decimal)
#'
#' @return a ggplot
#' @export regacc_scatter
#'
#' @examples
#' df<- NQR_data %>%
#'  filter(Country =="AT" & vintage =="2021" & na_item %in% c("B1G","B6N")) %>%
#'  pivot_wider(names_from = na_item,
#'              values_from = values)
#'
#'regacc_scatter(dat=df,
#'               hor=B1G,
#'               ver=B6N,
#'               grp=geo,
#'               clr=geo,
#'               leg = FALSE,
#'               fac = geo,
#'               psz = 2)
regacc_scatter <- function(dat,
                             hor,
                             ver,
                             grp,
                             clr,
                             leg = TRUE,
                             fac,
                             fsz = 14,
                             ffa = "mono",
                             psz = 0.8,
                             hbr = 3,
                             vbr = 4,
                             acc1 = 1,
                             acc2 = 1) {
  hor <- enquo(hor)
  ver <- enquo(ver)
  grp <- enquo(grp)
  clr <- enquo(clr)

  check_packages()

  scale_colour_eurostat <- function(){
    eurostat_cols<- c("#0E47CB", # 'EU Blue' =
                      "#FFCC00", #'EU Yellow' =
                      "#7D8088", # 'EU Grey 60' =
                      "#AA5F18",#'Sienna' =
                      "#B656BD", #'Fuchsia' =
                      "#AF155C", #'Berry' = ,
                      "#208486", #'Teal' = ,
                      "#672DC4",#'Dark orchid' =
                      "#E04040",#'Sunset red' =
                      "#388AE2",#'Cornflower' =
                      "#B39421",#'Dark Gold' =
                      "#2644A7",#'Cobalt' =
                      "#33A033",#'Forest Green' =
                      "#262B38") #'EU Grey 100' =

    ggplot2::scale_colour_manual(values = eurostat_cols)
  }

  theme_scatterchart <- function(..., base_size = fsz, base_family = ffa) {
    theme(plot.background = element_rect(fill= "#FFFFFF",
                                         colour = "#262B38"),
          line = element_line(colour = "#262B38"),
          rect = element_rect(fill = "#FAFAFA",
                              linetype = 0,
                              colour = NA),
          panel.background = element_rect(fill= "#FAFAFA"),
      panel.grid.minor = element_blank(),   ## drop minor gridlines
      panel.grid.major.y =  element_line(color = "#7D8088", linetype = 2,),# change horizontalgrid lines to gray
      panel.grid.major.x =  element_line(color = "#7D8088", linetype = 2,),
      panel.border = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text( colour ="#262B38",size=rel(1.0)),
      legend.key = element_rect(fill= "white",colour = NA),
      legend.key.size = unit(1, 'cm'),
      legend.background = element_rect(fill="white", colour = NA),
      plot.margin = margin(1, 1, 1, 1, unit = "cm"), # set margins
      axis.ticks = element_line(size=rel(1.2)), #size axis ticks
      axis.text = element_text(size = rel(1.0)),
      axis.title = element_text(face="bold"), # remove axis titles
      plot.title = element_text (size=rel(1.3)),
      plot.caption.position = "plot",
      plot.caption = element_text(size = rel(1.1),hjust = 0.05, vjust = 0, face="italic"),
      strip.background = element_rect(fill = "#0E47CB"),
      strip.text.x = element_text(  size = rel (1.3), face = "bold", hjust = 0.1, colour = "white" ), #margin = margin()
      strip.text.y = element_text(size = rel (1.3),hjust = 0.1, face= "bold", colour = "white"),
      strip.placement = "outside",
      panel.spacing = unit(0.8, "lines"),

      ...)}

  plot <- ggplot(dat) +
    geom_point(aes(!!hor, !!ver, colour = !!clr),size = psz)+
    theme_scatterchart()+
    scale_x_continuous(labels = scales::label_number(accuracy = acc1),
                       breaks = scales::breaks_pretty(n=hbr))+
    scale_y_continuous(labels = scales::label_number(accuracy = acc2),
                       breaks = scales::breaks_pretty(n=vbr))+

    scale_colour_eurostat()+
    labs(caption = "Source: Eurostat")

  if (leg == FALSE) {
    plot <- plot + theme(legend.position = "none")
  }
  if (leg == TRUE)  {
    plot <- plot + theme(legend.position = "bottom")
  }
  # facet
  if(!missing(fac)){
    plot <- plot + facet_wrap(vars({{fac}}),scales="free")
  }

  return(plot)
}

