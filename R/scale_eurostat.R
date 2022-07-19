
#' Color scale constructor for eurostat colors
#'
#' @param palette Character name of palette in luis_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @export scale_colour_eurostat
scale_colour_eurostat <- function(palette = "new", discrete = TRUE, reverse = FALSE, ...) {
  # list of named colours
  eurostat_colours <- c('EU Blue' = "#0E47CB",
                        'EU Grey 60' = "#7D8088",
                        'Fuchsia' = "#B656BD",
                        'Berry' = "#AF155C",
                        'Teal' = "#208486",
                        'Sienna' = "#AA5F18",
                        'Dark orchid' = "#672DC4",
                        'Sunset red' = "#E04040",
                        'Cornflower' = "#388AE2",
                        'Dark Gold' = "#B39421",
                        'Cobalt' = "#2644A7",
                        'Forest Green' = "#33A033",
                        'EU Grey 100' = "#262B38",
                        'EU Yellow' = "#FFCC00",
                        'theme1' = "#466EB4",
                        'theme2' = "#AF4B91",
                        'theme3' = "#E6A532",
                        'theme4' = "#00A0E1",
                        'theme5' = "#7DAF4B",
                        'theme6' = "#B93C46",
                        'theme7' = "#961E2D",
                        'theme8' = "#41AFAA",
                        'theme9' = "#D7642D")

  #' Function to extract  colors as hex codes
  #'
  #' @param ... Character names of colors
  #'
  eurostat_cols <- function(...) {
    cols <- c(...)

    if (is.null(cols)) {
      return(eurostat_colours)
    }
    eurostat_colours[cols]
  }



  eurostat_palettes <- list("new"=eurostat_cols('EU Blue',
                                                'EU Grey 60',
                                                'Fuchsia' ,
                                                'Berry',
                                                'Teal' ,
                                                'Dark orchid',
                                                'Sienna',
                                                'Sunset red' ,
                                                'Cornflower' ,
                                                'Dark Gold',
                                                'Cobalt',
                                                'Forest Green',
                                                'EU Yellow'),
                            "theme2"=eurostat_cols("theme2",
                                                   "theme6",
                                                   "theme8",
                                                   "theme4",
                                                   "theme7",
                                                   "theme1")
  )

  #' Return function to interpolate a  color palette
  #'
  #' @param palette Character name of palette in luis_palettes
  #' @param reverse Boolean indicating whether the palette should be reversed
  #' @param ... Additional arguments to pass to colorRampPalette()
  #'
  eurostat_pal<- function (palette= "new",
                           reverse = FALSE, ...) {
    pal <- eurostat_palettes[[palette]]
    if (reverse) pal <- rev (pal)
    colorRampPalette(pal, ...)
  }


  pal <- eurostat_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("eurostat_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for eurostat colors
#'
#' @param palette Character name of palette in luis_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @export scale_fill_eurostat

scale_fill_eurostat <- function(palette = "new", discrete = TRUE, reverse = FALSE, ...) {
  # list of named colours
  eurostat_colours <- c('EU Blue' = "#0E47CB",
                        'EU Grey 60' = "#7D8088",
                        'Fuchsia' = "#B656BD",
                        'Berry' = "#AF155C",
                        'Teal' = "#208486",
                        'Sienna' = "#AA5F18",
                        'Dark orchid' = "#672DC4",
                        'Sunset red' = "#E04040",
                        'Cornflower' = "#388AE2",
                        'Dark Gold' = "#B39421",
                        'Cobalt' = "#2644A7",
                        'Forest Green' = "#33A033",
                        'EU Grey 100' = "#262B38",
                        'EU Yellow' = "#FFCC00",
                        'theme1' = "#466EB4",
                        'theme2' = "#AF4B91",
                        'theme3' = "#E6A532",
                        'theme4' = "#00A0E1",
                        'theme5' = "#7DAF4B",
                        'theme6' = "#B93C46",
                        'theme7' = "#961E2D",
                        'theme8' = "#41AFAA",
                        'theme9' = "#D7642D")

  #' Function to extract  colors as hex codes
  #'
  #' @param ... Character names of colors
  #'
  eurostat_cols <- function(...) {
    cols <- c(...)

    if (is.null(cols)) {
      return(eurostat_colours)
    }
    eurostat_colours[cols]
  }



  eurostat_palettes <- list("new"=eurostat_cols('EU Blue',
                                                'EU Grey 60',
                                                'Fuchsia' ,
                                                'Berry',
                                                'Teal' ,
                                                'Dark orchid',
                                                'Sienna',
                                                'Sunset red' ,
                                                'Cornflower' ,
                                                'Dark Gold',
                                                'Cobalt',
                                                'Forest Green',
                                                'EU Yellow'),
                            "theme2"=eurostat_cols("theme2",
                                                   "theme6",
                                                   "theme8",
                                                   "theme4",
                                                   "theme7",
                                                   "theme1")
  )

  #' Return function to interpolate a  color palette
  #'
  #' @param palette Character name of palette in luis_palettes
  #' @param reverse Boolean indicating whether the palette should be reversed
  #' @param ... Additional arguments to pass to colorRampPalette()
  #'
  eurostat_pal<- function (palette= "new",
                           reverse = FALSE, ...) {
    pal <- eurostat_palettes[[palette]]
    if (reverse) pal <- rev (pal)
    colorRampPalette(pal, ...)
  }

  pal <- eurostat_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("eurostat_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
