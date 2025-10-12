#' Color palettes
#'
#' Wrapper function around paletteer and two additional color palettes.
#' See paletteer::palettes_c_names and paletteer::palettes_d_names.
#'
#' @param name name of the palette
#' @param n number of colors to return; may not work for every palette
#' @param direction reverse palette with -1
#' @param contrast_filter remove colors if contrast to bg_color is below contrast_ratio_min.
#' @param contrast_ratio_min minimum ration
#' @param bg_color background hex color or r color name
#'
#' @return a color palette as character vector
#' @export
#'
#'
#' @examples
#' col_pal(name = "custom", n = 10)
col_pal <- function(name = NULL,
                    n = NULL,
                    direction = c(1,-1),
                    contrast_filter = F,
                    contrast_ratio_min = 1.5,
                    bg_color = "white") {

  if (!requireNamespace("paletteer", quietly = T)) {
    utils::install.packages("paletteer")
  }

  paletteers <-
    dplyr::bind_rows(dplyr::mutate(paletteer::palettes_c_names, type2 = "continuous"),
                     dplyr::mutate(paletteer::palettes_d_names, type2 = "discrete"))
  paletteers <- dplyr::mutate(paletteers, command = paste0(package, "::", palette))

  if (is.null(name)) {
    message("Select one palette by palette or command. Additional ones are 'custom', 'ggplot', 'hue', 'material'.")
    print(paletteers, n = 50)
    return(paletteers)
  }

  direction <- as.numeric(match.arg(as.character(direction), choices = c("1","-1")))

  if (name %in% c("ggplot", "ggplot2", "hue", "hue_pal", "huepal")) {
    if (is.null(n) || n == 0) {
      n <- 100
    }
    pal_return <- prismatic::color(scales::hue_pal()(n))
    if (direction == -1) {
      pal_return <- rev(pal_return)
    }
  } else if (name == "custom") {
    pal_return <- prismatic::color(c("grey65", "darkgoldenrod1", "cornflowerblue", "forestgreen", "tomato2", "mediumpurple1", "turquoise3", "lightgreen", "navy", "plum1",
                                     "red4", "khaki1", "tan4", "cadetblue1", "olivedrab3", "darkorange2", "burlywood2", "violetred3", "aquamarine3",
                                     "grey30", "lavender", "blueviolet", "grey10", "pink3", "turquoise4", "darkkhaki", "magenta", "blue", "green", "red",
                                     "darkolivegreen", "orchid1", "springgreen", "dodgerblue4", "deepskyblue", "palevioletred4", "gold4", "maroon1", "lightyellow", "greenyellow", "purple4", "yellow"))
    if (!is.null(n)) {
      if (n > length(pal_return)) {
        pal_return <- prismatic::color(scales::hue_pal()(n))
      }
      pal_return <- pal_return[1:n]
    }
    if (direction == -1) {
      pal_return <- rev(pal_return)
    }
  } else if (name == "material") {
    # bg_col: '#273238'
    pal_return <- prismatic::color(c("#CC6666", "#DF935F", "#81A3BE","#B5BD68", "#707880", "#B394BB", "#4F5C69", "#CC0000", "#F1CC37",
                                     "#5787DA", "#25B876", "#5E3582", "#93C5DE", "#B7A7C5", "#2E90B0", "#C5C8C6", "#50C186")) # "#34312F"

    if (!is.null(n)) {
      if (n > length(pal_return)) {
        pal_return <- prismatic::color(scales::hue_pal()(n))
      }
      pal_return <- pal_return[1:n]
    }
    if (direction == -1) {
      pal_return <- rev(pal_return)
    }
  } else {
    pal_select <- ifelse(grepl("::", name),
                         list(dplyr::filter(paletteers, tolower(command) == tolower(name))),
                         list(dplyr::filter(paletteers, tolower(palette) == tolower(name))))[[1]]

  if (nrow(pal_select) == 0) {
    stop("Palette not found.")
  } else if (nrow(pal_select) > 1) {
    # special cases: prefer colorbrewer or viridis pkg by default
    if (nrow(pal_select) == 2 && sum(grepl("RColorBrewer", pal_select$package)) == 1) {
      pal_select <- dplyr::filter(pal_select, package == "RColorBrewer")
    } else if (nrow(pal_select) > 1 && sum(grepl("viridis", pal_select$package)) == 1) {
      pal_select <- dplyr::filter(pal_select, package == "viridis")
    } else  {
      print(pal_select)
      stop("Name is ambiguous. Please specify by command.")
    }
  }

  if (pal_select$type2 == "discrete") {
    type <- "discrete"
    if (is.null(n) || n == 0) {
      n <- pal_select$length
    }
    if (n > pal_select$length) {
      #message("n = ", n, " larger than number of discrete color in palette (", pal_select$length, "). Going to interpolate to provide ", n, " colors.")
      type <- "continuous"
    }
    # what if n>pal_select$length - fall back to hue pal?
    pal_return <- paletteer::paletteer_d(pal_select$command, n = n, type = type, direction = direction)

  } else if (pal_select$type2 == "continuous") {
    if (is.null(n) || n == 0) {
      n <- 100
    }
    pal_return <- paletteer::paletteer_c(pal_select$command, n = n, direction = direction)
  }
}

if (contrast_filter) {

  ratios <- purrr::map_dbl(stats::setNames(pal_return, pal_return), contrast_ratio, hex2 = bg_color)
  ratios <- ratios[which(ratios > contrast_ratio_min)]
  pal_return <- prismatic::color(names(ratios))
}

if (length(pal_return) > 200) {
  invisible(pal_return)
} else {
  return(pal_return)
}

}

if(base::getRversion() >= "2.15.1")  utils::globalVariables(c("package", "palette", "command"))
