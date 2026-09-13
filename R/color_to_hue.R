#' Convert Colors to Approximate Wavelengths
#'
#' Estimates a wavelength in nanometres for each supplied color by converting
#' it to HSV hue and interpolating between approximate hue–wavelength values.
#'
#' @param color A character vector of colors in any format accepted by
#'   [grDevices::col2rgb()], such as named colors or hexadecimal color codes.
#' @param return_hue rather return hue value which give a better ordering
#' of colors
#'
#' @return A numeric vector containing the approximate wavelength, in
#'   nanometres, for each element of `color`.
#'
#' @details
#' Colors are first converted from RGB to HSV. Hue values are then mapped to
#' approximate wavelengths using linear interpolation.
#'
#' This conversion is only a visual approximation. Most RGB colors do not
#' represent a single spectral wavelength, and purple or magenta colors have
#' no corresponding monochromatic wavelength. Saturation and brightness are
#' ignored, so achromatic colors such as white, gray, and black do not have a
#' physically meaningful result.
#'
#' @export
#'
#' @examples
#' color_to_wavelength(c("red", "yellow", "green", "blue"))
#'
#' color_to_wavelength(c("#FF0000", "#00FF00", "#0000FF"))
color_to_wavelength <- function(color, return_hue = F) {
  rgb <- grDevices::col2rgb(color) / 255
  hue <- grDevices::rgb2hsv(rgb)["h", ] * 360

  if (return_hue) {
    return(hue)
  }
  # Approximate hue to wavelength mapping
  stats::approx(
    x = c(0, 60, 120, 180, 240, 270, 300, 360),
    y = c(700, 580, 530, 490, 450, 420, 400, 700),
    xout = hue,
    rule = 2
  )$y
}

#' Identify Whitish Colors
#'
#' Classifies colors as whitish using their CIE Lab lightness and chroma.
#'
#' @param x A character vector of colors in any format accepted by
#'   [grDevices::col2rgb()], such as named colors or hexadecimal color codes.
#' @param min_lightness A numeric scalar giving the minimum CIE Lab lightness
#'   (`L*`) required. Defaults to `92`.
#' @param max_chroma A numeric scalar giving the maximum CIE Lab chroma
#'   (`C*`) allowed. Defaults to `12`.
#' @param return_vals Logical. If `FALSE`, return the classification. If
#'   `TRUE`, return the calculated lightness and chroma values.
#'
#' @return If `return_vals = FALSE`, a logical vector with one value per color.
#'   If `return_vals = TRUE`, a data frame containing `color`, `lightness`,
#'   and `chroma`.
#'
#' @details
#' Colors are converted from sRGB to the CIE Lab color space. Chroma is
#' calculated as:
#'
#' \deqn{C^* = \sqrt{a^{*2} + b^{*2}}}
#'
#' Colors with high lightness and low chroma are classified as whitish.
#' Thresholds can be adjusted to make the classification more or less
#' inclusive.
#'
#' @export
#'
#' @examples
#' is_whitish(c("white", "snow", "ivory", "red", "gray50"))
#'
#' is_whitish(
#'   c("#FFFFFF", "#FFFFF0", "#FF0000"),
#'   return_vals = TRUE
#' )
#' cols <- grep("grey|gray|^white$|black", grDevices::colors(), value = T, invert = T)
#' df <- is_whitish(cols, return_vals = T)
#' library(ggplot2)
#' ggplot(df, aes(x = chroma, y = ligthness)) +
#'   annotate("rect", xmin = -Inf, xmax = 12, ymin = 92, ymax = Inf, fill = "grey70", alpha = 0.25) +
#'   geom_point(aes(fill = color), shape = 21, size = 3, colour = "grey30", stroke = 0.3) +
#'   scale_fill_identity() +
#'   labs(x = "Chroma", y = "Lightness (L*)", title = "Named R colors in lightness–chroma space", subtitle = "Shaded region indicates whitish colors") +
#'   theme_minimal(base_size = 12)
#'
#' df <- df |> dplyr::filter(chroma>25)
#' cols <- df$color
#' hue <- color_to_wavelength(cols, return_hue)
#' scales::show_col(cols[order(hue)], labels = T)
#'
#' order <- order_colors(cols, method = "hclust")
#' scales::show_col(cols[order], labels = T)
#'
#' order <- order_colors(cols, method = "TSP")
#' scales::show_col(cols[order], labels = T)
#'
#' order <- order_colors(cols, method = "path")
#' scales::show_col(cols[order], labels = T)
#'
#' df2 <- group_ordered_colors(cols[order], max_distance = 25)
#' df2 <- cbind(df2, is_whitish(df2$color, return_vals = T)[,-1])
#' ggplot(df2, aes(x = chroma, y = lightness)) +
#'   annotate("rect", xmin = -Inf, xmax = 12, ymin = 92, ymax = Inf, fill = "grey70", alpha = 0.25) +
#'   geom_point(aes(fill = color), shape = 21, size = 3, colour = "grey30", stroke = 0.3) +
#'   scale_fill_identity() +
#'   labs(x = "Chroma", y = "Lightness (L*)", title = "Named R colors in lightness–chroma space", subtitle = "Shaded region indicates whitish colors") +
#'   theme_minimal(base_size = 12) +
#'   facet_wrap(vars(group))
is_whitish <- function(x, min_lightness = 92, max_chroma = 12,
                       return_vals = FALSE) {
  rgb <- t(grDevices::col2rgb(x)) / 255
  lab <- grDevices::convertColor(rgb, from = "sRGB", to = "Lab")

  lightness <- lab[, "L"]
  chroma <- sqrt(lab[, "a"]^2 + lab[, "b"]^2)

  if (return_vals) {
    return(data.frame(
      color = x,
      lightness = lightness,
      chroma = chroma
    ))
  }

  lightness >= min_lightness & chroma <= max_chroma
}

#' Group an Ordered Vector of Similar Colors
#'
#' Divides an ordered vector of colors into contiguous groups based on the
#' perceptual distance between adjacent colors.
#'
#' @param x A character vector of colors in any format accepted by
#'   [grDevices::col2rgb()], including named colors and hexadecimal strings.
#' @param max_distance A non-negative numeric value specifying the maximum
#'   CIE76 distance allowed between adjacent colors in the same group.
#'   Smaller values produce more groups. A value near `10` is a reasonable
#'   starting point.
#'
#' @return A data frame with one row per input color and the following columns:
#'
#' * `position`: Position of the color in `x`.
#' * `color`: Original color value.
#' * `distance_previous`: CIE76 distance from the preceding color, or `NA`
#'   for the first color.
#' * `group`: Integer identifying the contiguous color group.
#'
#' An empty input returns an empty data frame with the same columns.
#'
#' @details
#' Colors are converted from sRGB to CIE Lab. The Euclidean distance between
#' adjacent Lab coordinates, also called CIE76 distance, is then calculated.
#' A new group begins whenever this distance is greater than `max_distance`.
#'
#' Because each color is compared only with its immediate predecessor, a
#' gradual color gradient can form one group even when its first and last
#' colors differ substantially.
#'
#' @export
#'
#' @examples
#' x <- c(
#'   "snow", "ivory", "white",
#'   "pink", "salmon",
#'   "darkred", "blue", "navy"
#' )
#'
#' groups <- group_ordered_colors(x, max_distance = 15)
#' groups
#'
#' split(groups$color, groups$group)
group_ordered_colors <- function(x, max_distance = 25) {
  if (!is.numeric(max_distance) ||
      length(max_distance) != 1L ||
      is.na(max_distance) ||
      max_distance < 0) {
    stop("`max_distance` must be one non-negative number.")
  }

  if (!length(x)) {
    return(data.frame(
      position = integer(),
      color = character(),
      distance_previous = numeric(),
      group = integer()
    ))
  }

  rgb <- t(grDevices::col2rgb(x)) / 255
  lab <- grDevices::convertColor(rgb, from = "sRGB", to = "Lab")

  adjacent_distance <- if (length(x) > 1L) {
    sqrt(rowSums(diff(lab)^2))
  } else {
    numeric()
  }

  data.frame(
    position = seq_along(x),
    color = x,
    distance_previous = c(NA_real_, adjacent_distance),
    group = cumsum(c(TRUE, adjacent_distance > max_distance))
  )
}
