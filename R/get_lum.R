#' Calculate the perceptual lightness of a color
#'
#' Converts an sRGB color to its CIE L* perceptual lightness value. The result
#' ranges approximately from 0 for black to 100 for white.
#'
#' @param col Either a single color specified by an R color name or hexadecimal
#'   string, or a numeric vector of length three containing red, green, and blue
#'   channel values on a scale from 0 to 255.
#'
#' @returns A numeric scalar containing the color's CIE L* lightness.
#'
#' @references
#' The conversion follows the sRGB relative-luminance formula described in
#' [W3C Web Content Accessibility Guidelines](https://www.w3.org/TR/WCAG21/#dfn-relative-luminance).
#'
#' @export
#'
#' @examples
#' get_lum("black")
#' get_lum("white")
#' get_lum("#336699")
#' get_lum(c(51, 102, 153))
get_lum <- function(col) {
  #https://stackoverflow.com/questions/596216/formula-to-determine-perceived-brightness-of-rgb-color

  # list vs vector?!
  if (length(col) == 1 && methods::is(col, "character")) {
    col <- unname(grDevices::col2rgb(col)[,1])
  }
  if (!is.numeric(col) || length(col) != 3) {
    stop("col has to be one character (hex code or name of a color), or a numeric of length 3 (rgb).")
  }

  col = col / 255
  col = .RGBtoLin(col)
  Y = (0.2126 * col[1] + 0.7152 * col[2] + 0.0722 * col[3])
  Ls <- .YtoLstar(Y)
  return(Ls)
}

.RGBtoLin <- function(cc) {
  sapply(cc, function(x) {
    if (x <= 0.04045) {
      x / 12.92
    } else {
      ((x + 0.055)/1.055)^2.4
    }
  })
}

.YtoLstar <- function(Y) {
  sapply(Y, function(x) {
    if (x <= (216/24389)) {
      x * (24389/27)
    } else {
      x^(1/3) * 116 - 16
    }
  })
}
