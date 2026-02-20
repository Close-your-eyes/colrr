#' Wavelength to hex color value
#'
#' From this old repo https://github.com/ucl-cssb/wavelength2colour to chatgpt
#' to here. Can it be improved?
#'
#' @param wavelength wl in 380 nm to 780 nm
#' @param gamma delta
#'
#' @returns hex character
#' @export
#'
#' @examples
#' scales::show_col(wl_to_hex(seq(380,780,2)))
wl_to_hex <- function(wavelength, gamma = 0.8) {

  if (is.null(wavelength)) {
    wavelength <- purrr::set_names(wavelength)
  }
  hexcol <- purrr::map_chr(wavelength, function(x) {
    if (x < 380 || x > 780) {
      return(grDevices::rgb(0, 0, 0))
    }

    if (x >= 380 && x < 440) {
      R <- -(x - 440) / (440 - 380)
      G <- 0
      B <- 1
    } else if (x < 490) {
      R <- 0
      G <- (x - 440) / (490 - 440)
      B <- 1
    } else if (x < 510) {
      R <- 0
      G <- 1
      B <- -(x - 510) / (510 - 490)
    } else if (x < 580) {
      R <- (x - 510) / (580 - 510)
      G <- 1
      B <- 0
    } else if (x < 645) {
      R <- 1
      G <- -(x - 645) / (645 - 580)
      B <- 0
    } else {
      R <- 1
      G <- 0
      B <- 0
    }

    # Intensity correction near vision limits
    if (x >= 380 && x < 420) {
      factor <- 0.3 + 0.7 * (x - 380) / (420 - 380)
    } else if (x < 645) {
      factor <- 1
    } else {
      factor <- 0.3 + 0.7 * (780 - x) / (780 - 645)
    }

    R <- (R * factor)^gamma
    G <- (G * factor)^gamma
    B <- (B * factor)^gamma

    grDevices::rgb(R, G, B)
  })
  return(hexcol)
}

