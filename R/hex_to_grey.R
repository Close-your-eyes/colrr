#' Hey color to grey scale
#'
#' @param hex hex color vector
#' @param out output
#' @param bw_thresh threshold for black/white
#' @param grey_algo algorithm for greyscale
#'
#' @returns
#' @export
#'
#' @examples
hex_to_grey <- function(hex,
                        out = c("grey", "bw"),
                        bw_thresh = 0.5,
                        grey_algo = c(3,2,1)) {

  out <- rlang::arg_match(out)
  grey_algo <- as.character(grey_algo)
  grey_algo <- rlang::arg_match(grey_algo, values = c("3", "2", "1"))

  if (out == "bw") {
    rgb <- grDevices::col2rgb(hex) / 255

    # perceived luminance (ITU-R BT.709)
    luminance <- 0.2126 * rgb[1, ] +
      0.7152 * rgb[2, ] +
      0.0722 * rgb[3, ]

    out_col <- ifelse(luminance > threshold, "#000000", "#FFFFFF")
  }

  if (out == "grey") {
    if (grey_algo == 1) {
      out_col <- grDevices::gray(grDevices::col2gray(cols))
    } else if (grey_algo == 2) {
      rgb <- grDevices::col2rgb(hex) / 255
      # perceptual luminance
      lum <- 0.2126 * rgb[1, ] +
        0.7152 * rgb[2, ] +
        0.0722 * rgb[3, ]

      out_col <- grDevices::rgb(lum, lum, lum)
    } else if (grey_algo == 3) {
      # Convert to RGB
      rgb <- farver::decode_colour(hex, to = "rgb")
      # Compute grayscale
      rgb <- 0.299 * rgb[,1] + 0.587 * rgb[,2] + 0.114 * rgb[,3]
      # Rebuild grayscale colors
      out_col <- farver::encode_colour(cbind(rgb, rgb, rgb), from = "rgb")
    }
  }
   return(out_col)
}


