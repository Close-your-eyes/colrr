rgb_to_linear_rgb <- function(rgb) {
  linear_rgb <- ifelse(rgb <= 0.03928,
                       rgb / 12.92,
                       ((rgb + 0.055) / 1.055) ^ 2.4)

  if (!is.matrix(linear_rgb)) {
    linear_rgb <- matrix(linear_rgb, nrow = 3)
    rownames(linear_rgb) <- c("red", "green", "blue")
  }
  return(linear_rgb)
}


hex_to_linear_rgb <- function(hex) {
  rgb <- grDevices::col2rgb(hex) / 255
  linear_rgb <- rgb_to_linear_rgb(rgb)
  return(linear_rgb)
}


relative_luminance <- function(hex, rgb) {

  if (!missing(hex) && !missing(rgb)) {
    message("using hex.")
  }
  if (missing(hex) && missing(rgb)) {
    stop("hex and rgb missing.")
  }

  if (!missing(hex)) {
    linear_rgb <- hex_to_linear_rgb(hex)
  } else if (!missing(rgb)) {
    linear_rgb <- rgb_to_linear_rgb(rgb)
  }

  luminance_calc <- function(x) {
    0.2126 * x[1] +
      0.7152 * x[2] +
      0.0722 * x[3]
  }
  luminance <- apply(linear_rgb, 2, luminance_calc)
  return(luminance)
}


contrast_ratio <- function(hex1, hex2) {
  # hex1 becomes rows
  L1 <- relative_luminance(hex1)
  L2 <- relative_luminance(hex2)
  # with single vals
  # if (L1 < L2) { temp <- L1; L1 <- L2; L2 <- temp } value swap
  # ratio <- (L1 + 0.05) / (L2 + 0.05)
  ratio <- matrix(unlist(lapply(L2, function(L22) (pmax(L1, L22) + 0.05) / (pmin(L1, L22) + 0.05))),
                  nrow = length(L1), byrow = F)

  rownames(ratio) <- as.character(hex1)
  colnames(ratio) <- as.character(hex2)

  return(ratio)
}
