#' Find most similar built-in R color
#'
#' @param hex hex color code
#' @param n top n closest colors
#' @param print do print both colors side-by-side?
#'
#' @returns color name
#' @export
#'
#' @examples
#' closest_color("#f030e3")
closest_color <- function(hex, n = 1, print = T) {

  target_rgb <- as.vector(grDevices::col2rgb(hex))
  all_colors <- grDevices::colors()
  color_rgbs <- t(grDevices::col2rgb(all_colors))

  # euclidean distances
  distances <- rowSums(sweep(color_rgbs, 2, target_rgb, "-")^2)

  closest_color <- all_colors[order(distances)[1:n]]
  if (print) {
    scales::show_col(c(hex, closest_color))
  }
  return(closest_color)
}
