#' Find most similar built-in R color
#'
#' @param hex hex color code
#' @param n top n closest colors
#' @param print do print colors side-by-side?
#'
#' @returns color name
#' @export
#'
#' @examples
#' closest_color(hex =  rainbow(10))
closest_color <- function(hex, n = 1, print = T) {

  # target_rgb <- as.vector(grDevices::col2rgb(hex))
  # all_colors <- grDevices::colors()
  # color_rgbs <- t(grDevices::col2rgb(all_colors))
  # distances <- rowSums(sweep(color_rgbs, 2, target_rgb, "-")^2) # euclidean distances
  # closest_color <- all_colors[order(distances)[1:n]]


  mat <- farver::compare_colour(from = farver::decode_colour(hex),
                                to = farver::decode_colour(grDevices::colors()),
                                from_space = "rgb", method = "cie2000")
  if (n == 1) {
    close_col <- grDevices::colors()[apply(mat, 1, which.min)]
    if (print) {
      scales::show_col(c(hex, close_col))
    }
  } else {
    close_col <- apply(mat, 1, function(x) {
      grDevices::colors()[order(x, decreasing = T)[1:n]]
      }, simplify = F)
  }

  return(close_col)
}
