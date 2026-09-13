#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
#' rcolors <- unique(grDevices::colors())
#' rcolorsrgb <- grDevices::col2rgb(rcolors, alpha = T)
#' rcolorshex <- grDevices::rgb(rcolorsrgb[1, ], rcolorsrgb[2, ], rcolorsrgb[3, ], maxColorValue = 255, alpha = T)
#' out <- order_colors(rcolorsrgb)
#' scales::show_col(rcolors[out], labels = F)
order_colors <- function(x, method = c("TSP", "hclust", "path")) {


  method <- rlang::arg_match(method)

  if (is.character(x)) {
    x <- grDevices::col2rgb(x, alpha = T)
  }
  # handle rgb, hex, colors

  # other methods?

  if (method == "TSP") {
    colrr:::.ensure_package("TSP")
    # from https://github.com/cj-holmes/image-colour-distributions:
    col_order <- x |>
      t() |>
      stats::dist() |>
      TSP::as.TSP() |>
      TSP::solve_TSP(control = list(rep = 1000))
    return(as.numeric(col_order))
  }

  if (method == "hclust") {
    d <- stats::dist(t(x))
    fit <- hclust(d, method = "average")
    return(fit$order)
  }

  if (method == "path") {
    return(order_colors_perceptual(t(x)))
  }


}

order_colors_perceptual <- function(colors) {
  colrr:::.ensure_package("farver")
  n <- nrow(colors)

  if (n < 2L)
    return(colors)

  # sRGB values in the 0–255 range
  #rgb <- t(grDevices::col2rgb(colors))

  rgb <- colors

  # Pairwise perceptual differences (Delta E 2000)
  distances <- farver::compare_colour(
    rgb,
    rgb,
    from_space = "rgb",
    method = "cie2000"
  )

  # Construct a nearest-neighbor path from each possible starting color
  make_path <- function(start) {
    remaining <- setdiff(seq_len(n), start)
    path <- start

    while (length(remaining)) {
      current <- tail(path, 1)
      next_color <- remaining[
        which.min(distances[current, remaining])
      ]

      path <- c(path, next_color)
      remaining <- setdiff(remaining, next_color)
    }

    path
  }

  path_length <- function(path) {
    sum(distances[cbind(
      path[-length(path)],
      path[-1]
    )])
  }

  candidates <- lapply(seq_len(n), make_path)
  scores <- vapply(candidates, path_length, numeric(1))

  best_path <- candidates[[which.min(scores)]]
  return(best_path)
  # colors[best_path]
}

# out <- order_colors(x = rcolorsrgb, method = "path")
# scales::show_col(rcolors[out], labels = F)
