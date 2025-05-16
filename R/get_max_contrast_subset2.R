#' Get a subset of colors with max contrasts
#'
#' @param cols color vector
#' @param contrast_matrix optional pre-computed contrast matrix with
#' colrr::contrast_ratio(cols, cols)
#' @param k number of colors to return
#' @param include indices in cols to keep
#' @param exclude indices in cols to remove
#' @param topn number of top results to return
#'
#' @return subset color vector from cols
#' @export
#'
#' @examples
#' get_max_contrast_subset2(colrr::col_pal("material"))
get_max_contrast_subset2 <- function(cols,
                                     contrast_matrix = NULL,
                                     k = 5,
                                     include = NULL,
                                     exclude = NULL,
                                     topn = 1) {

  if (is.null(contrast_matrix)) {
    contrast_matrix <- contrast_ratio(cols, cols)
  }

  g <- igraph::graph_from_adjacency_matrix(contrast_matrix, mode="undirected", weighted=TRUE, diag=FALSE)

  # total contrast of a subset of nodes
  subset_contrast <- function(nodes, graph) {
    sg <- igraph::induced_subgraph(graph, vids = nodes)
    sum(igraph::E(sg)$weight)
  }

  # Brute force: all combinations of k nodes
  all_combinations <- utils::combn(igraph::V(g), k, simplify = F)
  if (!is.null(include)) {
    all_combinations <- all_combinations[purrr::map_lgl(all_combinations, ~all(include %in% .x))]
  }
  if (!is.null(exclude)) {
    all_combinations <- all_combinations[purrr::map_lgl(all_combinations, ~all(!exclude %in% .x))]
  }

  # Calculate contrast for each combo
  contrast_scores <- sapply(all_combinations, subset_contrast, graph = g)

  # Get the best subset
  best_indices <- order(contrast_scores, decreasing = T)
  best_index <- which.max(contrast_scores)
  best_nodes <- all_combinations[[best_index]]

  if (topn > 1) {
    return(purrr::map(all_combinations[best_indices[1:topn]], ~prismatic::color(names(.x))))
  } else {
    return(prismatic::color(names(best_nodes)))
  }
}
