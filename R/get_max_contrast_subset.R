#' Get a subset of colors with max contrasts (simple)
#'
#' @param cols color vector
#' @param contrast_matrix optional pre-computed contrast matrix with
#' colrr::contrast_ratio(cols, cols)
#' @param k number of colors to return
#' @param include indices in cols to keep
#' @param seed seed for random start
#'
#' @return subset color vector from cols
#' @export
#'
#' @examples
#' get_max_contrast_subset(colrr::col_pal("material"))
get_max_contrast_subset <- function(cols,
                                    contrast_matrix = NULL,
                                    k = 5,
                                    seed = 42,
                                    include = NULL) {

  if (is.null(contrast_matrix)) {
    contrast_matrix <- contrast_ratio(cols, cols)
  }

  if (is.null(include)) {
    set.seed(seed)
    include <- sample(1:nrow(contrast_matrix), 1)
  }

  while (length(include) < k) {
    candidates <- setdiff(1:nrow(contrast_matrix), include)
    scores <- contrast_matrix[candidates,include,drop=F]
    # Strategy: maximize contrast
    csum <- rowSums(scores)
    nextcand <- candidates[which.max(csum)]

    include <- c(include, nextcand)
  }

  return(prismatic::color(rownames(contrast_matrix)[include]))
}
