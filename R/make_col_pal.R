#' Algorithm to adjust color palette to factor levels which are to be colored
#'
#' Heuristic to handle shorter, longer, named, unnamed col_vec in comparison to
#' fct_lvls.
#'
#' @param col_vec named or unnamed, complete or incomplete vector of colors;
#' names should correspond to fct_lvls, can be an R color name, a hex color
#' starting with '#' or a vector of these
#' @param fct_lvls factor levels to color, can be a character; NULL for
#' continuous scale w/o factor levels to color
#' @param col_pal_args arguments to colrr::col_pal but not n
#' @param adjust_to_fct_lvls adjust missing or wrong names of col_pal?
#' @param na_color
#' @param missing_fct_to_na
#'
#' @returns named color vector
#' @export
#'
#' @examples
#' exdata <- readRDS(system.file("extdata", "make_col_pal_example.rds", package = "colrr"))
#' ex_col <- exdata[[1]]
#' fct_lvls <- exdata[[2]]
#' # exact col_vec
#' make_col_pal(col_vec = ex_col[unique(fct_lvls)], fct_lvls = unique(fct_lvls))
#' # longer col_vec
#' make_col_pal(col_vec = ex_col, fct_lvls = unique(fct_lvls))
#' # dup fct_lvls
#' make_col_pal(col_vec = ex_col, fct_lvls = fct_lvls)
#' # longer col_vec unnamed
#' make_col_pal(col_vec = unname(ex_col), fct_lvls = unique(fct_lvls))
#' # shorter col_vec
#' make_col_pal(col_vec = ex_col[1:5], fct_lvls = unique(fct_lvls))
#' make_col_pal(col_vec = unname(ex_col[1:5]), fct_lvls = unique(fct_lvls))
#' # with adjust - hue pal
#' make_col_pal(col_vec = ex_col[1:5], fct_lvls = unique(fct_lvls), adjust_to_fct_lvls = T)
#' # other order
#' make_col_pal(col_vec = ex_col[1:5], fct_lvls = sort(unique(fct_lvls)), adjust_to_fct_lvls = T)
#' # only "" names (= no matching name)
#' make_col_pal(col_vec = stats::setNames(ex_col, rep("", length(ex_col))), fct_lvls = unique(fct_lvls), adjust_to_fct_lvls = F)
#' make_col_pal(col_vec = stats::setNames(ex_col, rep("", length(ex_col))), fct_lvls = unique(fct_lvls), adjust_to_fct_lvls = T)
#' # some "" names
#' ex_col2 <- ex_col
#' names(ex_col2)[c(3,5,7,9)] <- ""
#' make_col_pal(col_vec = ex_col2, fct_lvls = unique(fct_lvls), adjust_to_fct_lvls = T)
#' make_col_pal(col_vec = ex_col2, fct_lvls = unique(fct_lvls), adjust_to_fct_lvls = F)
#' # some wrong names
#' names(ex_col2)[c(3,5,7,9)] <- c(letters[1:4])
#' make_col_pal(col_vec = ex_col2, fct_lvls = unique(fct_lvls), adjust_to_fct_lvls = T)
#' make_col_pal(col_vec = ex_col2, fct_lvls = unique(fct_lvls), adjust_to_fct_lvls = F)
#' # single r color
#' make_col_pal(col_vec = "grey90", fct_lvls = unique(fct_lvls), adjust_to_fct_lvls = F)
#' make_col_pal(col_vec = "grey90", fct_lvls = unique(fct_lvls), adjust_to_fct_lvls = T)
#' # single hex color
#' make_col_pal(col_vec = "#FFB90FFF", fct_lvls = unique(fct_lvls), adjust_to_fct_lvls = F)
#' make_col_pal(col_vec = "#FFB90FFF", fct_lvls = unique(fct_lvls), adjust_to_fct_lvls = T)
#'
#' # then use:
#' # ggplot2::scale_color_manual(values = col.pal,
#' #                             na.value = col_na)
make_col_pal <- function(col_vec,
                         fct_lvls = NULL,
                         col_pal_args = list(),
                         adjust_to_fct_lvls = F,
                         na_color = "grey50",
                         missing_fct_to_na = T) {

  # with adjust_to_fct_lvls = F and later
  # ggplot2::scale_color_manual(values = col.pal,
  #                             na.value = col_na)
  # highlighting is easily possible

  if (!is.null(fct_lvls) && anyDuplicated(as.character(fct_lvls))) {
    message("make_col_pal: duplicate fct_lvls. they are made unique.")
  }
  fct_lvls <- unique(fct_lvls) # as.character? what about numeric? what about factor?

  if (length(col_vec) == 1 && (col_vec %in% grDevices::colors() || grepl("^#", col_vec))) {
    # col_vec is r color name or hex color
    col_pal <- col_vec
  } else if (length(col_vec) == 1) {
    # col_vec is palette name from colrr::col_pal
    col_pal <- do.call(what = colrr::col_pal, args = c(list(name = col_vec, n = length(fct_lvls)), col_pal_args))
  } else {
    # col_vec is a vector of colors already
    # could go with if clause but anyway
    col_pal <- col_vec
  }

  if (is.null(fct_lvls)) {
    # continuous scale: no factor levels
    # early exit
    return(col_pal)
  }

  col_pal <- stats::setNames(as.character(col_pal), names(col_pal)) # keep all "" names

  if (length(col_pal) > length(fct_lvls)) {
    # cut col_pal if too long
    # when fct_lvls too long: handled below
    if (!is.null(names(col_pal))) {
      col_pal <- col_pal[fct_lvls]
      # complicated, could this cause error - yes.
      # col_pal_temp <- col_pal[fct_lvls[which(fct_lvls %in% names(col_pal))]]
      # col_pal <- c(col_pal_temp, col_pal[which(!names(col_pal) %in% fct_lvls)][1:(length(fct_lvls)-length(col_pal_temp))])
    } else {
      col_pal <- col_pal[seq_along(fct_lvls)]
    }
  }

  if (is.null(names(col_pal))) {
    # react to different lengths
    # always named
    names(col_pal) <- fct_lvls[seq_along(col_pal)]
  } else {
    # repl NA
    names(col_pal)[which(is.na(names(col_pal)))] <- ""
    # repl wrong names
    names(col_pal)[which(!names(col_pal) %in% fct_lvls)] <- ""
    if (!length(intersect(names(col_pal), fct_lvls))) {
      stop("col_pal: none of names matches fct_lvls.")
    }
    # fill "" in names
    names(col_pal)[which(names(col_pal) == "")] <- fct_lvls[which(!fct_lvls %in% names(col_pal))][seq_along(which(names(col_pal) == ""))]
  }

  # col_pal can now only be too short but not too long, hopefully
  if (length(col_pal) > length(fct_lvls)) {
    stop("col_pal longer than fct_lvls. check make_col_pal function.")
  }
  # and should have (valid) names by now
  if (any(!names(col_pal) %in% fct_lvls)) {
    stop("some col_pal names not in fct_lvls. check make_col_pal function.")
  }

  if (adjust_to_fct_lvls) {

    if (length(col_pal) < length(fct_lvls)) {
      col_pal <- scales::hue_pal()(length(fct_lvls))
      names(col_pal) <- fct_lvls
      message("make_col_pal: length(col_pal) < length(fct_lvls). either wrong length or missing/wrong names compared to fct_lvls. set adjust_to_fct_lvls = F to ignore this or change provided colors. Falling back to scales::hue_pal().")
    } else {
      if (any(!names(col_pal) %in% fct_lvls)) {
        col_pal <- scales::hue_pal()(length(fct_lvls))
        names(col_pal) <- fct_lvls
        message("make_col_pal: Not all names of col_pal found in fct_lvls. set adjust_to_fct_lvls = F to ignore this or change provided colors. Falling back to scales::hue_pal().")
      }
    }

  } else {

    # add NA for missing colors - should this be optional?
    if (missing_fct_to_na) {
      if (length(col_pal) < length(fct_lvls)) {
        nmiss <- length(fct_lvls) - length(col_pal)
        col_pal <- c(col_pal, stats::setNames(rep(NA, nmiss), nm = fct_lvls[which(!fct_lvls %in% names(col_pal))]))
      }
    }
  }

  # adjust order - no
  #col_pal <- col_pal[fct_lvls]
  col_pal[which(is.na(col_pal))] <- na_color
  return(col_pal)
}
