#' Make a continuous fill/color scale for ggplot
#'
#' Make nice default color steps.
#'
#' @param values vector of numeric values
#' @param zscored are values z-scored? if NULL, evaluated within function
#' @param colorsteps NULL for colorbar, a number for number of steps or ..auto..
#' for decision based on zscored etc; when colorsteps_nice = T it influences number
#' of steps
#' @param legendbreaks vector of breaks
#' @param legendlabels vector of labels
#' @param colors to ggplot2::scale_fill/color_gradientn or
#' ggplot2::scale_fill/color_stepsn
#' @param colorsteps_nice to nice.breaks in ggplot2::scale_fill/color_stepsn
#' @param type fill or color type to return
#' @param col_na color for NA values
#' @param qmin adjust legend to a lower quantile cutoff
#' @param qmax adjust legend to an upper quantile cutoff
#' @param scale.max formatted (e.g. rounded) max of values; if NULL, calculated
#' within function
#' @param scale.min see scale.max
#'
#' @returns ggplot scale object
#' @export
#'
#' @examples
get_color_scale_continuous <- function(values,
                                       zscored = NULL,
                                       colorsteps = "..auto..",
                                       legendbreaks = "..auto..",
                                       legendlabels = "..auto..",
                                       colors = col_pal("RdBu", direction = -1),
                                       colorsteps_nice = T,
                                       type = c("fill", "color"),
                                       col_na = "grey50",
                                       qmin = 0,
                                       qmax = 1,
                                       scale.max = NULL,
                                       scale.min = NULL) {

  #qmin, qmax for featureplot2 from scexpr, for correct limits of colorsteps, colorsteps must be auto or vector
  # scale.min: provided from scexpr featureplot2 but exclude non expressers (=0)

  type <- rlang::arg_match(type)

  decimals <- brathering::decimals_adaptive(values)
  if (is.null(scale.max)) {
    scale.max <- as.numeric(format(brathering::floor2(max(values), decimals), nsmall = decimals))
  }
  if (is.null(scale.min)) {
    scale.min <- as.numeric(format(brathering::ceiling2(min(values), decimals), nsmall = decimals))
  }
  scale.mid <- ifelse(zscored, 0, as.numeric(format(round(scale.min + ((scale.max - scale.min) / 2), decimals), nsmall = decimals)))

  if (is.null(zscored)) {
    zscored <- brathering::is_z_scored(values)
  }

  colorsteps <- sort(unique(colorsteps))
  if (is.null(colorsteps)) {
    ## colorbar legend
    if (type == "fill") {
      scalefun <- ggplot2::scale_fill_gradientn
    } else {
      scalefun <- ggplot2::scale_color_gradientn
    }

    if (length(legendbreaks) == 1 && legendbreaks == "..auto..") {
      legendbreaks <- ggplot2::waiver()
    } else if (length(legendbreaks) == 1 && legendbreaks == "minmidmax") {
      legendbreaks <- c(scale.min, scale.mid, scale.max)
    } else if (length(legendbreaks) == 1) {
      legendbreaks <- seq(scale.min, scale.max, length.out = legendbreaks)
    } else {
      # legendbreaks is vector
    }
    if (length(legendlabels) == 1 && legendlabels == "..auto..") {
      legendlabels <- ggplot2::waiver()
    } else if (length(legendlabels) != length(legendbreaks)) {
      message("length(legendlabels) != length(legendbreaks), using ggplot2 default")
      legendlabels <- ggplot2::waiver()
    }

    scale_fill <-
      scalefun(values = scales::rescale(c(scale.min, scale.mid, scale.max)),
               colors = colors,
               breaks = legendbreaks,
               labels = legendlabels,
               na.value = col_na)
  } else {
    ## colorstep legend
    if (type == "fill") {
      scalefun <- ggplot2::scale_fill_stepsn
    } else {
      scalefun <- ggplot2::scale_color_stepsn
    }

    if (length(colorsteps) == 1) {
      if (zscored) {
        n <- ifelse(colorsteps == "..auto..", 1, colorsteps)
        colorsteps <- seq(round(scale.min), round(scale.max), n)
      } else {
        n <- ifelse(colorsteps == "..auto..", 6, colorsteps)
        if (colorsteps_nice) {
          colorsteps <- scales:::extended_breaks(n = n)(c(round(scale.min, decimals), round(scale.max, decimals)))
        } else {
          colorsteps <- seq(round(scale.min), round(scale.max), length.out = n)
          # make semi nice breaks?
          # round_auto_any(colorsteps)
        }
      }
      # remove limits as they appear anyway
      colorsteps <- colorsteps[-c(1,length(colorsteps))]
    } else {
      # colorsteps is a vector
    }

    scale_fill <-
      scalefun(colors = colors,
               values = scales::rescale(c(scale.min, scale.mid, scale.max)),
               breaks = colorsteps, #round(colorsteps, decimals),
               #labels = format(colorsteps, nsmall = decimals),
               limits = c(scale.min, scale.max), #c(scale.min, scale.max), # what about relevant decimals?
               show.limits = T,
               nice.breaks = F, # is done above
               na.value = col_na)

    # change limits
    if (qmin > 0 || qmax < 1) {
      # only to alter limit labels

      #colorsteps <- round(seq(scale.min, scale.max, length.out = 6),1)
      min.lab <- ifelse(qmin > 0, paste0(scale.min, " (q", round(qmin*100, 0), ")"), scale.min)
      max.lab <- ifelse(qmax < 1, paste0(scale.max, " (q", round(qmax*100, 0), ")"), scale.max)

      colorstepbreaks <- colorsteps
      #if (dplyr::near(colorstepbreaks[1], scale.min)) {
      while(colorstepbreaks[1] < scale.min) {
        colorstepbreaks <- colorstepbreaks[-1]
      }
      #if (dplyr::near(colorstepbreaks[length(colorstepbreaks)], scale.max)) {
      while(colorstepbreaks[length(colorstepbreaks)] > scale.max) {
        colorstepbreaks <- colorstepbreaks[-length(colorstepbreaks)]
      }
      colorstepbreaks <- round(colorstepbreaks, digits = decimals)

      scale_fill <-
        scalefun(colors = colors,
                 values = scales::rescale(c(scale.min, scale.mid, scale.max)),
                 breaks = c(scale.min, colorstepbreaks, scale.max), # manually add limits as breaks
                 limits = c(scale.min, scale.max), # limit must be he same as outer breaks
                 labels = c(min.lab, format(colorstepbreaks, nsmall = decimals), max.lab),
                 na.value = col_na)
    }
  }
  return(scale_fill)
}
