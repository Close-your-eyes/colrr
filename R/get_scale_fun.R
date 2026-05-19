#' Make a continuous fill/color scale for ggplot
#'
#' Make nice default color steps.
#'
#' @param values vector of numeric values
#' @param zscored are values z-scored? if NULL, evaluated within function
#' @param steps NULL for colorbar, a number for number of steps or a vector or
#' ..auto.. for decision based on zscored etc; when steps_nice = T it
#' influences number of steps
#' @param legendbreaks vector of breaks
#' @param legendlabels vector of labels
#' @param palette to color arg of ggplot2::scale_fill/color_gradientn or
#' ggplot2::scale_fill/color_stepsn
#' @param steps_nice to nice.breaks in ggplot2::scale_fill/color_stepsn
#' @param type fill or color type to return
#' @param col_na color for NA values
#' @param qmin adjust legend by percentile cutoff at lower end
#' @param qmax adjust legend by percentile cutoff at upper end
#' @param scale_max formatted (e.g. rounded) max of values; if NULL, calculated
#' within function
#' @param scale_min see max
#' @param trans_log log transform colorscale?
#' @param ...
#'
#' @returns ggplot scale object
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(colrr)
#'
#' df <- data.frame(x = rnorm(1000),
#'                  y = rnorm(1000),
#'                  z = rnorm(1000))
#'
#' p <- ggplot(df, aes(x,y,color = z)) +
#'   geom_point()
#' p
#'
#' # color steps by default
#' p + colrr::get_scale_color_fun(df$z)
#'
#' # back to normal
#' p + colrr::get_scale_color_fun(df$z,
#'                                steps = NULL)
#'
#' # set number of steps
#' p + colrr::get_scale_color_fun(df$z,
#'                                steps = 12)
#'
#' # set steps exactly
#' p + colrr::get_scale_color_fun(df$z,
#'                                steps = c(-2.5,-2,0,1))
#'
#' # other palette
#' p + colrr::get_scale_color_fun(df$z,
#'                                palette = colorRamps::blue2red(n = 50))
#'
#'
#'
#' # color vecor not z-scored
#' # colpal_info <- col_pal()
#' df$z <- runif(1000, max = 9)
#' p <- ggplot(df, aes(x,y,color = z)) +
#'   geom_point()
#'
#' p + colrr::get_scale_color_fun(df$z,
#'                                palette = colrr::col_pal("spectral", direction = -1))
#'
#' # spectral color palette from rcolorbrewer
#' p + colrr::get_scale_color_fun(df$z,
#'                                palette = rev(RColorBrewer::brewer.pal(11, "Spectral")))
#'
#' # rainbow palette
#' p + colrr::get_scale_color_fun(df$z,
#'                                palette = grDevices::rainbow(n = 50),
#'                                steps = 10)
#' # jet palette
#' p + colrr::get_scale_color_fun(df$z,
#'                                palette = colrr::col_pal("jet"),
#'                                steps = NULL)
#'
#' p + colrr::get_scale_color_fun(df$z,
#'                                palette = colrr::col_pal("jetColors"),
#'                                steps = NULL)
#'
#' p + colrr::get_scale_color_fun(df$z,
#'                                palette = colrr::col_pal("spectral", direction = -1),
#'                                steps = NULL)
#'
#' p + colrr::get_scale_color_fun(df$z,
#'                                palette = colrr::col_pal("spectral", direction = -1),
#'                                steps = 8)
#'
#' # quantile limits: when cutting the color value range to any percentile, add this info to the color legend
#' df$z <- scales::squish(df$z, range = c(quantile(df$z, 0.2), quantile(df$z, 0.8)))
#' max(df$z)
#' min(df$z)
#' p <- ggplot(df, aes(x,y,color = z)) +
#'   geom_point()
#'
#' p + colrr::get_scale_color_fun(df$z,
#'                                palette = colrr::col_pal("spectral", direction = -1),
#'                                qmin = 0.2,
#'                                qmax = 0.8)
#'
#' # upper and lower limit missing yet, here
#' p + colrr::get_scale_color_fun(df$z,
#'                                palette = colrr::col_pal("spectral", direction = -1),
#'                                qmin = 0.2,
#'                                qmax = 0.8,
#'                                steps = NULL)
#'
#' p + colrr::get_scale_color_fun(df$z,
#'                                palette = colrr::col_pal("spectral", direction = -1),
#'                                qmin = 0.2,
#'                                qmax = 0.8,
#'                                steps = 10)
get_scale_fun <- function(values,
                          zscored = NULL,
                          steps = "..auto..",
                          legendbreaks = "..auto..",
                          legendlabels = "..auto..",
                          palette = col_pal("RdBu", direction = -1),
                          steps_nice = T,
                          type = c("fill", "color"),
                          col_na = "grey50",
                          qmin = 0,
                          qmax = 1,
                          scale_max = NULL,
                          scale_min = NULL,
                          trans_log = F,
                          ...) {

  if (!requireNamespace("brathering", quietly = T)) {
    pak::pak("Close-your-eyes/brathering")
  }

  #qmin, qmax for featureplot2 from scexpr, for correct limits of steps, steps must be auto or vector
  # min: provided from scexpr featureplot2 but exclude non expressers (=0)

  if (trans_log && any(values <= 0, na.rm = TRUE)) {
    stop("Log scale requested but values contain non-positive numbers.")
  }

  type <- rlang::arg_match(type)

  if (qmax > 1) {
    qmax <- qmax/100
    qmin <- qmin/100
  }

  sclfeat <- get_features(
    values = values,
    scale_min = scale_min,
    scale_max = scale_max,
    zscored = zscored
  )
  values <- scales::rescale(c(sclfeat[["min"]], sclfeat[["mid"]], sclfeat[["max"]]))


  if (is.null(steps)) {

    ## continuous colorbar legend without steps
    if (type == "fill") {
      scalefun <- ggplot2::scale_fill_gradientn
    } else {
      scalefun <- ggplot2::scale_color_gradientn
    }

    breaks_labels <- make_breaks_labels(legendbreaks = legendbreaks,
                                        legendlabels = legendlabels,
                                        sclfeat = sclfeat)

    scale_obj <-
      scalefun(values = values,
               colors = palette,
               breaks = breaks_labels[["breaks"]],
               labels = breaks_labels[["labels"]],
               na.value = col_na,
               transform = ifelse(trans_log, "log10", "identity"))
  } else {

    ## colorstep legend
    if (type == "fill") {
      scalefun <- ggplot2::scale_fill_stepsn
    } else {
      scalefun <- ggplot2::scale_color_stepsn
    }

    steps <- make_steps(steps = steps,
                        sclfeat = sclfeat,
                        trans_log = trans_log,
                        steps_nice = steps_nice)

    # change limits
    if (qmin > 0 || qmax < 1) {

      # only to alter limit labels
      scllabs <- make_new_limits(qmin = qmin,
                                 qmax = qmax,
                                 sclfeat = sclfeat,
                                 steps = steps)

      scale_obj <-
        scalefun(colors = palette,
                 values = scales::rescale(steps),
                 breaks = c(sclfeat[["min"]], scllabs[["mids"]], sclfeat[["max"]]), # manually add limits as breaks
                 limits = c(sclfeat[["min"]], sclfeat[["max"]]), # limit must be he same as outer breaks
                 labels = c(scllabs[["min"]], format(scllabs[["mids"]], nsmall = sclfeat[["decimals"]]), scllabs[["max"]]),
                 na.value = col_na)

    } else {

      scale_obj <-
        scalefun(colors = palette,
                 #values = values,
                 values = scales::rescale(steps),
                 breaks = steps, #round(steps, decimals),
                 #labels = format(steps, nsmall = decimals),
                 limits = c(sclfeat[["min"]], sclfeat[["max"]]), #c(min, max), # what about relevant decimals?
                 show.limits = T,
                 nice.breaks = F, # is done above
                 na.value = col_na)

    }
  }

  return(scale_obj)
}

#' @rdname get_scale_fun
#' @export
get_scale_color_fun <- function(values,
                                zscored = NULL,
                                steps = "..auto..",
                                legendbreaks = "..auto..",
                                legendlabels = "..auto..",
                                palette = col_pal("RdBu", direction = -1),
                                steps_nice = T,
                                col_na = "grey50",
                                qmin = 0,
                                qmax = 1,
                                scale_max = NULL,
                                scale_min = NULL,
                                trans_log = F,
                                ...) {

  out <- get_scale_fun(values = values,
                       zscored = zscored,
                       steps = steps,
                       legendbreaks = legendbreaks,
                       legendlabels = legendlabels,
                       palette = palette,
                       steps_nice = steps_nice,
                       col_na = col_na,
                       type = "color",
                       qmin = qmin,
                       qmax = qmax,
                       scale_max = scale_max,
                       scale_min = scale_min,
                       trans_log = trans_log)

  return(out)

}

#' @rdname get_scale_fun
#' @export
get_scale_fill_fun <- function(values,
                               zscored = NULL,
                               steps = "..auto..",
                               legendbreaks = "..auto..",
                               legendlabels = "..auto..",
                               palette = col_pal("RdBu", direction = -1),
                               steps_nice = T,
                               col_na = "grey50",
                               qmin = 0,
                               qmax = 1,
                               scale_max = NULL,
                               scale_min = NULL,
                               trans_log = F,
                               ...) {

  out <- get_scale_fun(values = values,
                       zscored = zscored,
                       steps = steps,
                       legendbreaks = legendbreaks,
                       legendlabels = legendlabels,
                       palette = palette,
                       steps_nice = steps_nice,
                       col_na = col_na,
                       type = "fill",
                       qmin = qmin,
                       qmax = qmax,
                       scale_max = scale_max,
                       scale_min = scale_min,
                       trans_log = trans_log)

  return(out)

}

get_features <- function(values,
                         scale_min = NULL,
                         scale_max = NULL,
                         zscored = NULL) {
  if (is.null(zscored)) {
    zscored <- brathering::is_z_scored(values, verbose = F)
    if (zscored) {
      message("color scale seen as z-scored.")
    }
  }

  decimals <- brathering::decimals_adaptive(values)

  if (is.null(scale_max)) {
    scale_max <- as.numeric(format(brathering::floor2(max(values, na.rm = T), decimals),
                                   nsmall = decimals))
  }
  if (is.null(scale_min)) {
    scale_min <- as.numeric(format(brathering::ceiling2(min(values, na.rm = T), decimals),
                                   nsmall = decimals))
  }
  scale_mid <- ifelse(zscored, 0, as.numeric(format(round(scale_min + ((scale_max - scale_min) / 2), decimals),
                                                    nsmall = decimals)))

  return(list(
    zscored = zscored,
    decimals = decimals,
    max = scale_max,
    mid = scale_mid,
    min = scale_min
  ))
}

make_breaks_labels <- function(legendbreaks = "..auto..",
                               legendlabels = "..auto..",
                               sclfeat) {

  min <- sclfeat[["min"]]
  mid <- sclfeat[["mid"]]
  max <- sclfeat[["max"]]

  if (length(legendbreaks) == 1 && legendbreaks == "..auto..") {
    legendbreaks <- ggplot2::waiver()
  } else if (length(legendbreaks) == 1 && legendbreaks == "minmidmax") {
    legendbreaks <- c(min, mid, max)
  } else if (length(legendbreaks) == 1) {
    legendbreaks <- seq(min, max, length.out = legendbreaks)
  } else {
    # legendbreaks is vector
  }
  if (length(legendlabels) == 1 && legendlabels == "..auto..") {
    legendlabels <- ggplot2::waiver()
  } else if (length(legendlabels) != length(legendbreaks)) {
    message("length(legendlabels) != length(legendbreaks), using ggplot2 default")
    legendlabels <- ggplot2::waiver()
  }

  return(list(breaks = legendbreaks,
              labels = legendlabels))
}

make_steps <- function(steps = "..auto..",
                       sclfeat,
                       trans_log = F,
                       steps_nice = T) {

  min <- sclfeat[["min"]]
  mid <- sclfeat[["mid"]]
  max <- sclfeat[["max"]]
  decimals <- sclfeat[["decimals"]]
  zscored <- sclfeat[["zscored"]]

  steps <- sort(unique(steps))
  if (length(steps) == 1) {
    if (zscored) {

      if (steps == "..auto..") {
        steps <- seq(round(min), round(max), 1)
      } else {
        # steps is numeric of len 1
        steps <- make_zscore_breaks(min_x = min, max_x = max, n_breaks = steps)
        steps <- round(steps, decimals)
        steps <- sort(unique(c(steps, 0)))
      }

    } else {
      n <- ifelse(steps == "..auto..", 6, steps)
      if (steps_nice) {
        if (trans_log) {
          steps <- scales::log_breaks(n = n)(
            c(round(min, decimals), round(max, decimals))
          )
        } else {
          steps <- scales::extended_breaks(n = n)(
            c(round(min, decimals), round(max, decimals))
          )
        }
      } else {
        if (trans_log) {
          steps <- round(
            10 ^ seq(log10(min), log10(max), length.out = n),
            digits = 2
          )
        } else {
          steps <- seq(round(min), round(max), length.out = n)
        }
        # make semi nice breaks?
        # round_auto_any(steps)
      }
    }
    # remove limits as they appear anyway
    steps <- steps[-c(1,length(steps))]
  } else {
    # steps is a vector
  }

  return(steps)
}

make_new_limits <- function(qmin = 0,
                            qmax = 1,
                            sclfeat,
                            steps) {
  min <- sclfeat[["min"]]
  decimals <- sclfeat[["decimals"]]
  max <- sclfeat[["max"]]

  min.lab <- ifelse(qmin > 0, paste0(min, " (q", qmin*100, ")"), min)
  max.lab <- ifelse(qmax < 1, paste0(max, " (q", qmax*100, ")"), max)

  colorstepbreaks <- steps
  #if (dplyr::near(colorstepbreaks[1], min)) {
  while(length(colorstepbreaks) > 0 && colorstepbreaks[1] < min) {
    colorstepbreaks <- colorstepbreaks[-1]
  }

  #if (dplyr::near(colorstepbreaks[length(colorstepbreaks)], max)) {
  while(length(colorstepbreaks) > 0 && colorstepbreaks[length(colorstepbreaks)] > max) {
    colorstepbreaks <- colorstepbreaks[-length(colorstepbreaks)]
  }
  colorstepbreaks <- round(colorstepbreaks, digits = decimals)

  return(list(min = min.lab,
              mids = colorstepbreaks,
              max = max.lab))
}


make_zscore_breaks <- function(min_x, max_x, n_breaks) {
  # rng <- range(x, na.rm = TRUE)
  # min_x <- rng[1]
  # max_x <- rng[2]
  #
  # distances from zero
  neg_range <- abs(min(0, min_x))
  pos_range <- max(0, max_x)
  total_range <- neg_range + pos_range

  if (total_range == 0) {
    return(rep(0, n_breaks))
  }

  # allocate breaks proportionally
  n_neg <- round(n_breaks * (neg_range / total_range))
  n_pos <- n_breaks - n_neg

  # ensure at least one bin if range exists
  if (neg_range > 0 && n_neg == 0) n_neg <- 1
  if (pos_range > 0 && n_pos == 0) n_pos <- 1

  # recompute to keep total correct
  n_pos <- n_breaks - n_neg

  # generate sequences
  neg_breaks <- if (n_neg > 0) {
    seq(min_x, 0, length.out = n_neg + 1)
  } else numeric(0)

  pos_breaks <- if (n_pos > 0) {
    seq(0, max_x, length.out = n_pos + 1)
  } else numeric(0)

  # combine, avoiding duplicate zero
  breaks <- c(neg_breaks, pos_breaks[-1])

  return(breaks)
}
