#' Create a continuous or binned ggplot2 colour scale
#'
#' Creates a continuous gradient or a binned colour scale for the `fill` or
#' `colour` aesthetic. Breaks can be calculated automatically, supplied
#' explicitly, or derived from quantiles such as quartiles, quintiles, or
#' deciles.
#'
#' @param values Numeric vector used to determine the scale limits, breaks,
#'   quantiles, and z-score status. Missing and non-finite values are ignored
#'   where appropriate.
#' @param zscored Logical indicating whether `values` are z-scored. If `NULL`,
#'   this is determined automatically.
#' @param steps Controls whether and how the scale is binned. One of:
#'   \itemize{
#'     \item `NULL` for a continuous colour bar.
#'     \item A single number giving the approximate number of breaks.
#'     \item A numeric vector giving the exact internal break positions.
#'     \item `"..auto.."` to choose breaks automatically.
#'     \item A named n-tile specification: `"..tertiles.."`,
#'       `"..terciles.."`, `"..quartiles.."`, `"..quintiles.."`,
#'       `"..sextiles.."`, `"..septiles.."`, `"..octiles.."`,
#'       `"..noniles.."`, `"..deciles.."`, or `"..quantiles.."`.
#'     \item A numeric n-tile specification such as `"..6tiles.."` or
#'       `"..12tiles.."`.
#'     \item leading and trailing dots are optional
#'     \item add a trailing "0" behind tiles, e.g. `"octiles0"` will only
#'     use scale values above 0 to derive bins
#'   }
#' @param legendbreaks Break positions for a continuous colour-bar legend.
#'   Use `"..auto.."` for ggplot2 defaults, `"minmidmax"` for the scale
#'   minimum, midpoint, and maximum, a single number for evenly spaced
#'   breaks, or a numeric vector for explicit breaks.
#' @param legendlabels Labels for `legendbreaks`. Use `"..auto.."` for
#'   ggplot2 defaults. When supplied explicitly, the number of labels should
#'   equal the number of breaks.
#' @param palette Character vector of colours passed to
#'   [ggplot2::scale_fill_gradientn()], [ggplot2::scale_colour_gradientn()],
#'   [ggplot2::scale_fill_stepsn()], or
#'   [ggplot2::scale_colour_stepsn()].
#' @param steps_nice Logical. If `TRUE`, automatically generated numeric
#'   breaks are adjusted to visually convenient values. This has no effect
#'   on explicit break vectors or n-tile breaks.
#' @param type Aesthetic for which the scale is created: `"fill"` or
#'   `"color"`.
#' @param col_na Colour assigned to missing values.
#' @param qmin,qmax Lower and upper quantiles represented by the supplied
#'   data limits. Values may be given as proportions between 0 and 1 or as
#'   percentages. These arguments affect the displayed limit labels.
#' @param scale_min,scale_max Optional numeric scale limits. If `NULL`, they
#'   are calculated from the finite values in `values`.
#' @param trans_log Logical. If `TRUE`, apply a base-10 logarithmic colour
#'   transformation. All finite values and scale limits must then be
#'   positive.
#' @param center_zero Logical. If `TRUE`, position the centre of the palette
#'   at zero. This is primarily intended for diverging continuous scales and
#'   cannot be combined meaningfully with a logarithmic transformation.
#' @param ... Additional arguments reserved for scale customization.
#'
#' @details
#' Named n-tile specifications use empirical quantiles as internal bin
#' boundaries. For example, `steps = "..quartiles.."` creates breaks at
#' probabilities 0.25, 0.50, and 0.75, producing four bins.
#'
#' N-tile bins contain approximately equal numbers of observations, but their
#' numeric widths may differ. Colours are sampled evenly from `palette` and
#' positioned at the midpoint of each bin. Consequently, each bin receives
#' an evenly spaced colour from the palette regardless of its width in data
#' space.
#'
#' Repeated values can produce duplicate quantiles. Duplicate boundaries are
#' removed, so data with many ties may produce fewer than the requested
#' number of bins.
#'
#' @return A ggplot2 continuous or binned colour-scale object.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' set.seed(1)
#' df <- data.frame(
#'   x = rnorm(1000),
#'   y = rnorm(1000),
#'   z = rnorm(1000)
#' )
#'
#' p <- ggplot(df, aes(x, y, colour = z)) +
#'   geom_point()
#'
#' # Automatically selected binned scale
#' p + get_scale_color_fun(df$z)
#'
#' # Continuous colour scale
#' p + get_scale_color_fun(df$z, steps = NULL)
#'
#' # Approximately eight numeric breaks
#' p + get_scale_color_fun(df$z, steps = 8)
#'
#' # Explicit internal breaks
#' p + get_scale_color_fun(df$z, steps = c(-2, -1, 0, 1, 2))
#'
#' # Four equal-frequency bins
#' p + get_scale_color_fun(df$z, steps = "..quartiles..")
#'
#' # Ten equal-frequency bins
#' p + get_scale_color_fun(df$z, steps = "..deciles..")
#'
#' # Arbitrary number of equal-frequency bins
#' p + get_scale_color_fun(df$z, steps = "..12tiles..")
#'
#' # Alternative palette
#' p + get_scale_color_fun(
#'   df$z,
#'   steps = "..quintiles..",
#'   palette = rev(RColorBrewer::brewer.pal(11, "Spectral"))
#' )
#'
#' # Fill scale
#' ggplot(df, aes(x, y, fill = z)) +
#'   geom_raster() +
#'   get_scale_fill_fun(df$z, steps = "..quartiles..")
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
                          center_zero = F,
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

  if (length(unique(values)) == 1) {
    if (type == "fill") {
      scalefun <- ggplot2::scale_fill_gradientn
    } else {
      scalefun <- ggplot2::scale_color_gradientn
    }
    scale_obj <- scalefun(colors = palette)
    return(scale_obj)
  }

  if (qmax > 1) {
    qmax <- qmax/100
    qmin <- qmin/100
  }

  sclfeat <- get_scale_features(
    values = values,
    scale_min = scale_min,
    scale_max = scale_max,
    zscored = zscored)

  limits <- c(sclfeat[["min"]], sclfeat[["max"]])
  zero_pos <- scales::rescale(0, from = limits)

  steps <- make_steps(steps = steps,
                      sclfeat = sclfeat,
                      trans_log = trans_log,
                      steps_nice = steps_nice)
  names(steps) <- format(steps, nsmall = max(brathering::get_decimal_places(limits))) # just as with limits above: shift all values by one char if there are negative values
  # check if it is appropriate in all situations

  if (!is.null(steps)) {
    names(limits) <- format(limits, nsmall = max(brathering::get_decimal_places(steps)))
  }

  # if (center_zero) {
  #   values <- c(0, zero_pos, 1)
  # } else {
  #   if (is.null(steps)) {
  #     values <- scales::rescale(c(sclfeat[["min"]], sclfeat[["mid"]], sclfeat[["max"]]))
  #   } else {
  #     values <- scales::rescale(steps)
  #     #values <- setNames(seq(0.2,0.8,0.2), steps)
  #   }
  # }

  palette_values <- NULL

  if (center_zero) {
    palette_values <- c(0, zero_pos, 1)

  } else if (is.null(steps)) {
    # Continuous scale
    palette_values <- scales::rescale(
      c(sclfeat[["min"]], sclfeat[["mid"]], sclfeat[["max"]]),
      from = limits
    )

  } else {
    # Remove breaks equal to or outside the limits
    steps <- steps[
      is.finite(steps) &
        steps > limits[1] &
        steps < limits[2]
    ]

    boundaries <- c(limits[1], steps, limits[2])

    # ggplot performs binning in transformed scale space
    transformed_boundaries <- if (trans_log) {
      log10(boundaries)
    } else {
      boundaries
    }

    transformed_limits <- range(transformed_boundaries)

    # One palette position for each bin
    bin_midpoints <- (
      head(transformed_boundaries, -1L) +
        tail(transformed_boundaries, -1L)
    ) / 2

    # palette_values <- scales::rescale(
    #   bin_midpoints,
    #   from = transformed_limits
    # )
    #
    # n_bins <- length(bin_midpoints)
    #
    # # Sample exactly one evenly distributed colour per bin
    # palette <- scales::colour_ramp(palette)(
    #   seq(0, 1, length.out = n_bins)
    # )

    bin_colors <- scales::colour_ramp(palette)(
      seq(0, 1, length.out = length(bin_midpoints))
    )

    midpoint_values <- scales::rescale(
      bin_midpoints,
      from = transformed_limits
    )

    palette <- c(
      bin_colors[1],
      bin_colors,
      bin_colors[length(bin_colors)]
    )

    palette_values <- c(
      0,
      midpoint_values,
      1
    )

  }


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

    # change limits
    if (qmin > 0 || qmax < 1) {

      # only to alter limit labels
      scllabs <- make_new_limits(qmin = qmin,
                                 qmax = qmax,
                                 sclfeat = sclfeat,
                                 steps = steps)
      # labels, breaks, limits still a bit mixed up with naming etc, but seems to work
      scale_obj <-
        scalefun(colors = palette,
                 values = palette_values,
                 nice.breaks = F,
                 breaks = c(sclfeat[["min"]], scllabs[["mids"]], sclfeat[["max"]]), # manually add limits as breaks
                 limits = limits, # limit must be he same as outer breaks
                 labels = c(scllabs[["min"]], format(scllabs[["mids"]], nsmall = sclfeat[["decimals"]]), scllabs[["max"]]),
                 na.value = col_na)

    } else {

      scale_obj <-
        scalefun(colors = palette,
                 values = palette_values,
                 breaks = steps,
                 #labels = format(steps, nsmall = decimals), # done with name of steps
                 limits = limits,
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
                                center_zero = F,
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
                       trans_log = trans_log,
                       center_zero = center_zero)

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
                               center_zero = F,
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
                       trans_log = trans_log,
                       center_zero = center_zero)

  return(out)

}

get_scale_features <- function(values,
                               scale_min = NULL,
                               scale_max = NULL,
                               zscored = NULL) {


  if (is.null(zscored)) {
    zscored <- brathering::is_z_scored(values, verbose = F)
    if (zscored) {
      message("color scale seen as z-scored.")
    }
  }

  #brathering::
  decimals <- brathering::decimals_adaptive(values)

  if (is.null(scale_max)) {
    # ceiling2 and floor2 appropriate?
    scale_max <- as.numeric(format(brathering::ceiling2(max(values[which(is.finite(values))], na.rm = T), decimals),
                                   nsmall = decimals))
  }
  if (is.null(scale_min)) {
    scale_min <- as.numeric(format(brathering::floor2(min(values[which(is.finite(values))], na.rm = T), decimals),
                                   nsmall = decimals))
  }
  scale_mid <- ifelse(zscored, 0, as.numeric(format(round(scale_min + ((scale_max - scale_min) / 2), decimals),
                                                    nsmall = decimals)))

  return(list(
    zscored = zscored,
    decimals = decimals,
    max = scale_max,
    mid = scale_mid,
    min = scale_min,
    uniques = stats::na.omit(sort(unique(values))),
    values = values
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
  if (grepl("tiles", steps[1], ignore.case = T)) {
    if (grepl("0$", steps[1])) {
      steps <- resolve_steps(steps = steps[1], values = sclfeat[["values"]][which(sclfeat[["values"]]>0)])
    } else {
      steps <- resolve_steps(steps = steps[1], values = sclfeat[["values"]])
    }
  }

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

      steps <- steps[-c(1,length(steps))]
    }
    # remove limits as they appear anyway
    # steps <- steps[-c(1,length(steps))]
  } else {
    # steps is a vector
  }

  if (length(sclfeat[["uniques"]]) <= length(steps)) {
    steps <- sclfeat[["uniques"]]
    if (length(steps) == 1) {
      steps <- c(steps, steps+1)
    }
  }
  if (brathering::is_int_like(sclfeat$uniques) && !brathering::is_int_like(steps)) {
    steps <- unique(round(steps))
    if (length(steps) == 1) {
      steps <- c(steps, steps+1)
    }
  }

  return(unique(steps))
}

make_new_limits <- function(qmin = 0,
                            qmax = 1,
                            sclfeat,
                            steps) {

  # min <- sclfeat[["min"]]
  # max <- sclfeat[["max"]]

  decimals <- max(sclfeat[["decimals"]], brathering::get_decimal_places(steps))
  steps2 <- c(sclfeat[["min"]], steps, sclfeat[["max"]])
  names(steps2) <- format(steps2)
  min <- names(steps2)[1]
  max <- names(steps2)[length(names(steps2))]

  min.lab <- ifelse(qmin > 0, paste0(min, " (q", qmin*100, ")"), min)
  max.lab <- ifelse(qmax < 1, paste0(max, " (q", qmax*100, ")"), max)

  colorstepbreaks <- steps
  #if (dplyr::near(colorstepbreaks[1], min)) {
  while(length(colorstepbreaks) > 0 && colorstepbreaks[1] < as.numeric(min)) {
    colorstepbreaks <- colorstepbreaks[-1]
  }

  #if (dplyr::near(colorstepbreaks[length(colorstepbreaks)], max)) {
  while(length(colorstepbreaks) > 0 && colorstepbreaks[length(colorstepbreaks)] > as.numeric(max)) {
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

resolve_steps <- function(steps, values, na.rm = TRUE) {
  if (
    !is.character(steps) ||
    length(steps) != 1L ||
    is.na(steps)
  ) {
    return("..auto..")
  }

  step_name <- gsub("[^[:alpha:]]", "", tolower(steps))

  divisions <- switch(
    step_name,
    tertiles  = 3L,
    terciles  = 3L,
    quartiles = 4L,
    quintiles = 5L,
    sextiles  = 6L,
    septiles  = 7L,
    octiles   = 8L,
    noniles   = 9L,
    deciles   = 10L,
    quantiles = 10L,
    NULL
  )

  # Also supports forms such as "..6tiles..", "..8tiles..", etc.
  if (is.null(divisions) && grepl("^[2-9][0-9]*tiles$", steps)) {
    divisions <- as.integer(sub("^([0-9]+)tiles$", "\\1", steps))
  }

  if (is.null(divisions)) {
    return("..auto..")
  }

  steps <- stats::quantile(
    values,
    probs = seq_len(divisions - 1L) / divisions,
    na.rm = na.rm
  )

  if (length(unique(steps)) == 1) {
    message("only one colorstep derived: ", unique(steps), ". switch to '..auto..'")
    steps <- "..auto.."
  }

  return(steps)
}
