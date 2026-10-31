#' Compute the layout of a hexamap
#'
#' Does all the computation behind [hexamap()] without drawing anything:
#' hexagon centers and colors, isoline segments, color scale and colorbar
#' ticks. Draw the result with [plot()][plot.hexamap_layout()], or inspect it
#' to build custom graphics.
#'
#' @inheritParams hexamap
#' @return An object of class `hexamap_layout`, a list with:
#'   \describe{
#'     \item{`cells`}{Data frame with one row per age-period cell: `age`,
#'       `period`, `cohort`, `value`, hexagon center `x` and `y`, and fill
#'       `color` (`NA` for cells that are not drawn).}
#'     \item{`hexagon`}{List with the `x` and `y` offsets of the hexagon
#'       vertices relative to its center.}
#'     \item{`isolines`}{List of data frames `age`, `period` and `cohort`,
#'       each with the isoline `value` and segment ends `x1`, `y1`, `x2`,
#'       `y2`.}
#'     \item{`scale`}{List with the scale `type`, `limits`, `colors` and
#'       colorbar `ticks` (data frame with `fraction` and `value`).}
#'     \item{`dims`}{List with the first and last age and period, and the
#'       `interval`.}
#'     \item{`extent`}{Numeric vector `xmin`, `xmax`, `ymin`, `ymax` covering
#'       the hexagons and isolines.}
#'   }
#' @export
#' @examples
#' rates <- outer(15:70, 1999:2018, function(a, p) (a - 10) * (p - 1990) / 40)
#' dimnames(rates) <- list(15:70, 1999:2018)
#' lay <- hexamap_layout(rates, isoline_interval = 10)
#' lay
#' head(lay$cells)
#' plot(lay)
hexamap_layout <- function(data,
                           first_age = NULL,
                           first_period = NULL,
                           interval = NULL,
                           first_age_isoline = NULL,
                           first_period_isoline = NULL,
                           isoline_interval = NULL,
                           colorbar_range = NULL,
                           colorbar_scale = "linear",
                           color_map = NULL,
                           wrap_cohort_labels = TRUE) {

  # check inputs and set default values for missing parameters
  data <- check_apc_matrix(data)
  dims <- resolve_dims(data, first_age, first_period, interval)
  interval <- dims$interval
  colorbar_scale <- resolve_scale(colorbar_scale)
  check_flag(wrap_cohort_labels, "wrap_cohort_labels")

  if (is.null(first_age_isoline)) {
    first_age_isoline <- dims$first_age
  }
  if (is.null(first_period_isoline)) {
    first_period_isoline <- dims$first_period
  }
  if (is.null(isoline_interval)) {
    isoline_interval <- 2 * interval
  }
  check_number(isoline_interval, "isoline_interval", positive = TRUE)
  check_number(first_age_isoline, "first_age_isoline")
  check_number(first_period_isoline, "first_period_isoline")
  if (first_age_isoline < dims$first_age || first_age_isoline > dims$last_age) {
    stop("`first_age_isoline` must be between ", dims$first_age, " and ",
         dims$last_age, ".", call. = FALSE)
  }
  if (first_period_isoline < dims$first_period ||
      first_period_isoline > dims$last_period) {
    stop("`first_period_isoline` must be between ", dims$first_period,
         " and ", dims$last_period, ".", call. = FALSE)
  }
  if (is.null(color_map)) {
    # define jet colormap
    jet_colors <- colorRampPalette(c("black", "#00007F", "blue", "#007FFF", "cyan",
                                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
    color_map <- jet_colors(100)
  }
  if (length(color_map) < 1) {
    stop("`color_map` must have at least one color.", call. = FALSE)
  }

  # one row per cell, in the column-major order of `data`
  ages <- seq(from = dims$first_age, to = dims$last_age, by = interval)
  periods <- seq(from = dims$first_period, to = dims$last_period, by = interval)
  cells <- data.frame(age = rep(ages, times = length(periods)),
                      period = rep(periods, each = length(ages)),
                      value = as.vector(data))
  cells$cohort <- cells$period - cells$age
  cells$x <- compute_xcoordinate(cells$period)
  cells$y <- compute_ycoordinate(cells$period, cells$age)
  cells <- cells[, c("age", "period", "cohort", "value", "x", "y")]

  # keep only the finite cells and truncate them to the color scale limits
  is_drawn <- is.finite(cells$value)
  if (!any(is_drawn)) {
    stop("`data` has no finite values to plot.", call. = FALSE)
  }
  values <- cells$value[is_drawn]
  colorbar_range <- resolve_limits(colorbar_range, values, colorbar_scale)
  values <- pmin(pmax(values, colorbar_range[1]), colorbar_range[2])
  color_index <- compute_color_index(values, colorbar_range, colorbar_scale,
                                     length(color_map))
  cells$color <- NA_character_
  cells$color[is_drawn] <- color_map[color_index]

  tick_fractions <- seq(0, 1, length.out = 11)
  ticks <- data.frame(
    fraction = tick_fractions,
    value = colorbar_tick_values(values, colorbar_range, colorbar_scale,
                                 tick_fractions)
  )

  isolines <- compute_isolines(dims, first_age_isoline, first_period_isoline,
                               isoline_interval, wrap_cohort_labels)

  hexagon <- hex_vertices(interval)
  iso_x <- unlist(lapply(isolines, function(d) c(d$x1, d$x2)))
  iso_y <- unlist(lapply(isolines, function(d) c(d$y1, d$y2)))
  extent <- c(xmin = min(cells$x + min(hexagon$x), iso_x),
              xmax = max(cells$x + max(hexagon$x), iso_x),
              ymin = min(cells$y + min(hexagon$y), iso_y),
              ymax = max(cells$y + max(hexagon$y), iso_y))

  structure(
    list(cells = cells,
         hexagon = hexagon,
         isolines = isolines,
         scale = list(type = colorbar_scale, limits = colorbar_range,
                      colors = color_map, ticks = ticks),
         dims = dims[c("first_age", "last_age", "first_period",
                       "last_period", "interval")],
         extent = extent),
    class = "hexamap_layout"
  )
}

#' @export
print.hexamap_layout <- function(x, ...) {
  d <- x$dims
  n_drawn <- sum(!is.na(x$cells$color))
  n_cells <- nrow(x$cells)
  cat("<hexamap_layout>\n")
  cat("  Ages:    ", d$first_age, "-", d$last_age,
      "  Periods: ", d$first_period, "-", d$last_period,
      "  Cohorts: ", d$first_period - d$last_age, "-",
      d$last_period - d$first_age, "\n", sep = "")
  cat("  Interval: ", d$interval, "\n", sep = "")
  cat("  Cells:   ", n_cells, " (", n_drawn, " drawn, ", n_cells - n_drawn,
      " missing)\n", sep = "")
  cat("  Scale:   ", x$scale$type, ", ", format(x$scale$limits[1]), " to ",
      format(x$scale$limits[2]), ", ", length(x$scale$colors), " colors\n",
      sep = "")
  invisible(x)
}

#' Resolve the ages, periods and interval of the data
#'
#' Arguments left `NULL` are taken from the numeric row (age) and column
#' (period) names of `data`.
#'
#' @param data Numeric matrix.
#' @inheritParams hexamap
#' @return A list with `first_age`, `last_age`, `first_period`,
#'   `last_period` and `interval`.
#' @noRd
resolve_dims <- function(data, first_age, first_period, interval) {
  ages_dn <- dimnames_as_numeric(rownames(data))
  periods_dn <- dimnames_as_numeric(colnames(data))

  if (is.null(interval)) {
    steps <- c(diff(ages_dn), diff(periods_dn))
    interval <- if (length(steps) > 0) steps[1] else 1
  }
  check_number(interval, "interval", positive = TRUE)
  tol <- sqrt(.Machine$double.eps) * max(1, interval)
  if (any(abs(diff(ages_dn) - interval) > tol)) {
    stop("The row names of `data` (ages) must be equally spaced by ",
         "`interval` (", interval, "). Ages and periods must use groups of ",
         "the same width.", call. = FALSE)
  }
  if (any(abs(diff(periods_dn) - interval) > tol)) {
    stop("The column names of `data` (periods) must be equally spaced by ",
         "`interval` (", interval, "). Ages and periods must use groups of ",
         "the same width.", call. = FALSE)
  }

  if (is.null(first_age)) {
    if (is.null(ages_dn)) {
      stop("Supply `first_age`, or give `data` numeric row names (ages).",
           call. = FALSE)
    }
    first_age <- ages_dn[1]
  }
  if (is.null(first_period)) {
    if (is.null(periods_dn)) {
      stop("Supply `first_period`, or give `data` numeric column names ",
           "(periods).", call. = FALSE)
    }
    first_period <- periods_dn[1]
  }
  check_number(first_age, "first_age")
  check_number(first_period, "first_period")

  list(first_age = first_age,
       last_age = first_age + (nrow(data) - 1) * interval,
       first_period = first_period,
       last_period = first_period + (ncol(data) - 1) * interval,
       interval = interval)
}

dimnames_as_numeric <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  values <- suppressWarnings(as.numeric(x))
  if (anyNA(values)) NULL else values
}

#' Age, period and cohort isoline segments
#'
#' @param dims Output of `resolve_dims()`.
#' @inheritParams hexamap
#' @return A list of data frames `age`, `period` and `cohort` with the
#'   isoline `value` and the segment ends `x1`, `y1`, `x2`, `y2`. Labels go
#'   at the end of age and period isolines and at the start of cohort
#'   isolines.
#' @noRd
compute_isolines <- function(dims, first_age_isoline, first_period_isoline,
                             isoline_interval, wrap_cohort_labels) {
  first_age <- dims$first_age
  last_age <- dims$last_age
  first_period <- dims$first_period
  last_period <- dims$last_period
  interval <- dims$interval

  age_isolines <- seq(from = first_age_isoline, to = last_age, by = isoline_interval)
  period_isolines <- seq(from = first_period_isoline, to = last_period, by = isoline_interval)
  first_cohort_isoline <- first_period_isoline - tail(age_isolines, 1)
  cohort_isolines <- seq(from = first_cohort_isoline, to = last_period - first_age,
                         by = isoline_interval)

  age <- data.frame(value = age_isolines,
                    x1 = compute_xcoordinate(first_period),
                    y1 = compute_ycoordinate(first_period, age_isolines),
                    x2 = compute_xcoordinate(last_period + interval),
                    y2 = compute_ycoordinate(last_period + interval, age_isolines))

  period <- data.frame(value = period_isolines,
                       x1 = compute_xcoordinate(period_isolines),
                       y1 = compute_ycoordinate(period_isolines, first_age),
                       x2 = compute_xcoordinate(period_isolines),
                       y2 = compute_ycoordinate(period_isolines, last_age + interval))

  ends <- cohort_isoline_endpoints(cohort_isolines, first_age, last_age,
                                   first_period, last_period,
                                   wrap = wrap_cohort_labels)
  cohort <- data.frame(value = cohort_isolines,
                       x1 = compute_xcoordinate(ends$p1 - interval),
                       y1 = compute_ycoordinate(ends$p1 - interval, ends$a1 - interval),
                       x2 = compute_xcoordinate(ends$p2),
                       y2 = compute_ycoordinate(ends$p2, ends$a2))

  list(age = age, period = period, cohort = cohort)
}
