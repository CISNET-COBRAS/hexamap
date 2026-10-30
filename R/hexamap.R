#' Plot a hexamap of age-period-cohort data
#'
#' Draws a field of hexagonal tiles, one per age-period cell, with age,
#' period and cohort isolines at equal 60 degree angles, plus a colorbar.
#' Each tile is centered at \eqn{x = p\sqrt{3}/2} and \eqn{y = a - p/2}, so
#' neighboring tiles are one `interval` apart along every APC axis.
#'
#' @param data Numeric matrix (or data frame of numeric columns) with ages as
#'   rows and periods as columns. Missing and non-finite cells are not drawn.
#' @param first_age First age in `data`.
#' @param first_period First period in `data`.
#' @param interval Width of the age and period groups (must be equal).
#' @param first_age_isoline,first_period_isoline First age and period at
#'   which to draw an isoline. Default to `first_age` and `first_period`.
#' @param isoline_interval Spacing between isolines. Defaults to
#'   `2 * interval`.
#' @param colorbar_range Numeric vector of length 2 with the lower and upper
#'   limits of the color scale. Values outside are truncated. Defaults to the
#'   range of `data`.
#' @param colorbar_scale How colors are mapped to values: `"linear"`, `"log"`
#'   or `"quantile"` (each color covers the same share of cells). The names
#'   `"Normal"` and `"Percentile"` used in earlier versions are also accepted.
#' @param colorbar_width,colorbar_height Size of the colorbar relative to the
#'   plot.
#' @param title_text,subtitle_text Title and subtitle.
#' @param color_map Vector of colors from low to high. Defaults to a 100-color
#'   "jet" palette.
#' @param line_width,line_color Width and color of the isolines.
#' @param label_size,label_color Size and color of the isoline and colorbar
#'   labels.
#' @param scale_units Text displayed above the colorbar.
#' @param zoom_factor Scaling of the plotting region, in `(0, 1]`.
#' @param wrap_cohort_labels Logical; whether cohort labels wrap around the
#'   bottom edge of the hexamap.
#'
#' @return Called for its side effect of drawing on the current device;
#'   returns `NULL` invisibly.
#' @references Jalal H, Burke DS. Hexamaps for age-period-cohort data
#'   visualization and implementation in R. *Epidemiology*.
#'   2020;31(6):e47-e49. \doi{10.1097/EDE.0000000000001236}
#' @export
#' @examples
#' ages <- 15:70
#' periods <- 1999:2018
#' rates <- outer(ages, periods, function(a, p) exp(-((a - 45) / 15)^2 + (p - 1999) / 20))
#' hexamap(rates, first_age = 15, first_period = 1999, interval = 1,
#'         isoline_interval = 10)
hexamap <- function(data,
                    first_age,
                    first_period,
                    interval,
                    first_age_isoline = NULL,
                    first_period_isoline = NULL,
                    isoline_interval = NULL,
                    colorbar_range = NULL,
                    colorbar_scale = "linear",
                    colorbar_width = 0.2,
                    colorbar_height = 0.25,
                    title_text = "Hexagonal Heatmap",
                    subtitle_text = "",
                    color_map = NULL,
                    line_width = .5,
                    line_color = "grey",
                    label_size = .5,
                    label_color = "black",
                    scale_units = "Rate",
                    zoom_factor = 1,
                    wrap_cohort_labels = TRUE) {

  # check inputs and set default values for missing parameters
  data <- check_apc_matrix(data)
  check_number(first_age, "first_age")
  check_number(first_period, "first_period")
  check_number(interval, "interval", positive = TRUE)
  colorbar_scale <- resolve_scale(colorbar_scale)
  check_number(zoom_factor, "zoom_factor", positive = TRUE)
  if (zoom_factor > 1) {
    stop("`zoom_factor` must be in (0, 1].", call. = FALSE)
  }
  check_flag(wrap_cohort_labels, "wrap_cohort_labels")

  if (is.null(first_age_isoline)) {
    first_age_isoline <- first_age
  }
  if (is.null(first_period_isoline)) {
    first_period_isoline <- first_period
  }
  if (is.null(isoline_interval)) {
    isoline_interval <- 2 * interval
  }
  check_number(isoline_interval, "isoline_interval", positive = TRUE)
  if (is.null(color_map)) {
    # define jet colormap
    jet_colors <- colorRampPalette(c("black", "#00007F", "blue", "#007FFF", "cyan",
                                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
    color_map <- jet_colors(100)
  }
  # end of default values

  last_age <- first_age + (nrow(data) - 1) * interval
  last_period <- first_period + (ncol(data) - 1) * interval
  last_cohort <- last_period - first_age

  check_number(first_age_isoline, "first_age_isoline")
  check_number(first_period_isoline, "first_period_isoline")
  if (first_age_isoline < first_age || first_age_isoline > last_age) {
    stop("`first_age_isoline` must be between ", first_age, " and ", last_age,
         ".", call. = FALSE)
  }
  if (first_period_isoline < first_period || first_period_isoline > last_period) {
    stop("`first_period_isoline` must be between ", first_period, " and ",
         last_period, ".", call. = FALSE)
  }

  age_isolines <- seq(from = first_age_isoline, to = last_age, by = isoline_interval)
  period_isolines <- seq(from = first_period_isoline, to = last_period, by = isoline_interval)
  last_age_isoline <- tail(age_isolines, 1)
  first_cohort_isoline <- first_period_isoline - last_age_isoline
  cohort_isolines <- seq(from = first_cohort_isoline, to = last_cohort, by = isoline_interval)

  periods <- seq(from = first_period, to = last_period, by = interval)
  ages <- seq(from = first_age, to = last_age, by = interval)
  n_ages <- length(ages)
  n_periods <- length(periods)

  # keep only the finite cells and truncate them to the color scale limits
  is_drawn <- is.finite(data)
  if (!any(is_drawn)) {
    stop("`data` has no finite values to plot.", call. = FALSE)
  }
  values <- data[is_drawn]
  colorbar_range <- resolve_limits(colorbar_range, values, colorbar_scale)
  values <- pmin(pmax(values, colorbar_range[1]), colorbar_range[2])

  # === plotting
  ncol <- length(color_map)
  color_index <- compute_color_index(values, colorbar_range, colorbar_scale, ncol)

  a <- interval / sqrt(3) # radius of the hexagon (distance from center to a vertex).
  b <- sqrt(3) * a / 2 # half height of the hexagon
  yv <- c(0, b, b, 0, -b, -b, 0)
  xv <- c(-a, -a/2, a/2, a, a/2, -a/2, -a)

  # compute the center of each hexagon by creating an a*p grid for each age-period combination
  P0 <- matrix(periods, nrow = n_ages, ncol = n_periods, byrow = TRUE)
  A0 <- t(matrix(ages, nrow = n_periods, ncol = n_ages, byrow = TRUE))

  # convert the grid to the X-Y coordinate
  X <- compute_xcoordinate(P0)
  Y <- compute_ycoordinate(P0, A0)

  # only keep the cells that are drawn
  Xvec <- X[is_drawn]
  Yvec <- Y[is_drawn]
  n_hexagons <- length(Xvec)

  # get the color for each hexagon
  hex_colors <- color_map[color_index]

  # compute the X and Y coordinates for each hexagon - each hexagon is a row and each point is a column
  Xhex <- outer(Xvec, xv, '+')
  Yhex <- outer(Yvec, yv, '+')

  minX <- min(Xhex) - interval
  maxX <- max(Xhex) + interval
  maxX2 <- maxX + (maxX - minX) * (colorbar_width * 2)
  if (wrap_cohort_labels) {
    minY <- min(Yhex) - interval
  } else {
    minY <- compute_ycoordinate(p = first_period,
                                a = first_age - (last_period - first_period)) - interval
  }
  maxY <- max(Yhex) + interval

  plt_wdth <- 4.8
  plt_ht <- 4.8

  old_par <- par(mai = c((1 - zoom_factor) * plt_ht / 2,
                         (1 - zoom_factor) * plt_wdth / 2,
                         (1 - zoom_factor) * plt_ht / 2,
                         (1 - zoom_factor) * plt_wdth / 2))
  on.exit(par(old_par), add = TRUE)

  plot(x = NULL, y = NULL,
       xlim = c(minX, maxX2), ylim = c(minY, maxY),
       axes = FALSE, frame.plot = FALSE,
       xaxt = 'n', yaxt = 'n', type = 'n', asp = 1)
  title(main = title_text, line = -1)
  if (nzchar(subtitle_text)) { # prettyNum("") returns "NA"
    title(main = prettyNum(subtitle_text, big.mark = ",", trim = TRUE), line = -2, cex.main = 1)
  }
  for (i in seq_len(n_hexagons)) {
    polygon(x = Xhex[i, ],   # X-Coordinates of polygon
            y = Yhex[i, ],   # Y-Coordinates of polygon
            col = hex_colors[i],  # Color of polygon
            border = NA, # Color of polygon border
            lwd = 1)
  }

  # age-isolines
  y1 <- compute_ycoordinate(first_period, age_isolines)
  y2 <- compute_ycoordinate(last_period + interval, age_isolines)
  x1 <- compute_xcoordinate(first_period)
  x2 <- compute_xcoordinate(last_period + interval)
  for (i in seq_along(age_isolines)) {
    lines(x = c(x1, x2), y = c(y1[i], y2[i]), col = line_color, lwd = line_width)
    text(x = x2, y = y2[i], labels = paste("A:", age_isolines[i]),
         col = label_color, cex = label_size, srt = -30,
         adj = c(0, 0.5))
  }

  # period-isolines
  x <- compute_xcoordinate(period_isolines)
  y1 <- compute_ycoordinate(period_isolines, first_age)
  y2 <- compute_ycoordinate(period_isolines, last_age + interval)
  for (i in seq_along(period_isolines)) {
    lines(x = c(x[i], x[i]), y = c(y1[i], y2[i]), col = line_color, lwd = line_width)
    text(x = x[i], y = y2[i], labels = paste("P:", period_isolines[i]),
         col = label_color, cex = label_size, srt = 90,
         adj = c(0, .5))
  }

  # cohort-isolines
  ends <- cohort_isoline_endpoints(cohort_isolines, first_age, last_age,
                                   first_period, last_period,
                                   wrap = wrap_cohort_labels)
  x1 <- compute_xcoordinate(ends$p1 - interval)
  x2 <- compute_xcoordinate(ends$p2)
  y1 <- compute_ycoordinate(ends$p1 - interval, ends$a1 - interval)
  y2 <- compute_ycoordinate(ends$p2, ends$a2)
  for (i in seq_along(cohort_isolines)) {
    lines(x = c(x1[i], x2[i]), y = c(y1[i], y2[i]), col = line_color, lwd = line_width)
    text(x = x1[i], y = y1[i], labels = paste("C:", cohort_isolines[i]),
         col = label_color, cex = label_size, srt = 30,
         adj = c(1, .5))
  }

  # draw the colorbar
  cb_x1 <- maxX2 - (maxX - minX) * (colorbar_width * 0.5)
  cb_x2 <- maxX2 - (maxX - minX) * (colorbar_width * 0.25)
  cb_y2 <- (minY + maxY)/2 + (maxY - minY) * colorbar_height
  cb_y1 <- (minY + maxY)/2 - (maxY - minY) * colorbar_height

  cb_y_points <- seq(from = cb_y1, to = cb_y2, length.out = ncol + 1)
  rect(xleft = cb_x1, ybottom = cb_y_points[1:ncol],
       xright = cb_x2, ytop = cb_y_points[2:(ncol + 1)],
       col = color_map, border = NA)
  text(x = (cb_x1 + cb_x2) / 2, y = cb_y2 + (maxY - minY) * 0.03,
       labels = scale_units, col = label_color, cex = label_size,
       adj = c(0.5, 0))

  # add value ticks and labels on the right of the colorbar, and percentile
  # ticks on the left when the colors are scaled by quantiles
  n_ticks <- 11
  tick_fractions <- seq(0, 1, length.out = n_ticks)
  tick_pos <- cb_y1 + tick_fractions * (cb_y2 - cb_y1)
  tick_values <- colorbar_tick_values(values, colorbar_range, colorbar_scale,
                                      tick_fractions)
  tick_width <- 0.25 * (cb_x2 - cb_x1)
  value_tick_start <- cb_x2 + tick_width
  for (i in seq_len(n_ticks)) {
    lines(x = c(value_tick_start, cb_x2),
          y = c(tick_pos[i], tick_pos[i]),
          col = label_color, lwd = line_width)
  }
  text(x = value_tick_start + tick_width, y = tick_pos,
       labels = format_tick_labels(tick_values),
       col = label_color, cex = label_size, adj = 0)
  if (colorbar_scale == "quantile") {
    pctile_tick_start <- cb_x1 - tick_width
    for (i in seq_len(n_ticks)) {
      lines(x = c(pctile_tick_start, cb_x1),
            y = c(tick_pos[i], tick_pos[i]),
            col = label_color, lwd = line_width)
    }
    text(x = pctile_tick_start - tick_width, y = tick_pos,
         labels = paste0(tick_fractions * 100, "%"),
         col = label_color, cex = label_size, adj = 1)
  }

  invisible(NULL)
}

#' Convert age-period coordinates to Cartesian coordinates
#'
#' @param p Period.
#' @param a Age.
#' @return Numeric vector of x or y coordinates.
#' @noRd
compute_xcoordinate <- function(p) {
  x <- p * sqrt(3) / 2
  return(x)
}

compute_ycoordinate <- function(p, a){
  y <- a - p / 2
  return(y)
}
