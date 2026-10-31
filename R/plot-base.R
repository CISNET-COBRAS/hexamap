#' Draw a hexamap layout with base graphics
#'
#' Draws a layout computed by [hexamap_layout()] on the current device. The
#' hexamap, its isoline labels and the colorbar are sized to fit the current
#' plotting region, so hexamaps can be combined with `par(mfrow = )` or
#' [graphics::layout()].
#'
#' @param x A `hexamap_layout` object.
#' @param title_text,subtitle_text Title and subtitle, drawn above the
#'   hexamap. `NULL` or `""` for none.
#' @param line_width,line_color Width and color of the isolines.
#' @param label_size,label_color Size and color of the isoline and colorbar
#'   labels.
#' @param scale_units Text displayed above the colorbar.
#' @param legend Logical; whether to draw the colorbar.
#' @param colorbar_width Width of the colorbar, in inches.
#' @param colorbar_height Height of the colorbar, as a fraction of the
#'   height of the hexamap.
#' @param ... Not used.
#' @return `x`, invisibly.
#' @export
#' @examples
#' rates <- outer(15:70, 1999:2018, function(a, p) (a - 10) * (p - 1990) / 40)
#' lay <- hexamap_layout(rates, first_age = 15, first_period = 1999,
#'                       interval = 1, isoline_interval = 10)
#' plot(lay, title_text = "Simulated rates", scale_units = "Rate")
plot.hexamap_layout <- function(x,
                                title_text = NULL,
                                subtitle_text = NULL,
                                line_width = .5,
                                line_color = "grey",
                                label_size = .5,
                                label_color = "black",
                                scale_units = "Rate",
                                legend = TRUE,
                                colorbar_width = 0.15,
                                colorbar_height = 0.5,
                                ...) {
  check_number(label_size, "label_size", positive = TRUE)
  check_number(line_width, "line_width")
  check_flag(legend, "legend")
  check_number(colorbar_width, "colorbar_width", positive = TRUE)
  check_number(colorbar_height, "colorbar_height", positive = TRUE)
  has_title <- !is.null(title_text) && nzchar(title_text)
  has_subtitle <- !is.null(subtitle_text) && nzchar(subtitle_text)

  # titles go in the top margin; labels and colorbar inside the plot region
  top_lines <- 0.5 + 1.6 * has_title + 1.3 * has_subtitle
  old_par <- par(mar = c(0.5, 0.5, top_lines, 0.5), xpd = NA)
  on.exit(par(old_par), add = TRUE)
  plot.new()

  labels <- isoline_labels(x)
  ticks <- tick_labels(x)
  space <- label_space(x, labels, ticks, label_size * par("cex"), legend,
                       colorbar_width, scale_units)
  fit_plot_window(x$extent, space$margins)

  draw_hexagons(x)
  draw_isolines(x, labels, line_color, line_width, label_color, label_size)
  if (legend) {
    draw_colorbar(x, ticks, space, colorbar_width, colorbar_height,
                  scale_units, line_width, label_color, label_size)
  }

  if (has_title) {
    title(main = title_text, line = 0.6 + 1.3 * has_subtitle)
  }
  if (has_subtitle) {
    title(main = subtitle_text, line = 0.6, cex.main = 1, font.main = 1)
  }
  invisible(x)
}

isoline_labels <- function(x) {
  list(age = paste("A:", x$isolines$age$value),
       period = paste("P:", x$isolines$period$value),
       cohort = paste("C:", x$isolines$cohort$value))
}

tick_labels <- function(x) {
  list(value = format_tick_labels(x$scale$ticks$value),
       percent = if (x$scale$type == "quantile") paste0(x$scale$ticks$fraction * 100, "%"))
}

#' Space needed around the hexamap for labels and the colorbar
#'
#' Text sizes do not depend on the plot scale, so the space is computed in
#' inches and converted to user coordinates once the scale is known.
#'
#' @return A list with `margins` (left, right, bottom, top, in inches) and
#'   the horizontal positions of the colorbar elements, in inches from the
#'   right edge of the hexamap.
#' @noRd
label_space <- function(x, labels, ticks, cex, legend, colorbar_width,
                        scale_units) {
  width <- function(s) {
    if (length(s) == 0) 0 else max(graphics::strwidth(s, units = "inches", cex = cex))
  }
  char_height <- graphics::strheight("M", units = "inches", cex = cex)
  pad <- 0.05
  cos30 <- cos(pi / 6)
  sin30 <- 0.5

  age_w <- width(labels$age)
  cohort_w <- width(labels$cohort)
  # age labels slope down to the right; cohort labels slope down to the left
  left <- cohort_w * cos30 + char_height + pad
  right <- age_w * cos30 + char_height + pad
  bottom <- max(age_w, cohort_w) * sin30 + char_height + pad
  top <- width(labels$period) + pad

  space <- list(margins = c(left = left, right = right, bottom = bottom, top = top))
  if (legend) {
    gap <- 0.2
    tick <- 0.05
    percent_w <- if (length(ticks$percent)) width(ticks$percent) + tick + pad else 0
    bar_start <- right + gap + percent_w
    block <- percent_w + colorbar_width + tick + pad + width(ticks$value)
    title_overhang <- percent_w + (colorbar_width + width(scale_units)) / 2
    space$bar_start <- bar_start
    space$margins[["right"]] <- right + gap + max(block, title_overhang) + pad
  }
  space
}

#' Set up user coordinates so the hexamap and its labels fill the region
#'
#' @param extent Extent of the hexamap in user coordinates.
#' @param margins Space needed around it, in inches.
#' @return User units per inch, invisibly.
#' @noRd
fit_plot_window <- function(extent, margins) {
  pin <- par("pin")
  avail_w <- pin[1] - margins[["left"]] - margins[["right"]]
  avail_h <- pin[2] - margins[["bottom"]] - margins[["top"]]
  if (avail_w <= 0 || avail_h <= 0) {
    stop("The plotting region is too small for the labels; enlarge the ",
         "device or reduce `label_size`.", call. = FALSE)
  }
  upi <- max((extent[["xmax"]] - extent[["xmin"]]) / avail_w,
             (extent[["ymax"]] - extent[["ymin"]]) / avail_h)
  graphics::plot.window(
    xlim = c(extent[["xmin"]] - margins[["left"]] * upi,
             extent[["xmax"]] + margins[["right"]] * upi),
    ylim = c(extent[["ymin"]] - margins[["bottom"]] * upi,
             extent[["ymax"]] + margins[["top"]] * upi),
    asp = 1, xaxs = "i", yaxs = "i"
  )
  invisible(upi)
}

draw_hexagons <- function(x) {
  drawn <- !is.na(x$cells$color)
  hex_x <- outer(x$cells$x[drawn], x$hexagon$x, "+")
  hex_y <- outer(x$cells$y[drawn], x$hexagon$y, "+")
  # one polygon() call, with hexagons separated by NA
  polygon(x = as.vector(t(cbind(hex_x, NA))),
          y = as.vector(t(cbind(hex_y, NA))),
          col = x$cells$color[drawn], border = NA)
}

draw_isolines <- function(x, labels, line_color, line_width, label_color,
                          label_size) {
  iso <- x$isolines
  for (type in c("age", "period", "cohort")) {
    d <- iso[[type]]
    graphics::segments(d$x1, d$y1, d$x2, d$y2, col = line_color, lwd = line_width)
  }
  text(x = iso$age$x2, y = iso$age$y2, labels = labels$age,
       col = label_color, cex = label_size, srt = -30, adj = c(0, 0.5))
  text(x = iso$period$x2, y = iso$period$y2, labels = labels$period,
       col = label_color, cex = label_size, srt = 90, adj = c(0, 0.5))
  text(x = iso$cohort$x1, y = iso$cohort$y1, labels = labels$cohort,
       col = label_color, cex = label_size, srt = 30, adj = c(1, 0.5))
}

draw_colorbar <- function(x, ticks, space, colorbar_width,
                          colorbar_height, scale_units, line_width,
                          label_color, label_size) {
  # user units per inch, as set by plot.window() (asp = 1 can widen one axis)
  upi <- diff(par("usr")[1:2]) / par("pin")[1]
  colors <- x$scale$colors
  n <- length(colors)
  tick <- 0.05 * upi

  cb_x1 <- x$extent[["xmax"]] + space$bar_start * upi
  cb_x2 <- cb_x1 + colorbar_width * upi
  y_mid <- (x$extent[["ymin"]] + x$extent[["ymax"]]) / 2
  half <- colorbar_height * (x$extent[["ymax"]] - x$extent[["ymin"]]) / 2
  cb_y1 <- y_mid - half
  cb_y2 <- y_mid + half

  cb_y <- seq(from = cb_y1, to = cb_y2, length.out = n + 1)
  rect(xleft = cb_x1, ybottom = cb_y[1:n], xright = cb_x2, ytop = cb_y[2:(n + 1)],
       col = colors, border = NA)
  text(x = (cb_x1 + cb_x2) / 2, y = cb_y2 + 0.08 * upi, labels = scale_units,
       col = label_color, cex = label_size, adj = c(0.5, 0))

  # value ticks on the right; percentile ticks on the left for quantile scales
  tick_y <- cb_y1 + x$scale$ticks$fraction * (cb_y2 - cb_y1)
  graphics::segments(cb_x2, tick_y, cb_x2 + tick, tick_y,
                     col = label_color, lwd = line_width)
  text(x = cb_x2 + tick + 0.05 * upi, y = tick_y, labels = ticks$value,
       col = label_color, cex = label_size, adj = c(0, 0.5))
  if (length(ticks$percent)) {
    graphics::segments(cb_x1 - tick, tick_y, cb_x1, tick_y,
                       col = label_color, lwd = line_width)
    text(x = cb_x1 - tick - 0.05 * upi, y = tick_y, labels = ticks$percent,
         col = label_color, cex = label_size, adj = c(1, 0.5))
  }
}
