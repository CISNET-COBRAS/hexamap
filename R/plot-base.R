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
#' @param title_adj Horizontal alignment of the title and subtitle: 0 for
#'   left, 0.5 for centered, 1 for right.
#' @param labels Named character vector with the prefixes of the `age`,
#'   `period` and `cohort` isoline labels. Missing names keep their default,
#'   so `labels = c(period = "Year: ")` only changes the period labels.
#' @param highlight List of ages, periods and/or cohorts to draw as
#'   highlighted lines, with optional `col` and `lwd`, e.g.
#'   `list(cohort = 1975)` or `list(age = c(30, 50), col = "white", lwd = 2)`.
#'   Values need not be isolines but must be within the range of the data.
#'   Their labels are drawn in bold.
#' @param line_width,line_color Width and color of the isolines.
#' @param label_size,label_color Size and color of the isoline and colorbar
#'   labels.
#' @param scale_units Text displayed above the colorbar.
#' @param legend Logical; whether to draw the colorbar.
#' @param colorbar_width Width of the colorbar, in inches.
#' @param colorbar_height Height of the colorbar, as a fraction of the
#'   height of the hexamap.
#' @param na_color Color of cells with missing values (for example,
#'   suppressed counts). `NA` leaves them blank.
#' @param na_label Label of the missing-value key shown below the colorbar
#'   when some cells are missing.
#' @param hex_border Color of the hexagon borders. The default, `"fill"`,
#'   draws each border in the hexagon's own color, which avoids the thin
#'   white seams that many viewers show between adjacent tiles. Use `NA` for
#'   no border, or a color such as `"white"` to outline the tiles.
#' @param hex_border_width Line width of the hexagon borders.
#' @param ... Not used.
#' @return `x`, invisibly.
#' @export
#' @examples
#' rates <- outer(15:70, 1999:2018, function(a, p) (a - 10) * (p - 1990) / 40)
#' lay <- hexamap_layout(rates, first_age = 15, first_period = 1999,
#'                       interval = 1, isoline_interval = 10)
#' plot(lay, title_text = "Simulated rates", scale_units = "Rate")
#'
#' # full-word labels and a highlighted birth cohort
#' plot(lay, labels = c(age = "Age ", period = "", cohort = "Born "),
#'      highlight = list(cohort = 1975, col = "white", lwd = 2))
plot.hexamap_layout <- function(x,
                                title_text = NULL,
                                subtitle_text = NULL,
                                title_adj = 0.5,
                                labels = c(age = "A: ", period = "P: ", cohort = "C: "),
                                highlight = NULL,
                                line_width = .5,
                                line_color = "grey",
                                label_size = .7,
                                label_color = "black",
                                scale_units = "Rate",
                                legend = TRUE,
                                colorbar_width = 0.15,
                                colorbar_height = 0.5,
                                na_color = "grey90",
                                na_label = "Missing",
                                hex_border = "fill",
                                hex_border_width = 0.5,
                                ...) {
  check_number(label_size, "label_size", positive = TRUE)
  check_number(hex_border_width, "hex_border_width")
  if (length(na_color) != 1 || length(hex_border) != 1) {
    stop("`na_color` and `hex_border` must be a single color.", call. = FALSE)
  }
  show_na_key <- legend && !is.na(na_color) && anyNA(x$cells$color)
  check_number(line_width, "line_width")
  check_flag(legend, "legend")
  check_number(colorbar_width, "colorbar_width", positive = TRUE)
  check_number(colorbar_height, "colorbar_height", positive = TRUE)
  check_number(title_adj, "title_adj")
  prefixes <- resolve_label_prefixes(labels)
  highlight <- resolve_highlight(highlight, x)
  has_title <- !is.null(title_text) && nzchar(title_text)
  has_subtitle <- !is.null(subtitle_text) && nzchar(subtitle_text)

  # titles go in the top margin; labels and colorbar inside the plot region
  top_lines <- 0.5 + 1.6 * has_title + 1.3 * has_subtitle
  old_par <- par(mar = c(0.5, 0.5, top_lines, 0.5), xpd = NA)
  on.exit(par(old_par), add = TRUE)
  plot.new()

  labels <- isoline_labels(x$isolines, prefixes)
  highlight_labels <- isoline_labels(highlight$isolines, prefixes) # empty if NULL
  ticks <- tick_labels(x)
  if (show_na_key) {
    ticks$na <- na_label
  }
  space <- label_space(x, Map(c, labels, highlight_labels), ticks,
                       label_size * par("cex"), legend, colorbar_width,
                       scale_units)
  fit_plot_window(x$extent, space$margins)

  draw_tiles(x, na_color, hex_border, hex_border_width)
  draw_isolines(x$isolines, labels, x$tiles, line_color, line_width,
                label_color, label_size)
  if (!is.null(highlight)) { # bold labels stay readable for any line color
    draw_isolines(highlight$isolines, highlight_labels, x$tiles, highlight$col,
                  highlight$lwd, label_color, label_size, label_font = 2)
  }
  if (legend) {
    draw_colorbar(x, ticks, space, colorbar_width, colorbar_height,
                  scale_units, line_width, label_color, label_size,
                  na_color)
  }

  if (has_title) {
    title(main = title_text, line = 0.6 + 1.3 * has_subtitle, adj = title_adj)
  }
  if (has_subtitle) {
    title(main = subtitle_text, line = 0.6, cex.main = 1, font.main = 1,
          adj = title_adj)
  }
  invisible(x)
}

isoline_labels <- function(isolines, prefixes) {
  # paste0() turns zero-length values into one label, so guard against it
  label <- function(type) {
    values <- isolines[[type]]$value
    if (length(values) == 0) character(0) else paste0(prefixes[[type]], values)
  }
  list(age = label("age"), period = label("period"), cohort = label("cohort"))
}

resolve_label_prefixes <- function(labels) {
  prefixes <- c(age = "A: ", period = "P: ", cohort = "C: ")
  if (is.null(labels)) {
    return(prefixes)
  }
  if (!is.character(labels) || is.null(names(labels)) ||
      !all(names(labels) %in% names(prefixes))) {
    stop("`labels` must be a named character vector with names among ",
         "\"age\", \"period\" and \"cohort\".", call. = FALSE)
  }
  prefixes[names(labels)] <- labels
  prefixes
}

#' Validate highlighted lines and compute their segments
#'
#' @param highlight `NULL`, or a list with any of `age`, `period`, `cohort`
#'   (numeric values) and optional `col` and `lwd`.
#' @param x A `hexamap_layout` object.
#' @return `NULL`, or a list with `isolines` (as in the layout), `col` and
#'   `lwd`.
#' @noRd
resolve_highlight <- function(highlight, x) {
  if (is.null(highlight)) {
    return(NULL)
  }
  types <- c("age", "period", "cohort")
  if (!is.list(highlight) || is.null(names(highlight)) ||
      !all(names(highlight) %in% c(types, "col", "lwd")) ||
      !any(names(highlight) %in% types)) {
    stop("`highlight` must be a list with any of `age`, `period` and ",
         "`cohort`, plus optional `col` and `lwd`, e.g. ",
         "list(cohort = 1975).", call. = FALSE)
  }
  d <- x$dims
  ranges <- list(age = c(d$first_age, d$last_age),
                 period = c(d$first_period, d$last_period),
                 cohort = c(d$first_period - d$last_age, d$last_period - d$first_age))
  isolines <- lapply(stats::setNames(types, types), function(type) {
    values <- highlight[[type]]
    if (is.null(values)) {
      values <- numeric(0)
    }
    if (!is.numeric(values) || anyNA(values) ||
        any(values < ranges[[type]][1] | values > ranges[[type]][2])) {
      stop("Highlighted ", type, "s must be numbers between ",
           ranges[[type]][1], " and ", ranges[[type]][2], ".", call. = FALSE)
    }
    isoline_segments(type, sort(values), d, x$wrap_cohort_labels, x$tiles)
  })
  col <- if (is.null(highlight$col)) "red" else highlight$col
  lwd <- if (is.null(highlight$lwd)) 1.5 else highlight$lwd
  check_number(lwd, "highlight$lwd", positive = TRUE)
  if (length(col) != 1) {
    stop("`highlight$col` must be a single color.", call. = FALSE)
  }
  list(isolines = isolines, col = col, lwd = lwd)
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
  angle <- isoline_angles(x$tiles) * pi / 180

  age_w <- width(labels$age)
  cohort_w <- width(labels$cohort)
  # age labels point right (and down on hexamaps); cohort labels point down
  # to the left; period labels point up
  left <- cohort_w * cos(angle[["cohort"]]) + char_height + pad
  right <- age_w * cos(angle[["age"]]) + char_height + pad
  bottom <- max(age_w * abs(sin(angle[["age"]])),
                cohort_w * sin(angle[["cohort"]])) + char_height + pad
  top <- width(labels$period) + pad

  space <- list(margins = c(left = left, right = right, bottom = bottom, top = top))
  if (legend) {
    gap <- 0.2
    tick <- 0.05
    percent_w <- if (length(ticks$percent)) width(ticks$percent) + tick + pad else 0
    bar_start <- right + gap + percent_w
    block <- percent_w + colorbar_width + tick + pad + width(c(ticks$value, ticks$na))
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

draw_tiles <- function(x, na_color, hex_border, hex_border_width) {
  colors <- x$cells$color
  colors[is.na(colors)] <- na_color
  drawn <- !is.na(colors)
  border <- if (identical(hex_border, "fill")) colors[drawn] else hex_border
  tile_x <- outer(x$cells$x[drawn], x$tile$x, "+")
  tile_y <- outer(x$cells$y[drawn], x$tile$y, "+")
  # one polygon() call, with tiles separated by NA
  polygon(x = as.vector(t(cbind(tile_x, NA))),
          y = as.vector(t(cbind(tile_y, NA))),
          col = colors[drawn], border = border, lwd = hex_border_width)
}

draw_isolines <- function(iso, labels, tiles, line_color, line_width,
                          label_color, label_size, label_font = 1) {
  # labels go at the end of age and period isolines, at the start of cohort ones
  angle <- isoline_angles(tiles)
  label_at <- list(age = list(end = 2, srt = angle[["age"]], adj = c(0, 0.5)),
                   period = list(end = 2, srt = angle[["period"]], adj = c(0, 0.5)),
                   cohort = list(end = 1, srt = angle[["cohort"]], adj = c(1, 0.5)))
  for (type in names(label_at)) {
    d <- iso[[type]]
    if (NROW(d) == 0) { # text() fails on zero-length labels
      next
    }
    graphics::segments(d$x1, d$y1, d$x2, d$y2, col = line_color, lwd = line_width)
    pos <- label_at[[type]]
    text(x = d[[paste0("x", pos$end)]], y = d[[paste0("y", pos$end)]],
         labels = labels[[type]], col = label_color, cex = label_size,
         font = label_font, srt = pos$srt, adj = pos$adj)
  }
}

draw_colorbar <- function(x, ticks, space, colorbar_width,
                          colorbar_height, scale_units, line_width,
                          label_color, label_size, na_color) {
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

  # missing-value key below the colorbar
  if (length(ticks$na)) {
    key_y2 <- cb_y1 - 0.15 * upi
    key_y1 <- key_y2 - colorbar_width * upi
    rect(xleft = cb_x1, ybottom = key_y1, xright = cb_x2, ytop = key_y2,
         col = na_color, border = NA)
    text(x = cb_x2 + tick + 0.05 * upi, y = (key_y1 + key_y2) / 2,
         labels = ticks$na, col = label_color, cex = label_size, adj = c(0, 0.5))
  }
}
