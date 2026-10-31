#' Hexamaps with ggplot2
#'
#' `gg_hexamap()` draws a hexamap (or a Lexis diagram, with
#' `tiles = "square"`) as a ggplot object, which can be modified with the
#' usual ggplot2 functions such as [ggplot2::labs()] or [ggplot2::theme()].
#' Give a named list of matrices to draw one panel per element, sharing one
#' color scale. `autoplot()` does the same for a layout computed by
#' [hexamap_layout()]. Both need the 'ggplot2' package.
#'
#' @param data A numeric matrix with ages as rows and periods as columns, as
#'   in [hexamap()], or a named list of such matrices to draw as panels. The
#'   names label the panels.
#' @param ... Arguments passed to [hexamap_layout()], such as `first_age`,
#'   `first_period`, `interval`, `colorbar_range`, `colorbar_scale`,
#'   `palette`, `midpoint`, `isolines` or `tiles`.
#' @inheritParams plot.hexamap_layout
#' @param line_width Width of the isolines, in mm (as `linewidth` in
#'   ggplot2).
#' @param line_color Color of the isolines.
#' @param label_size Size of the isoline labels, in mm (as `size` in
#'   [ggplot2::geom_text()]).
#' @param label_color Color of the isoline labels.
#' @param ncol Number of columns of panels when `data` is a list.
#' @return A ggplot object.
#' @export
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   rates <- simulate_apc(15:70, 1999:2018,
#'                         age_effect = function(a) -((a - 45) / 15)^2,
#'                         cohort_effect = function(c) ifelse(c >= 1955 & c <= 1965, 0.7, 0))
#'   gg_hexamap(rates, highlight = list(cohort = 1960)) +
#'     ggplot2::labs(title = "Simulated cohort effect")
#'
#'   # one panel per group, with a shared color scale
#'   later <- simulate_apc(15:70, 1999:2018,
#'                         age_effect = function(a) -((a - 35) / 12)^2,
#'                         period_effect = function(p) 0.1 * pmax(p - 2010, 0))
#'   gg_hexamap(list(`Cohort effect` = rates, `Period effect` = later))
#'
#'   # autoplot() draws a precomputed layout, here as a Lexis diagram
#'   ggplot2::autoplot(hexamap_layout(rates, tiles = "square"))
#' }
gg_hexamap <- function(data,
                       ...,
                       labels = c(age = "A: ", period = "P: ", cohort = "C: "),
                       highlight = NULL,
                       line_color = "grey",
                       line_width = 0.25,
                       label_size = 2.5,
                       label_color = "black",
                       scale_units = "Rate",
                       na_color = "grey90",
                       ncol = NULL) {
  check_ggplot2()
  if (!is.list(data) || is.data.frame(data)) {
    layouts <- list(hexamap_layout(data, ...))
    panels <- NULL
  } else {
    if (length(data) == 0 || is.null(names(data)) || any(!nzchar(names(data)))) {
      stop("A list `data` must be named; the names label the panels.",
           call. = FALSE)
    }
    layouts <- shared_scale_layouts(data, ...)
    panels <- names(data)
  }
  build_gg_hexamap(layouts, panels, labels, highlight, line_color, line_width,
                   label_size, label_color, scale_units, na_color, ncol)
}

#' @rdname gg_hexamap
#' @param object A `hexamap_layout` object.
#' @exportS3Method ggplot2::autoplot
autoplot.hexamap_layout <- function(object,
                                    labels = c(age = "A: ", period = "P: ", cohort = "C: "),
                                    highlight = NULL,
                                    line_color = "grey",
                                    line_width = 0.25,
                                    label_size = 2.5,
                                    label_color = "black",
                                    scale_units = "Rate",
                                    na_color = "grey90",
                                    ...) {
  check_ggplot2()
  build_gg_hexamap(list(object), NULL, labels, highlight, line_color,
                   line_width, label_size, label_color, scale_units, na_color,
                   ncol = NULL)
}

check_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is needed for this function. Install it with ",
         "install.packages(\"ggplot2\"), or use hexamap() for base graphics.",
         call. = FALSE)
  }
}

#' Layouts for several panels with one color scale
#'
#' @param data Named list of matrices.
#' @param ... Arguments passed to [hexamap_layout()].
#' @return A list of `hexamap_layout` objects with common color limits.
#' @noRd
shared_scale_layouts <- function(data, ...) {
  args <- list(...)
  scale <- resolve_scale(if (is.null(args$colorbar_scale)) "linear" else args$colorbar_scale)
  if (scale == "quantile") {
    stop("A quantile color scale cannot be shared across panels; use a ",
         "linear or log scale.", call. = FALSE)
  }
  if (is.null(args$colorbar_range)) {
    limits <- lapply(data, function(d) hexamap_layout(d, ...)$scale$limits)
    args$colorbar_range <- range(unlist(limits))
  }
  lapply(data, function(d) do.call(hexamap_layout, c(list(d), args)))
}

#' Build the ggplot object for one or more layouts
#' @noRd
build_gg_hexamap <- function(layouts, panels, labels, highlight, line_color,
                             line_width, label_size, label_color, scale_units,
                             na_color, ncol) {
  check_number(label_size, "label_size", positive = TRUE)
  check_number(line_width, "line_width")
  if (length(na_color) != 1) {
    stop("`na_color` must be a single color.", call. = FALSE)
  }
  prefixes <- resolve_label_prefixes(labels)
  if (is.null(panels)) {
    panels <- ""
  }
  panel_levels <- panels

  tiles <- list()
  lines <- list()
  texts <- list()
  label_lengths <- list(age = 0, period = 0, cohort = 0)
  for (i in seq_along(layouts)) {
    lay <- layouts[[i]]
    panel <- panels[i]
    tiles[[i]] <- tile_frame(lay, panel, na_color)
    iso_labels <- isoline_labels(lay$isolines, prefixes)
    lines[[length(lines) + 1]] <- segment_frame(lay$isolines, panel, FALSE)
    texts[[length(texts) + 1]] <- text_frame(lay$isolines, iso_labels, lay$tiles, panel, FALSE)
    hl <- resolve_highlight(highlight, lay)
    if (!is.null(hl)) {
      hl_labels <- isoline_labels(hl$isolines, prefixes)
      lines[[length(lines) + 1]] <- segment_frame(hl$isolines, panel, TRUE)
      texts[[length(texts) + 1]] <- text_frame(hl$isolines, hl_labels, lay$tiles, panel, TRUE)
      iso_labels <- Map(c, iso_labels, hl_labels)
    }
    for (type in names(label_lengths)) {
      label_lengths[[type]] <- max(label_lengths[[type]], nchar(iso_labels[[type]]), 0)
    }
  }
  # panels keep the order of the list
  as_panels <- function(d) {
    if (!is.null(d)) {
      d$panel <- factor(d$panel, levels = panel_levels)
    }
    d
  }
  tiles <- as_panels(do.call(rbind, tiles))
  lines <- as_panels(do.call(rbind, lines))
  texts <- as_panels(do.call(rbind, texts))

  # room for the labels outside the panel, in mm (text size is in mm)
  char_mm <- 0.6 * label_size
  angle <- isoline_angles(layouts[[1]]$tiles) * pi / 180
  margin_mm <- c(top = label_lengths$period * char_mm,
                 right = label_lengths$age * char_mm * cos(angle[["age"]]),
                 bottom = max(label_lengths$age * char_mm * abs(sin(angle[["age"]])),
                              label_lengths$cohort * char_mm * sin(angle[["cohort"]])),
                 left = label_lengths$cohort * char_mm * cos(angle[["cohort"]])) +
    label_size

  sc <- layouts[[1]]$scale
  squish <- function(x, range = c(0, 1), only.finite = TRUE) pmin(pmax(x, range[1]), range[2])
  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = tiles,
                          ggplot2::aes(x = x, y = y, group = id, fill = fraction,
                                       colour = fraction),
                          linewidth = 0.1) +
    ggplot2::scale_fill_gradientn(name = scale_units, colours = sc$colors,
                                  limits = c(0, 1), breaks = sc$ticks$fraction,
                                  labels = format_tick_labels(sc$ticks$value),
                                  na.value = na_color, oob = squish) +
    ggplot2::scale_colour_gradientn(colours = sc$colors, limits = c(0, 1),
                                    na.value = na_color, oob = squish,
                                    guide = "none")
  if (!is.null(lines)) {
    regular <- lines[!lines$highlight, , drop = FALSE]
    special <- lines[lines$highlight, , drop = FALSE]
    p <- p +
      ggplot2::geom_segment(data = regular,
                            ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
                            colour = line_color, linewidth = line_width)
    if (nrow(special) > 0) {
      hl <- resolve_highlight(highlight, layouts[[1]])
      # highlight lwd uses base graphics units, like plot.hexamap_layout()
      p <- p +
        ggplot2::geom_segment(data = special,
                              ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
                              colour = hl$col, linewidth = hl$lwd / ggplot2::.pt)
    }
  }
  if (!is.null(texts)) {
    p <- p +
      ggplot2::geom_text(data = texts,
                         ggplot2::aes(x = x, y = y, label = label, angle = angle,
                                      hjust = hjust, fontface = fontface),
                         size = label_size, colour = label_color, vjust = 0.5)
  }
  # even a single panel is drawn as a facet: the strip (blank for one panel)
  # keeps titles added with labs() clear of the period labels
  p +
    ggplot2::facet_wrap(ggplot2::vars(panel), ncol = ncol) +
    ggplot2::coord_equal(expand = FALSE, clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(2, 2, margin_mm[["bottom"]], margin_mm[["left"]],
                                    unit = "mm"),
      legend.box.spacing = ggplot2::unit(margin_mm[["right"]] + 3, "mm"),
      panel.spacing.x = ggplot2::unit(margin_mm[["left"]] + margin_mm[["right"]], "mm"),
      panel.spacing.y = ggplot2::unit(margin_mm[["top"]] + margin_mm[["bottom"]], "mm"),
      strip.text = ggplot2::element_text(face = "bold",
                                         margin = ggplot2::margin(b = margin_mm[["top"]],
                                                                  unit = "mm"))
    )
}

tile_frame <- function(lay, panel, na_color) {
  cells <- lay$cells
  keep <- !is.na(cells$fraction) | !is.na(na_color)
  cells <- cells[keep, , drop = FALSE]
  k <- length(lay$tile$x)
  n <- nrow(cells)
  data.frame(id = paste(panel, rep(seq_len(n), each = k)),
             x = rep(cells$x, each = k) + rep(lay$tile$x, times = n),
             y = rep(cells$y, each = k) + rep(lay$tile$y, times = n),
             fraction = rep(cells$fraction, each = k),
             panel = panel)
}

segment_frame <- function(isolines, panel, highlight) {
  rows <- lapply(isolines, function(d) if (NROW(d) > 0) d[, c("x1", "y1", "x2", "y2")])
  d <- do.call(rbind, rows)
  if (is.null(d)) {
    return(NULL)
  }
  d$panel <- panel
  d$highlight <- highlight
  d
}

text_frame <- function(isolines, labels, tiles, panel, highlight) {
  angle <- isoline_angles(tiles)
  rows <- lapply(c("age", "period", "cohort"), function(type) {
    d <- isolines[[type]]
    if (NROW(d) == 0) {
      return(NULL)
    }
    # labels go at the end of age and period isolines, at the start of cohort ones
    end <- if (type == "cohort") "1" else "2"
    data.frame(x = d[[paste0("x", end)]], y = d[[paste0("y", end)]],
               label = labels[[type]], angle = angle[[type]],
               hjust = if (type == "cohort") 1 else 0)
  })
  d <- do.call(rbind, rows)
  if (is.null(d)) {
    return(NULL)
  }
  d$fontface <- if (highlight) "bold" else "plain"
  d$panel <- panel
  d
}

utils::globalVariables(c("x", "y", "id", "fraction", "x1", "y1", "x2", "y2",
                         "label", "angle", "hjust", "fontface", "panel"))
