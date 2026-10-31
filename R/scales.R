#' Color palettes for hexamaps
#'
#' Returns `n` colors from a named palette. The default, `"jet"`, is the
#' palette used in Jalal and Burke (2020). Any palette from
#' [grDevices::hcl.pals()] can also be used, such as `"viridis"` (perceptually
#' uniform and readable by people with color vision deficiencies),
#' `"Inferno"`, `"YlOrRd"`, or the diverging `"Blue-Red 3"`.
#'
#' @param name Name of the palette: `"jet"` or a name from
#'   [grDevices::hcl.pals()]. Matching ignores case and spaces.
#' @param n Number of colors.
#' @return A character vector of `n` colors, from low to high.
#' @export
#' @examples
#' hexamap_palette("jet", 5)
#' hexamap_palette("viridis", 5)
#' hexamap_palette("Blue-Red 3", 5)
hexamap_palette <- function(name = "jet", n = 100) {
  check_number(n, "n", positive = TRUE)
  if (n != round(n)) {
    stop("`n` must be a whole number.", call. = FALSE)
  }
  if (!is.character(name) || length(name) != 1 || is.na(name)) {
    stop("`palette` must be a single palette name.", call. = FALSE)
  }
  if (tolower(name) == "jet") {
    jet_colors <- colorRampPalette(c("black", "#00007F", "blue", "#007FFF", "cyan",
                                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
    return(jet_colors(n))
  }
  colors <- tryCatch(grDevices::hcl.colors(n, palette = name),
                     error = function(e) NULL)
  if (is.null(colors)) {
    stop("Unknown palette \"", name, "\". Use \"jet\" or a name from ",
         "grDevices::hcl.pals().", call. = FALSE)
  }
  colors
}

#' Color scale helpers
#'
#' Internal helpers that map data values to palette colors and compute the
#' ticks shown along the colorbar.
#'
#' @param scale Color scale name. Accepts `"linear"`, `"log"` and
#'   `"quantile"`, plus the legacy names `"Normal"` and `"Percentile"`, in
#'   any letter case.
#' @param limits Numeric vector of length 2 with the lower and upper limits
#'   of the scale, or `NULL` to use the range of `values`.
#' @param values Finite data values.
#' @param midpoint Value at the center of a diverging scale, or `NULL`.
#' @param n Number of colors in the palette.
#' @noRd
resolve_scale <- function(scale) {
  if (!is.character(scale) || length(scale) != 1) {
    stop("`colorbar_scale` must be one of \"linear\", \"log\" or ",
         "\"quantile\".", call. = FALSE)
  }
  scales <- c(normal = "linear", linear = "linear", log = "log",
              percentile = "quantile", quantile = "quantile")
  resolved <- scales[tolower(scale)]
  if (is.na(resolved)) {
    stop("`colorbar_scale` must be one of \"linear\", \"log\" or ",
         "\"quantile\", not \"", scale, "\".", call. = FALSE)
  }
  unname(resolved)
}

#' @rdname resolve_scale
#' @return `resolve_limits()` returns the lower and upper limits of the
#'   scale. With a `midpoint`, the limits are widened to be symmetric around
#'   it (on the log scale for log scales), so equal distances from the
#'   midpoint get equally strong colors.
#' @noRd
resolve_limits <- function(limits, values, scale, midpoint = NULL) {
  if (is.null(limits)) {
    limits <- range(values)
    if (scale == "log" && limits[1] <= 0) {
      positive <- values[values > 0]
      if (length(positive) == 0) {
        stop("A log color scale needs positive values in `data`.",
             call. = FALSE)
      }
      warning("`data` has values <= 0; they are drawn in the lowest color ",
              "of the log scale.", call. = FALSE)
      limits[1] <- min(positive)
    }
  } else {
    if (!is.numeric(limits) || length(limits) != 2 || any(!is.finite(limits)) ||
        limits[1] > limits[2]) {
      stop("`colorbar_range` must be two increasing finite numbers.",
           call. = FALSE)
    }
    if (scale == "log" && limits[1] <= 0) {
      stop("`colorbar_range` must be positive for a log color scale.",
           call. = FALSE)
    }
  }
  if (is.null(midpoint)) {
    return(limits)
  }

  check_number(midpoint, "midpoint")
  if (scale == "quantile") {
    stop("`midpoint` cannot be used with a quantile color scale.",
         call. = FALSE)
  }
  if (scale == "log") {
    if (midpoint <= 0) {
      stop("`midpoint` must be positive for a log color scale.", call. = FALSE)
    }
    half <- max(abs(log(limits) - log(midpoint)))
    return(midpoint * exp(c(-half, half)))
  }
  half <- max(abs(limits - midpoint))
  midpoint + c(-half, half)
}

#' @rdname resolve_scale
#' @return `compute_color_index()` returns, for each value, the index of its
#'   color in a palette of `n` colors (an integer between 1 and `n`).
#' @noRd
compute_color_index <- function(values, limits, scale, n) {
  breaks <- switch(scale,
    linear = seq(from = limits[1], to = limits[2], length.out = n + 1),
    log = exp(seq(from = log(limits[1]), to = log(limits[2]),
                  length.out = n + 1)),
    quantile = stats::quantile(values, probs = seq(0, 1, length.out = n + 1),
                               names = FALSE)
  )
  if (breaks[1] == breaks[n + 1]) { # constant data: use the middle color
    return(rep(ceiling(n / 2), length(values)))
  }
  findInterval(values, breaks, rightmost.closed = TRUE, all.inside = TRUE)
}

#' @rdname resolve_scale
#' @return `colorbar_ticks()` returns a data frame with the tick `value`s and
#'   their `fraction` of the colorbar height, from 0 (bottom) to 1 (top).
#'   Linear and log scales get round values within the limits (and the
#'   midpoint, if any); quantile scales get every 20th percentile.
#' @noRd
colorbar_ticks <- function(values, limits, scale, midpoint = NULL) {
  if (limits[1] == limits[2]) {
    return(data.frame(fraction = 0.5, value = limits[1]))
  }
  if (scale == "quantile") {
    fraction <- seq(0, 1, by = 0.2)
    return(data.frame(fraction = fraction,
                      value = stats::quantile(values, probs = fraction,
                                              names = FALSE)))
  }
  if (scale == "log") {
    value <- grDevices::axisTicks(log10(limits), log = TRUE, nint = 5)
    to_fraction <- function(v) (log(v) - log(limits[1])) / diff(log(limits))
  } else {
    value <- pretty(limits, n = 5)
    to_fraction <- function(v) (v - limits[1]) / diff(limits)
  }
  value <- sort(unique(c(value, midpoint)))
  tol <- sqrt(.Machine$double.eps)
  fraction <- to_fraction(value)
  keep <- fraction >= -tol & fraction <= 1 + tol
  data.frame(fraction = pmin(pmax(fraction[keep], 0), 1), value = value[keep])
}

#' @rdname resolve_scale
#' @param x Numeric values to label.
#' @return `format_tick_labels()` returns `x` rounded to 3 significant digits
#'   as character labels.
#' @noRd
format_tick_labels <- function(x) {
  vapply(signif(x, 3), format, character(1),
         big.mark = ",", scientific = FALSE, drop0trailing = TRUE, trim = TRUE)
}
