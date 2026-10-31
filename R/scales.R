#' Color scale helpers
#'
#' Internal helpers that map data values to palette colors and compute the
#' values shown along the colorbar.
#'
#' @param scale Color scale name. Accepts `"linear"`, `"log"` and
#'   `"quantile"`, plus the legacy names `"Normal"` and `"Percentile"`, in
#'   any letter case.
#' @param limits Numeric vector of length 2 with the lower and upper limits
#'   of the scale, or `NULL` to use the range of `values`.
#' @param values Finite data values.
#' @param n Number of colors in the palette.
#' @param fractions Positions along the colorbar, from 0 (bottom) to 1 (top).
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
#' @return `resolve_limits()` returns the lower and upper limits of the scale.
#' @noRd
resolve_limits <- function(limits, values, scale) {
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
    return(limits)
  }
  if (!is.numeric(limits) || length(limits) != 2 || any(!is.finite(limits)) ||
      limits[1] > limits[2]) {
    stop("`colorbar_range` must be two increasing finite numbers.",
         call. = FALSE)
  }
  if (scale == "log" && limits[1] <= 0) {
    stop("`colorbar_range` must be positive for a log color scale.",
         call. = FALSE)
  }
  limits
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
#' @return `colorbar_tick_values()` returns the data value at each position
#'   in `fractions`.
#' @noRd
colorbar_tick_values <- function(values, limits, scale, fractions) {
  switch(scale,
    linear = limits[1] + fractions * (limits[2] - limits[1]),
    log = exp(log(limits[1]) + fractions * (log(limits[2]) - log(limits[1]))),
    quantile = stats::quantile(values, probs = fractions, names = FALSE)
  )
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
