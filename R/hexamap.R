#' Plot a hexamap of age-period-cohort data
#'
#' Draws a field of hexagonal tiles, one per age-period cell, with age,
#' period and cohort isolines at equal 60 degree angles, plus a colorbar.
#' Each tile is centered at \eqn{x = p\sqrt{3}/2} and \eqn{y = a - p/2}, so
#' neighboring tiles are one `interval` apart along every APC axis.
#'
#' `hexamap()` is [hexamap_layout()] followed by
#' [plot()][plot.hexamap_layout()]. The hexamap is scaled to fit the current
#' plotting region, so several hexamaps can be combined with
#' `par(mfrow = )`. Use [apc_to_xy()] to add annotations.
#'
#' @param data Numeric matrix (or data frame of numeric columns) with ages as
#'   rows and periods as columns. Missing and non-finite cells are not drawn.
#' @param first_age First age in `data`. Defaults to the first row name of
#'   `data`, if numeric.
#' @param first_period First period in `data`. Defaults to the first column
#'   name of `data`, if numeric.
#' @param interval Width of the age and period groups, which must be equal.
#'   Defaults to the spacing of the numeric row or column names of `data`,
#'   or 1.
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
#' @param color_map Vector of colors from low to high. Defaults to a 100-color
#'   "jet" palette.
#' @param wrap_cohort_labels Logical; whether cohort labels wrap around the
#'   bottom edge of the hexamap.
#' @inheritParams plot.hexamap_layout
#' @param zoom_factor Deprecated and ignored: hexamaps now fit the plotting
#'   region.
#'
#' @return The `hexamap_layout` object returned by [hexamap_layout()],
#'   invisibly.
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
#'
#' # ages and periods can also come from the dimnames
#' dimnames(rates) <- list(ages, periods)
#' hexamap(rates, isoline_interval = 10, title_text = "Simulated rates")
hexamap <- function(data,
                    first_age = NULL,
                    first_period = NULL,
                    interval = NULL,
                    first_age_isoline = NULL,
                    first_period_isoline = NULL,
                    isoline_interval = NULL,
                    colorbar_range = NULL,
                    colorbar_scale = "linear",
                    colorbar_width = 0.15,
                    colorbar_height = 0.5,
                    title_text = NULL,
                    subtitle_text = NULL,
                    color_map = NULL,
                    line_width = .5,
                    line_color = "grey",
                    label_size = .5,
                    label_color = "black",
                    scale_units = "Rate",
                    legend = TRUE,
                    zoom_factor = NULL,
                    wrap_cohort_labels = TRUE) {
  if (!is.null(zoom_factor)) {
    warning("`zoom_factor` is deprecated and ignored: hexamaps now fit the ",
            "plotting region.", call. = FALSE)
  }
  layout <- hexamap_layout(data = data,
                           first_age = first_age,
                           first_period = first_period,
                           interval = interval,
                           first_age_isoline = first_age_isoline,
                           first_period_isoline = first_period_isoline,
                           isoline_interval = isoline_interval,
                           colorbar_range = colorbar_range,
                           colorbar_scale = colorbar_scale,
                           color_map = color_map,
                           wrap_cohort_labels = wrap_cohort_labels)
  plot(layout,
       title_text = title_text,
       subtitle_text = subtitle_text,
       line_width = line_width,
       line_color = line_color,
       label_size = label_size,
       label_color = label_color,
       scale_units = scale_units,
       legend = legend,
       colorbar_width = colorbar_width,
       colorbar_height = colorbar_height)
}
