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
#'   which to draw an isoline. Default to the first age and period in the
#'   data that are multiples of `isoline_interval` (e.g. 20 and 2000 for data
#'   starting at age 15 in 1999 with isolines every 10 years).
#' @param isoline_interval Spacing between isolines. Defaults to the
#'   smallest round value (1, 2, 5, 10, 20, 25, 50, ...) that is a multiple
#'   of `interval` and gives at most 8 isolines.
#' @param isolines Which isolines to draw: any of `"age"`, `"period"` and
#'   `"cohort"`, or `NULL` for none.
#' @param colorbar_range Numeric vector of length 2 with the lower and upper
#'   limits of the color scale. Values outside are truncated. Defaults to the
#'   range of `data`.
#' @param colorbar_scale How colors are mapped to values: `"linear"`, `"log"`
#'   or `"quantile"` (each color covers the same share of cells). The names
#'   `"Normal"` and `"Percentile"` used in earlier versions are also accepted.
#' @param color_map Vector of colors from low to high. Overrides `palette`.
#' @param palette Name of a palette passed to [hexamap_palette()], used when
#'   `color_map` is `NULL`. Defaults to `"jet"`, the palette of Jalal and
#'   Burke (2020), or to the diverging `"Blue-Red 3"` when `midpoint` is
#'   set. `"viridis"` is a perceptually uniform alternative that is readable
#'   by people with color vision deficiencies.
#' @param n_colors Number of colors taken from `palette`.
#' @param midpoint Value at the center of a diverging color scale, such as 1
#'   for rate ratios on a log scale or 0 for differences. The color limits
#'   are made symmetric around it.
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
#'
#' # a diverging scale for rate ratios relative to 2008
#' ratios <- rates / rates[, "2008"]
#' hexamap(ratios, colorbar_scale = "log", midpoint = 1,
#'         scale_units = "Rate ratio")
#'
#' # period isolines only, labelled by year, with the 1975 cohort highlighted
#' hexamap(rates, isolines = "period", labels = c(period = ""),
#'         highlight = list(cohort = 1975))
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
                    title_adj = 0.5,
                    color_map = NULL,
                    palette = NULL,
                    n_colors = 100,
                    midpoint = NULL,
                    isolines = c("age", "period", "cohort"),
                    labels = c(age = "A: ", period = "P: ", cohort = "C: "),
                    highlight = NULL,
                    line_width = .5,
                    line_color = "grey",
                    label_size = .7,
                    label_color = "black",
                    scale_units = "Rate",
                    legend = TRUE,
                    na_color = "grey90",
                    na_label = "Missing",
                    hex_border = "fill",
                    hex_border_width = 0.5,
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
                           palette = palette,
                           n_colors = n_colors,
                           midpoint = midpoint,
                           isolines = isolines,
                           wrap_cohort_labels = wrap_cohort_labels)
  plot(layout,
       title_text = title_text,
       subtitle_text = subtitle_text,
       title_adj = title_adj,
       labels = labels,
       highlight = highlight,
       line_width = line_width,
       line_color = line_color,
       label_size = label_size,
       label_color = label_color,
       scale_units = scale_units,
       legend = legend,
       colorbar_width = colorbar_width,
       colorbar_height = colorbar_height,
       na_color = na_color,
       na_label = na_label,
       hex_border = hex_border,
       hex_border_width = hex_border_width)
}
