#' Create a hexamap (eAppendix interface)
#'
#' Wrapper around [hexamap()] that keeps the argument names used in the
#' eAppendix of Jalal and Burke (2020), so published code runs unchanged.
#' It uses the "jet" palette of the paper by default. New code should call
#' [hexamap()] directly.
#'
#' @inheritParams hexamap
#' @param color_scale Numeric vector of length 2 with the lower and upper
#'   limits of the color scale. Passed to `colorbar_range` in [hexamap()].
#' @param palette Palette name passed to [hexamap()]. Defaults to `"jet"`,
#'   as in the paper.
#' @param ... Further arguments passed to [hexamap()].
#'
#' @return The `hexamap_layout` object returned by [hexamap_layout()],
#'   invisibly.
#' @export
#' @examples
#' rates <- outer(15:70, 1999:2018, function(a, p) (a - 10) * (p - 1990) / 40)
#' create_hexamap(data = rates,
#'                first_age = 15,
#'                first_period = 1999,
#'                interval = 1,
#'                first_age_isoline = 20,
#'                first_period_isoline = 2000,
#'                isoline_interval = 10,
#'                color_scale = c(0, 40))
create_hexamap <- function(data,
                           first_age,
                           first_period,
                           interval,
                           color_scale = NULL,
                           palette = "jet",
                           ...) {
  hexamap(data = data,
          first_age = first_age,
          first_period = first_period,
          interval = interval,
          colorbar_range = color_scale,
          palette = palette,
          ...)
}
