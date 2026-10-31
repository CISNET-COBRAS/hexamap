#' Plot a traditional Lexis diagram
#'
#' Draws the same data as [hexamap()] on square tiles, with periods on the x
#' axis, ages on the y axis and cohorts on the 45 degree diagonals. Use it
#' to compare a hexamap with the traditional display. In a Lexis diagram,
#' patterns between adjacent cohorts are compressed by about 30% and
#' patterns within a cohort are stretched by about 41% relative to age and
#' period, and adjacent tiles along a cohort touch only at a corner (Jalal
#' and Burke 2020).
#'
#' @param data Numeric matrix (or data frame of numeric columns) with ages as
#'   rows and periods as columns.
#' @param ... Further arguments passed to [hexamap()], such as `first_age`,
#'   `first_period`, `interval`, `colorbar_range` or `title_text`.
#' @return The `hexamap_layout` object returned by [hexamap_layout()] with
#'   square tiles, invisibly.
#' @references Jalal H, Burke DS. Hexamaps for age-period-cohort data
#'   visualization and implementation in R. *Epidemiology*.
#'   2020;31(6):e47-e49. \doi{10.1097/EDE.0000000000001236}
#' @export
#' @examples
#' rates <- simulate_apc(ages = 15:70, periods = 1999:2018,
#'                       cohort_effect = function(c) dnorm(c, 1975, 4) * 20)
#' op <- par(mfrow = c(1, 2))
#' lexis_map(rates, title_text = "Lexis diagram", legend = FALSE)
#' hexamap(rates, title_text = "Hexamap")
#' par(op)
lexis_map <- function(data, ...) {
  hexamap(data, ..., tiles = "square")
}
