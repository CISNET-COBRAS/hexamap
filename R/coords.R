#' Convert age and period to hexamap coordinates
#'
#' Maps age-period pairs onto the Cartesian coordinates used by [hexamap()],
#' with \eqn{x = p\sqrt{3}/2} and \eqn{y = a - p/2}. Use it to add points,
#' lines or text to an existing hexamap.
#'
#' @param age Numeric vector of ages.
#' @param period Numeric vector of periods, recycled to the length of `age`.
#' @return A list with numeric vectors `x` and `y`, which can be passed
#'   directly to [graphics::points()] or [graphics::text()].
#' @export
#' @examples
#' rates <- outer(15:70, 1999:2018, function(a, p) (a - 10) * (p - 1990) / 40)
#' hexamap(rates, first_age = 15, first_period = 1999, interval = 1)
#' # mark the 1975 birth cohort at age 40 (period 2015)
#' graphics::points(apc_to_xy(age = 40, period = 2015), pch = 19)
apc_to_xy <- function(age, period) {
  if (!is.numeric(age) || !is.numeric(period)) {
    stop("`age` and `period` must be numeric.", call. = FALSE)
  }
  list(x = compute_xcoordinate(period), y = compute_ycoordinate(period, age))
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

compute_ycoordinate <- function(p, a) {
  y <- a - p / 2
  return(y)
}

#' Vertices of a flat-top hexagon centered at the origin
#'
#' @param interval Distance between neighboring hexagon centers.
#' @return A list with the `x` and `y` offsets of the 6 vertices, with the
#'   first vertex repeated to close the polygon.
#' @noRd
hex_vertices <- function(interval) {
  a <- interval / sqrt(3) # radius of the hexagon (distance from center to a vertex)
  b <- sqrt(3) * a / 2    # half height of the hexagon
  list(x = c(-a, -a/2, a/2, a, a/2, -a/2, -a),
       y = c(0, b, b, 0, -b, -b, 0))
}
