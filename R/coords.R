#' Convert age and period to plot coordinates
#'
#' Maps age-period pairs onto the Cartesian coordinates used by [hexamap()],
#' with \eqn{x = p\sqrt{3}/2} and \eqn{y = a - p/2}, or by [lexis_map()],
#' with \eqn{x = p} and \eqn{y = a}. Use it to add points, lines or text to
#' an existing plot.
#'
#' @param age Numeric vector of ages.
#' @param period Numeric vector of periods, recycled to the length of `age`.
#' @param tiles `"hexagon"` for hexamaps or `"square"` for Lexis diagrams.
#' @return A list with numeric vectors `x` and `y`, which can be passed
#'   directly to [graphics::points()] or [graphics::text()].
#' @export
#' @examples
#' rates <- outer(15:70, 1999:2018, function(a, p) (a - 10) * (p - 1990) / 40)
#' hexamap(rates, first_age = 15, first_period = 1999, interval = 1)
#' # mark the 1975 birth cohort at age 40 (period 2015)
#' graphics::points(apc_to_xy(age = 40, period = 2015), pch = 19)
apc_to_xy <- function(age, period, tiles = c("hexagon", "square")) {
  tiles <- match.arg(tiles)
  if (!is.numeric(age) || !is.numeric(period)) {
    stop("`age` and `period` must be numeric.", call. = FALSE)
  }
  apc_xy(period, age, tiles)
}

#' Convert period-age coordinates to Cartesian coordinates
#'
#' @param p Period.
#' @param a Age.
#' @param tiles `"hexagon"` or `"square"`.
#' @return `apc_xy()` returns a list with `x` and `y`;
#'   `compute_xcoordinate()` and `compute_ycoordinate()` return the hexamap
#'   x or y coordinates.
#' @noRd
apc_xy <- function(p, a, tiles = "hexagon") {
  # adding 0 * a (or 0 * p) recycles both to a common length, including 0
  if (tiles == "square") {
    return(list(x = p + 0 * a, y = a + 0 * p))
  }
  list(x = compute_xcoordinate(p) + 0 * a, y = compute_ycoordinate(p, a))
}

compute_xcoordinate <- function(p) {
  x <- p * sqrt(3) / 2
  return(x)
}

compute_ycoordinate <- function(p, a) {
  y <- a - p / 2
  return(y)
}

#' Vertices of a tile centered at the origin
#'
#' Flat-top hexagons have circumradius `interval / sqrt(3)`, so neighboring
#' centers are `interval` apart; squares have side `interval`.
#'
#' @param interval Distance between neighboring tile centers.
#' @param tiles `"hexagon"` or `"square"`.
#' @return A list with the `x` and `y` offsets of the vertices, with the
#'   first vertex repeated to close the polygon.
#' @noRd
tile_vertices <- function(interval, tiles = "hexagon") {
  if (tiles == "square") {
    h <- interval / 2
    return(list(x = c(-h, h, h, -h, -h), y = c(-h, -h, h, h, -h)))
  }
  a <- interval / sqrt(3) # radius of the hexagon (distance from center to a vertex)
  b <- sqrt(3) * a / 2    # half height of the hexagon
  list(x = c(-a, -a/2, a/2, a, a/2, -a/2, -a),
       y = c(0, b, b, 0, -b, -b, 0))
}

#' Angles of the isolines and their labels, in degrees
#'
#' @param tiles `"hexagon"` or `"square"`.
#' @return A named numeric vector with the angle of the `age`, `period` and
#'   `cohort` isolines.
#' @noRd
isoline_angles <- function(tiles = "hexagon") {
  if (tiles == "square") {
    return(c(age = 0, period = 90, cohort = 45))
  }
  c(age = -30, period = 90, cohort = 30)
}
