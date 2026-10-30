#' Endpoints of the cohort isolines
#'
#' Each cohort isoline enters the hexamap through its left (first period) or
#' bottom (first age) edge and leaves through its top (last age) or right
#' (last period) edge. When `wrap` is `FALSE`, every line starts on the left
#' edge, extended below the first age if needed.
#'
#' @param cohort_isolines Cohorts at which isolines are drawn, in increasing
#'   order.
#' @param first_age,last_age,first_period,last_period Limits of the data.
#' @param wrap Logical; whether cohort lines (and labels) wrap around the
#'   bottom edge.
#' @return A list with the start (`p1`, `a1`) and end (`p2`, `a2`) of each
#'   isoline in period-age coordinates, in the order of `cohort_isolines`.
#' @noRd
cohort_isoline_endpoints <- function(cohort_isolines, first_age, last_age,
                                     first_period, last_period, wrap) {
  # periods where the cohort isolines cross the last age
  p_top <- cohort_isolines + last_age
  p_top <- p_top[p_top < last_period]
  # periods where they cross the first age
  p_bottom <- cohort_isolines + first_age
  p_bottom <- p_bottom[p_bottom > first_period]
  # ages where they cross the first period
  a_left <- first_period - cohort_isolines
  if (wrap) {
    a_left <- a_left[a_left >= first_age]
  }
  # ages where they cross the last period
  a_right <- last_period - cohort_isolines
  a_right <- a_right[a_right <= last_age]

  if (wrap) {
    p1 <- c(rep(first_period, length(a_left)), p_bottom)
    a1 <- c(a_left, rep(first_age, length(p_bottom)))
  } else {
    p1 <- rep(first_period, length(a_left))
    a1 <- a_left
  }
  p2 <- c(p_top, rep(last_period, length(a_right)))
  a2 <- c(rep(last_age, length(p_top)), a_right)
  list(p1 = p1, a1 = a1, p2 = p2, a2 = a2)
}
