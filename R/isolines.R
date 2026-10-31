#' Age, period and cohort isoline segments
#'
#' @param dims Output of `resolve_dims()`.
#' @param types Isoline types to compute; the others get no rows.
#' @inheritParams hexamap
#' @return A list of data frames `age`, `period` and `cohort` with the
#'   isoline `value` and the segment ends `x1`, `y1`, `x2`, `y2`. Labels go
#'   at the end of age and period isolines and at the start of cohort
#'   isolines.
#' @noRd
compute_isolines <- function(dims, first_age_isoline, first_period_isoline,
                             isoline_interval, wrap_cohort_labels,
                             types = c("age", "period", "cohort")) {
  age_isolines <- seq(from = first_age_isoline, to = dims$last_age, by = isoline_interval)
  period_isolines <- seq(from = first_period_isoline, to = dims$last_period, by = isoline_interval)
  # cohort isolines go through the crossings of the age and period isolines
  first_cohort_isoline <- first_period_isoline - tail(age_isolines, 1)
  cohort_isolines <- seq(from = first_cohort_isoline,
                         to = dims$last_period - dims$first_age,
                         by = isoline_interval)
  values <- list(age = age_isolines, period = period_isolines,
                 cohort = cohort_isolines)
  for (type in setdiff(names(values), types)) {
    values[[type]] <- numeric(0)
  }
  lapply(stats::setNames(names(values), names(values)), function(type) {
    isoline_segments(type, values[[type]], dims, wrap_cohort_labels)
  })
}

#' @rdname compute_isolines
#' @param type One of `"age"`, `"period"` or `"cohort"`.
#' @param values Ages, periods or cohorts at which to draw isolines, within
#'   the range of the data.
#' @noRd
isoline_segments <- function(type, values, dims, wrap_cohort_labels) {
  first_age <- dims$first_age
  last_age <- dims$last_age
  first_period <- dims$first_period
  last_period <- dims$last_period
  interval <- dims$interval

  if (type == "age") {
    return(data.frame(value = values,
                      x1 = rep(compute_xcoordinate(first_period), length(values)),
                      y1 = compute_ycoordinate(first_period, values),
                      x2 = rep(compute_xcoordinate(last_period + interval), length(values)),
                      y2 = compute_ycoordinate(last_period + interval, values)))
  }
  if (type == "period") {
    return(data.frame(value = values,
                      x1 = compute_xcoordinate(values),
                      y1 = compute_ycoordinate(values, first_age),
                      x2 = compute_xcoordinate(values),
                      y2 = compute_ycoordinate(values, last_age + interval)))
  }
  ends <- cohort_isoline_endpoints(values, first_age, last_age,
                                   first_period, last_period,
                                   wrap = wrap_cohort_labels)
  data.frame(value = values,
             x1 = compute_xcoordinate(ends$p1 - interval),
             y1 = compute_ycoordinate(ends$p1 - interval, ends$a1 - interval),
             x2 = compute_xcoordinate(ends$p2),
             y2 = compute_ycoordinate(ends$p2, ends$a2))
}

#' Default isoline spacing and starting points
#'
#' `nice_isoline_interval()` picks the smallest round spacing (1, 2, 5, 10,
#' 20, 25, 50, ...) that is a multiple of the data `interval` and gives at
#' most 8 isolines across the longer of the age and period ranges.
#' `round_isoline_start()` picks the first value of the data grid that is a
#' multiple of that spacing, so isolines fall on round ages and years.
#'
#' @param dims Output of `resolve_dims()`.
#' @param first,last Range of the ages or periods.
#' @param step Isoline spacing.
#' @param interval Data interval.
#' @noRd
nice_isoline_interval <- function(dims) {
  span <- max(dims$last_age - dims$first_age, dims$last_period - dims$first_period)
  candidates <- c(1, 2, 2.5, 5) * rep(10^(-2:4), each = 4)
  tol <- sqrt(.Machine$double.eps)
  is_multiple <- abs(candidates / dims$interval - round(candidates / dims$interval)) < tol &
    candidates >= dims$interval - tol
  candidates <- candidates[is_multiple]
  fits <- candidates[span / candidates <= 8]
  if (length(fits) > 0) min(fits) else max(candidates)
}

#' @rdname nice_isoline_interval
#' @noRd
round_isoline_start <- function(first, last, step, interval) {
  grid <- seq(from = first, to = max(first, min(last, first + step - interval)),
              by = interval)
  tol <- sqrt(.Machine$double.eps) * max(1, abs(first))
  round_values <- grid[abs(grid / step - round(grid / step)) * step < tol]
  if (length(round_values) > 0) round_values[1] else first
}

#' Resolve which isolines to draw
#'
#' @param isolines Character vector with any of `"age"`, `"period"` and
#'   `"cohort"`, or `NULL` / `character(0)` for none.
#' @return The validated character vector.
#' @noRd
resolve_isoline_types <- function(isolines) {
  if (length(isolines) == 0) {
    return(character(0))
  }
  types <- c("age", "period", "cohort")
  if (!is.character(isolines) || !all(isolines %in% types)) {
    stop("`isolines` must contain only \"age\", \"period\" and \"cohort\".",
         call. = FALSE)
  }
  unique(isolines)
}

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
