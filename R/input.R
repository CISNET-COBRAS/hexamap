#' Convert long age-period data to a matrix
#'
#' Reshapes a data frame with one row per age-period combination, such as an
#' export from CDC WONDER or SEER, into the age-by-period matrix used by
#' [hexamap()]. Combinations missing from `data` become `NA`.
#'
#' @param data A data frame.
#' @param age,period,value Names of the columns with the ages, periods and
#'   values (for example, rates).
#' @return A numeric matrix with one row per age and one column per period,
#'   on a regular grid from the first to the last age and period, with the
#'   ages and periods as dimnames.
#' @export
#' @examples
#' long <- expand.grid(age = seq(20, 80, 10), period = seq(1990, 2020, 10))
#' long$rate <- with(long, age * (period - 1980) / 100)
#' as_apc_matrix(long, value = "rate")
as_apc_matrix <- function(data, age = "age", period = "period", value = "value") {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  for (col in c(age, period, value)) {
    if (!is.character(col) || length(col) != 1 || !col %in% names(data)) {
      stop("Column `", col, "` not found in `data`.", call. = FALSE)
    }
  }
  a <- data[[age]]
  p <- data[[period]]
  v <- data[[value]]
  if (!is.numeric(a) || !is.numeric(p) || anyNA(a) || anyNA(p)) {
    stop("Columns `", age, "` and `", period, "` must be numeric without ",
         "missing values.", call. = FALSE)
  }
  if (!is.numeric(v)) {
    stop("Column `", value, "` must be numeric. Convert flags such as ",
         "\"Unreliable\" or \"Suppressed\" to NA first.", call. = FALSE)
  }
  if (anyDuplicated(data.frame(a, p))) {
    stop("`data` has more than one row for some age-period combinations.",
         call. = FALSE)
  }

  ages <- regular_grid(a, age)
  periods <- regular_grid(p, period)
  out <- matrix(NA_real_, nrow = length(ages), ncol = length(periods),
                dimnames = list(ages, periods))
  out[cbind(match_grid(a, ages), match_grid(p, periods))] <- v
  out
}

#' Compute rates from counts and population
#'
#' @param counts Numeric matrix (or data frame of numeric columns) of event
#'   counts, with ages as rows and periods as columns.
#' @param population Matrix of the same dimensions with the population at
#'   risk.
#' @param per Rates are expressed per `per` persons.
#' @return A numeric matrix of rates per `per`, with the dimnames of `counts`
#'   (or of `population`). Cells with zero or missing population are `NA`.
#' @export
#' @examples
#' deaths <- matrix(c(5, 12, 30, 8, 20, 41), nrow = 3,
#'                  dimnames = list(c(20, 30, 40), c(2000, 2001)))
#' pop <- matrix(c(9e4, 1e5, 1.1e5, 9e4, 1e5, 0), nrow = 3)
#' apc_rates(deaths, pop)
apc_rates <- function(counts, population, per = 1e5) {
  counts <- check_apc_matrix(counts)
  population <- check_apc_matrix(population)
  check_number(per, "per", positive = TRUE)
  if (!identical(dim(counts), dim(population))) {
    stop("`counts` and `population` must have the same dimensions.",
         call. = FALSE)
  }
  rates <- counts / population * per
  rates[is.na(population) | population <= 0] <- NA
  dimnames(rates) <- if (is.null(dimnames(counts))) dimnames(population) else dimnames(counts)
  rates
}

#' Regular grid covering a set of ages or periods
#'
#' @param x Numeric values.
#' @param arg Column name for error messages.
#' @return A sequence from `min(x)` to `max(x)` by the smallest difference
#'   between unique values.
#' @noRd
regular_grid <- function(x, arg) {
  values <- sort(unique(x))
  if (length(values) == 1) {
    return(values)
  }
  step <- min(diff(values))
  grid <- seq(from = values[1], to = values[length(values)], by = step)
  if (anyNA(match_grid(values, grid))) {
    stop("Column `", arg, "` must be equally spaced (gaps are allowed).",
         call. = FALSE)
  }
  grid
}

match_grid <- function(x, grid) {
  tol <- sqrt(.Machine$double.eps) * max(1, abs(grid))
  vapply(x, function(v) {
    i <- which(abs(grid - v) < tol)
    if (length(i)) i[1] else NA_integer_
  }, integer(1))
}
