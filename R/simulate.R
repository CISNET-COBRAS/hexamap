#' Simulate age-period-cohort rates
#'
#' Generates a matrix of rates from a log-linear age-period-cohort model,
#' \deqn{\log \mu_{ap} = \log(\mathrm{baseline}) + f(a) + g(p) + h(p - a),}
#' where \eqn{f}, \eqn{g} and \eqn{h} are the age, period and cohort effects
#' on the log scale (log rate ratios). Use it to see how each effect, alone
#' or combined, looks on a hexamap or a Lexis diagram. Optionally adds
#' Poisson noise for a given population size.
#'
#' Note that linear trends in age, period and cohort cannot be separated
#' (cohort = period - age): for example, `age_effect = function(a) 0.02 * a`
#' with `period_effect = function(p) 0.02 * p` gives the same rates as
#' `cohort_effect = function(c) 0.02 * c` with
#' `age_effect = function(a) 0.04 * a`. Visual displays reveal departures
#' from linearity, not the linear components.
#'
#' @param ages,periods Numeric vectors of ages and periods.
#' @param age_effect,period_effect,cohort_effect Effects on the log scale:
#'   `NULL` for none, a function of age, period or cohort (birth year,
#'   `period - age`) returning log rate ratios, or a numeric vector with one
#'   value per age, period or cohort. Cohort vectors are matched to
#'   `sort(unique(outer(periods, ages, "-")))`.
#' @param baseline Rate when all effects are 0, in units of `per`.
#' @param population `NULL` for expected rates without noise, or the
#'   population at risk in each cell (a number or an age-by-period matrix),
#'   used to draw Poisson counts.
#' @param per Rates are expressed per `per` persons.
#' @return A numeric matrix of rates per `per`, with ages as rows, periods as
#'   columns and the ages and periods as dimnames. With `population`, the
#'   simulated counts and population are attached as attributes `counts`
#'   and `population`.
#' @export
#' @examples
#' ages <- 15:70
#' periods <- 1999:2018
#' # a peak at age 45, a surge after 2010, and a high-risk 1955-1965 cohort
#' rates <- simulate_apc(ages, periods,
#'                       age_effect = function(a) -((a - 45) / 15)^2,
#'                       period_effect = function(p) 0.15 * pmax(p - 2010, 0),
#'                       cohort_effect = function(c) ifelse(c >= 1955 & c <= 1965, 0.7, 0))
#' hexamap(rates, scale_units = "Rate per 100,000")
#'
#' # with Poisson noise for 100,000 persons per cell
#' set.seed(1)
#' noisy <- simulate_apc(ages, periods, age_effect = function(a) -((a - 45) / 15)^2,
#'                       population = 1e5)
#' head(attr(noisy, "counts")[, 1:5])
simulate_apc <- function(ages,
                         periods,
                         age_effect = NULL,
                         period_effect = NULL,
                         cohort_effect = NULL,
                         baseline = 10,
                         population = NULL,
                         per = 1e5) {
  if (!is.numeric(ages) || !is.numeric(periods) || length(ages) < 1 ||
      length(periods) < 1 || any(!is.finite(c(ages, periods)))) {
    stop("`ages` and `periods` must be numeric vectors of finite values.",
         call. = FALSE)
  }
  check_number(baseline, "baseline", positive = TRUE)
  check_number(per, "per", positive = TRUE)

  age <- matrix(ages, nrow = length(ages), ncol = length(periods))
  period <- matrix(periods, nrow = length(ages), ncol = length(periods), byrow = TRUE)
  cohort <- period - age
  cohorts <- sort(unique(as.vector(cohort)))

  log_rate <- log(baseline) +
    evaluate_effect(age_effect, age, ages, "age_effect") +
    evaluate_effect(period_effect, period, periods, "period_effect") +
    evaluate_effect(cohort_effect, cohort, cohorts, "cohort_effect")
  rates <- exp(log_rate)
  dimnames(rates) <- list(ages, periods)

  if (is.null(population)) {
    return(rates)
  }
  if (!is.numeric(population) || any(!is.finite(population)) || any(population < 0) ||
      !(length(population) == 1 || identical(dim(population), dim(rates)))) {
    stop("`population` must be a non-negative number or a matrix with one ",
         "row per age and one column per period.", call. = FALSE)
  }
  population <- matrix(population, nrow = nrow(rates), ncol = ncol(rates),
                       dimnames = dimnames(rates))
  counts <- matrix(stats::rpois(length(rates), rates * population / per),
                   nrow = nrow(rates), dimnames = dimnames(rates))
  simulated <- ifelse(population > 0, counts / population * per, NA_real_)
  dimnames(simulated) <- dimnames(rates)
  attr(simulated, "counts") <- counts
  attr(simulated, "population") <- population
  simulated
}

#' Evaluate an APC effect on a grid
#'
#' @param effect `NULL`, a function, or a numeric vector with one value per
#'   element of `levels`.
#' @param grid Matrix of ages, periods or cohorts.
#' @param levels Sorted unique values the vector form refers to.
#' @param arg Argument name for error messages.
#' @return A matrix of log rate ratios with the dimensions of `grid`.
#' @noRd
evaluate_effect <- function(effect, grid, levels, arg) {
  if (is.null(effect)) {
    return(0 * grid)
  }
  if (is.function(effect)) {
    values <- effect(as.vector(grid))
    if (!is.numeric(values) || length(values) != length(grid)) {
      stop("`", arg, "` must return one number per value it is given.",
           call. = FALSE)
    }
  } else if (is.numeric(effect)) {
    if (length(effect) != length(levels)) {
      stop("`", arg, "` must have ", length(levels), " values, one per ",
           sub("_effect", "", arg), ".", call. = FALSE)
    }
    values <- effect[match(as.vector(grid), levels)]
  } else {
    stop("`", arg, "` must be NULL, a function or a numeric vector.",
         call. = FALSE)
  }
  if (any(!is.finite(values))) {
    stop("`", arg, "` gave non-finite values.", call. = FALSE)
  }
  matrix(values, nrow = nrow(grid), ncol = ncol(grid))
}
