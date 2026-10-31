test_that("simulate_apc() builds a log-linear APC surface", {
  rates <- simulate_apc(15:20, 2000:2002)
  expect_equal(dim(rates), c(6, 3))
  expect_equal(dimnames(rates), list(as.character(15:20), as.character(2000:2002)))
  expect_equal(unname(rates), matrix(10, 6, 3))

  f <- function(a) 0.1 * (a - 15)
  g <- function(p) -0.2 * (p - 2000)
  h <- function(c) ifelse(c == 1983, 1, 0)
  rates <- simulate_apc(15:20, 2000:2002, f, g, h, baseline = 5)
  expect_equal(log(rates["17", "2000"]), log(5) + f(17) + g(2000) + h(1983))
  expect_equal(log(rates["16", "2001"]), log(5) + f(16) + g(2001))
})

test_that("effects can be given as vectors", {
  rates <- simulate_apc(1:3, 1:2, age_effect = c(0, 1, 2), period_effect = c(0, 0.5),
                        cohort_effect = c(0, 0, 0, 0)) # cohorts -2:1
  expect_equal(log(rates[3, 2] / 10), 2.5)
  expect_error(simulate_apc(1:3, 1:2, age_effect = 1:2), "3 values, one per age")
  expect_error(simulate_apc(1:3, 1:2, cohort_effect = 1), "4 values, one per cohort")
})

test_that("linear age-period and age-cohort drifts are indistinguishable", {
  ap <- simulate_apc(15:70, 1999:2018, age_effect = function(a) 0.02 * a,
                     period_effect = function(p) 0.02 * p)
  ac <- simulate_apc(15:70, 1999:2018, age_effect = function(a) 0.04 * a,
                     cohort_effect = function(c) 0.02 * c)
  expect_equal(ap, ac)
})

test_that("Poisson noise is drawn for a population", {
  set.seed(42)
  rates <- simulate_apc(15:70, 1999:2018, population = 1e6)
  counts <- attr(rates, "counts")
  expect_equal(dim(counts), c(56, 20))
  expect_true(all(counts == round(counts)))
  expect_equal(mean(counts), 100, tolerance = 0.05)
  expect_equal(as.vector(rates), as.vector(counts / 1e6 * 1e5))
  pop <- matrix(1e5, 56, 20)
  pop[1, 1] <- 0
  expect_true(is.na(simulate_apc(15:70, 1999:2018, population = pop)[1, 1]))
})

test_that("simulate_apc() validates its inputs", {
  expect_error(simulate_apc("a", 1), "numeric vectors")
  expect_error(simulate_apc(1:3, 1:2, age_effect = "x"), "NULL, a function")
  expect_error(simulate_apc(1:3, 1:2, age_effect = function(a) 1), "one number per")
  expect_error(simulate_apc(1:3, 1:2, age_effect = function(a) 1 / (a - 2)), "non-finite")
  expect_error(simulate_apc(1:3, 1:2, population = -1), "population")
  expect_error(simulate_apc(1:3, 1:2, population = matrix(1, 2, 2)), "population")
})
