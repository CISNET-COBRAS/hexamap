test_that("every cohort isoline gets exactly one start and one end", {
  configs <- expand.grid(n_ages = c(10, 56), n_periods = c(10, 20),
                         interval = c(1, 5), wrap = c(TRUE, FALSE),
                         offset = c(0, 1))
  for (k in seq_len(nrow(configs))) {
    cfg <- configs[k, ]
    first_age <- 15
    first_period <- 1999
    last_age <- first_age + (cfg$n_ages - 1) * cfg$interval
    last_period <- first_period + (cfg$n_periods - 1) * cfg$interval
    step <- 2 * cfg$interval
    age_isolines <- seq(first_age + cfg$offset, last_age, by = step)
    first_cohort_isoline <- first_period + cfg$offset - tail(age_isolines, 1)
    cohorts <- seq(first_cohort_isoline, last_period - first_age, by = step)

    ends <- cohort_isoline_endpoints(cohorts, first_age, last_age,
                                     first_period, last_period, cfg$wrap)
    for (part in c("p1", "a1", "p2", "a2")) {
      expect_length(ends[[part]], length(cohorts))
    }
    # start and end points lie on the cohort they belong to
    expect_equal(ends$p1 - ends$a1, cohorts)
    expect_equal(ends$p2 - ends$a2, cohorts)
  }
})

test_that("default isoline spacing is a round multiple of the interval", {
  dims <- function(a1, a2, p1, p2, interval) {
    list(first_age = a1, last_age = a2, first_period = p1, last_period = p2,
         interval = interval)
  }
  expect_equal(nice_isoline_interval(dims(15, 70, 1999, 2018, 1)), 10)
  expect_equal(nice_isoline_interval(dims(0, 80, 1950, 2015, 5)), 10)
  expect_equal(nice_isoline_interval(dims(0, 100, 1900, 2020, 5)), 20)
  expect_equal(nice_isoline_interval(dims(40, 45, 2000, 2004, 1)), 1)
  expect_equal(nice_isoline_interval(dims(0, 84, 1950, 2014, 2)), 20)
  expect_equal(nice_isoline_interval(dims(0, 1, 0, 1, 0.25)), 0.25)
})

test_that("isolines start at the first round value on the data grid", {
  expect_equal(round_isoline_start(15, 70, 10, 1), 20)
  expect_equal(round_isoline_start(1999, 2018, 10, 1), 2000)
  expect_equal(round_isoline_start(1980, 2015, 10, 5), 1980)
  # no round value on the grid: fall back to the first value
  expect_equal(round_isoline_start(1983, 2018, 10, 5), 1983)
  # isoline spacing smaller than the interval
  expect_equal(round_isoline_start(15, 70, 0.5, 1), 15)
})

test_that("the default isolines of the paper example are round", {
  lay <- hexamap_layout(example_rates(), 15, 1999, 1)
  expect_equal(lay$isolines$age$value, seq(20, 70, 10))
  expect_equal(lay$isolines$period$value, c(2000, 2010))
  expect_equal(lay$isolines$cohort$value, seq(1930, 2000, 10))
})

test_that("isoline types can be selected", {
  lay <- hexamap_layout(example_rates(), 15, 1999, 1, isolines = c("period", "cohort"))
  expect_equal(nrow(lay$isolines$age), 0)
  expect_gt(nrow(lay$isolines$period), 0)
  lay <- hexamap_layout(example_rates(), 15, 1999, 1, isolines = NULL)
  expect_true(all(vapply(lay$isolines, nrow, integer(1)) == 0))
  expect_error(hexamap_layout(example_rates(), 15, 1999, 1, isolines = "year"),
               "`isolines`")
})
