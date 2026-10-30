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
