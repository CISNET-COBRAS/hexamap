test_that("hexamap draws and returns NULL invisibly", {
  local_null_device()
  expect_invisible(out <- hexamap(example_rates(), 15, 1999, 1))
  expect_null(out)
})

test_that("graphical parameters are restored", {
  local_null_device()
  # user coordinates (usr, xaxp, yaxp) legitimately change when plotting
  pars <- c("mai", "mar", "plt", "fig", "las", "cex")
  before <- par(pars)
  hexamap(example_rates(), 15, 1999, 1, zoom_factor = 0.8)
  expect_equal(par(pars), before)
})

test_that("missing cells are skipped instead of breaking the scale", {
  local_null_device()
  rates <- example_rates()
  rates[1:3, 1] <- NA
  rates[4, 2] <- NaN
  expect_no_error(hexamap(rates, 15, 1999, 1))
  rates[] <- NA
  expect_error(hexamap(rates, 15, 1999, 1), "no finite values")
})

test_that("all color scales draw, including legacy names", {
  local_null_device()
  rates <- example_rates()
  for (scale in c("linear", "log", "quantile", "Normal", "Log", "Percentile")) {
    expect_no_error(hexamap(rates, 15, 1999, 1, colorbar_scale = scale))
  }
})

test_that("a log scale with zeros warns instead of failing", {
  local_null_device()
  rates <- example_rates()
  rates[1, 1] <- 0
  expect_warning(hexamap(rates, 15, 1999, 1, colorbar_scale = "Log"),
                 "values <= 0")
})

test_that("palettes of any length are supported", {
  local_null_device()
  for (n in c(1, 9, 256)) {
    expect_no_error(hexamap(example_rates(), 15, 1999, 1,
                            color_map = grDevices::hcl.colors(n)))
  }
})

test_that("data frames of numeric columns are accepted", {
  local_null_device()
  expect_no_error(hexamap(as.data.frame(example_rates()), 15, 1999, 1))
})

test_that("5-year groups and non-wrapped cohort labels draw", {
  local_null_device()
  rates <- example_rates(ages = seq(15, 70, 5), periods = seq(1980, 2015, 5))
  expect_no_error(hexamap(rates, 15, 1980, 5))
  expect_no_error(hexamap(rates, 15, 1980, 5, isoline_interval = 10,
                          wrap_cohort_labels = FALSE))
})

test_that("invalid inputs give informative errors", {
  local_null_device()
  rates <- example_rates()
  expect_error(hexamap(letters, 15, 1999, 1), "numeric matrix")
  expect_error(hexamap(data.frame(a = "x"), 15, 1999, 1), "non-numeric")
  expect_error(hexamap(rates, "15", 1999, 1), "`first_age`")
  expect_error(hexamap(rates, 15, 1999, 0), "`interval` must be positive")
  expect_error(hexamap(rates, 15, 1999, 1, zoom_factor = 1.5), "zoom_factor")
  expect_error(hexamap(rates, 15, 1999, 1, colorbar_scale = "normalized"),
               "must be one of")
  expect_error(hexamap(rates, 15, 1999, 1, first_age_isoline = 80),
               "first_age_isoline")
  expect_error(hexamap(rates, 15, 1999, 1, first_period_isoline = 1990),
               "first_period_isoline")
  expect_error(hexamap(rates, 15, 1999, 1, wrap_cohort_labels = NA),
               "TRUE or FALSE")
})

test_that("the eAppendix example runs unchanged through create_hexamap()", {
  local_null_device()
  expect_no_error(
    create_hexamap(data = example_rates(),
                   first_age = 15,
                   first_period = 1999,
                   interval = 1,
                   first_age_isoline = 20,
                   first_period_isoline = 2000,
                   isoline_interval = 10,
                   color_scale = c(0, 40))
  )
})
