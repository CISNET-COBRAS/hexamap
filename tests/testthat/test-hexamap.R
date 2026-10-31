test_that("hexamap draws and returns its layout invisibly", {
  local_null_device()
  expect_invisible(out <- hexamap(example_rates(), 15, 1999, 1))
  expect_s3_class(out, "hexamap_layout")
})

test_that("graphical parameters are restored", {
  local_null_device()
  # user coordinates (usr, xaxp, yaxp) legitimately change when plotting
  pars <- c("mai", "mar", "plt", "fig", "las", "cex", "xpd")
  before <- par(pars)
  hexamap(example_rates(), 15, 1999, 1, title_text = "T", subtitle_text = "S")
  expect_equal(par(pars), before)
})

test_that("zoom_factor is deprecated", {
  local_null_device()
  expect_warning(hexamap(example_rates(), 15, 1999, 1, zoom_factor = 0.8),
                 "deprecated")
})

test_that("hexamaps can be combined with par(mfrow)", {
  local_null_device()
  old <- par(mfrow = c(1, 2))
  on.exit(par(old))
  hexamap(example_rates(), 15, 1999, 1)
  expect_equal(par("mfg")[1:2], c(1L, 1L))
  hexamap(example_rates(), 15, 1999, 1, colorbar_scale = "quantile")
  expect_equal(par("mfg")[1:2], c(1L, 2L))
})

test_that("labels, title and colorbar fit inside the figure region", {
  local_null_device()
  lay <- hexamap(example_rates(), 15, 1999, 1, title_text = "Title",
                 subtitle_text = "Subtitle")
  usr <- par("usr")
  # the hexamap extent is inside the user coordinates
  expect_true(lay$extent[["xmin"]] >= usr[1] && lay$extent[["xmax"]] <= usr[2])
  expect_true(lay$extent[["ymin"]] >= usr[3] && lay$extent[["ymax"]] <= usr[4])
})

test_that("a device too small for the labels gives a clear error", {
  grDevices::pdf(NULL, width = 0.6, height = 0.6)
  on.exit(grDevices::dev.off())
  expect_error(hexamap(example_rates(), 15, 1999, 1), "too small")
})

test_that("the colorbar can be turned off", {
  local_null_device()
  expect_no_error(hexamap(example_rates(), 15, 1999, 1, legend = FALSE))
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
