skip_if_not_installed("ggplot2")

test_that("gg_hexamap() returns a ggplot that builds", {
  rates <- example_rates()
  rates[1:3, 1:2] <- NA
  p <- gg_hexamap(rates, first_age = 15, first_period = 1999, interval = 1,
                  highlight = list(cohort = 1975))
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  # tiles, regular isolines, highlighted isolines and labels
  expect_length(built$data, 4)
  # one polygon per cell, NA cells included in na_color
  expect_equal(length(unique(built$data[[1]]$group)), length(rates))
  expect_true(all(built$data[[1]]$fill[is.na(built$data[[1]]$colour)] == "grey90"))
})

test_that("fill follows the layout's color scale", {
  lay <- hexamap_layout(example_rates(), 15, 1999, 1, colorbar_range = c(0, 40))
  p <- ggplot2::autoplot(lay)
  expect_s3_class(p, "ggplot")
  fill_scale <- p$scales$get_scales("fill")
  expect_equal(fill_scale$get_limits(), c(0, 1))
  expect_equal(fill_scale$get_breaks(), lay$scale$ticks$fraction)
  expect_equal(fill_scale$get_labels(), format_tick_labels(lay$scale$ticks$value))
  expect_equal(lay$cells$fraction, lay$cells$value / 40)
})

test_that("a named list gives panels with a shared scale", {
  a <- example_rates()
  b <- example_rates() * 2
  p <- gg_hexamap(list(Low = a, High = b), first_age = 15, first_period = 1999,
                  interval = 1)
  built <- ggplot2::ggplot_build(p)
  expect_equal(nrow(built$layout$layout), 2)
  expect_equal(levels(p$layers[[1]]$data$panel), c("Low", "High"))
  # the common range covers both panels
  expect_equal(max(p$layers[[1]]$data$fraction, na.rm = TRUE), 1)
  expect_equal(max(p$layers[[1]]$data$fraction[p$layers[[1]]$data$panel == "Low"],
                   na.rm = TRUE),
               (max(a) - min(a)) / (max(b) - min(a)))
  expect_error(gg_hexamap(list(a, b), first_age = 15, first_period = 1999),
               "must be named")
  expect_error(gg_hexamap(list(A = a, B = b), first_age = 15, first_period = 1999,
                          colorbar_scale = "quantile"),
               "cannot be shared")
})

test_that("Lexis tiles, hidden isolines and blank missing cells work", {
  rates <- example_rates()
  rates[1, 1] <- NA
  p <- gg_hexamap(rates, first_age = 15, first_period = 1999, interval = 1,
                  tiles = "square", isolines = NULL, na_color = NA)
  built <- ggplot2::ggplot_build(p)
  expect_length(built$data, 1) # tiles only
  expect_equal(length(unique(built$data[[1]]$group)), length(rates) - 1)
})

test_that("ggplot arguments are validated", {
  expect_error(gg_hexamap(example_rates(), first_age = 15, first_period = 1999,
                          label_size = 0), "label_size")
  expect_error(gg_hexamap(example_rates(), first_age = 15, first_period = 1999,
                          labels = c(year = "Y")), "labels")
})
