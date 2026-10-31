test_that("scale names are resolved, including legacy names in any case", {
  expect_equal(resolve_scale("Normal"), "linear")
  expect_equal(resolve_scale("normal"), "linear")
  expect_equal(resolve_scale("LOG"), "log")
  expect_equal(resolve_scale("Percentile"), "quantile")
  expect_equal(resolve_scale("quantile"), "quantile")
  expect_error(resolve_scale("loglinear"), "must be one of")
  expect_error(resolve_scale(c("log", "linear")), "must be one of")
})

test_that("every palette color is reachable and indices stay in range", {
  values <- seq(0, 1, length.out = 1e4)
  for (scale in c("linear", "log", "quantile")) {
    v <- if (scale == "log") values + 0.01 else values
    idx <- compute_color_index(v, range(v), scale, 100)
    expect_setequal(unique(idx), 1:100)
  }
  # the maximum gets the top color, the minimum the bottom one
  idx <- compute_color_index(c(0, 1), c(0, 1), "linear", 100)
  expect_equal(idx, c(1L, 100L))
})

test_that("quantile scale handles heavily tied data", {
  v <- c(rep(0, 500), seq(1, 10, length.out = 500))
  idx <- compute_color_index(v, range(v), "quantile", 100)
  expect_true(all(idx >= 1 & idx <= 100))
  expect_true(all(idx[v == 0] == idx[1]))
})

test_that("constant data map to the middle color", {
  expect_equal(compute_color_index(rep(3, 4), c(3, 3), "linear", 100),
               rep(50, 4))
})

test_that("log limits need positive values", {
  v <- c(0, 1, 10)
  expect_warning(lim <- resolve_limits(NULL, v, "log"), "values <= 0")
  expect_equal(lim, c(1, 10))
  expect_error(resolve_limits(c(0, 10), v, "log"), "must be positive")
  expect_error(suppressWarnings(resolve_limits(NULL, c(-1, 0), "log")),
               "needs positive values")
  expect_error(resolve_limits(c(10, 1), v, "linear"), "two increasing")
  expect_error(resolve_limits(c(1, NA), v, "linear"), "two increasing")
})

test_that("linear and log ticks are round values inside the limits", {
  ticks <- colorbar_ticks(NULL, c(0.235, 40), "linear")
  expect_equal(ticks$value, c(10, 20, 30, 40))
  expect_equal(ticks$fraction, (ticks$value - 0.235) / (40 - 0.235))

  ticks <- colorbar_ticks(NULL, c(1, 100), "log")
  expect_true(all(c(1, 10, 100) %in% ticks$value))
  expect_equal(ticks$fraction[ticks$value == 10], 0.5)
  expect_true(all(ticks$fraction >= 0 & ticks$fraction <= 1))
})

test_that("quantile ticks are every 20th percentile", {
  v <- 1:101
  ticks <- colorbar_ticks(v, range(v), "quantile")
  expect_equal(ticks$fraction, seq(0, 1, 0.2))
  expect_equal(ticks$value, seq(1, 101, 20))
})

test_that("the midpoint is always a tick", {
  ticks <- colorbar_ticks(NULL, c(0.4, 2.5), "log", midpoint = 1)
  expect_true(1 %in% ticks$value)
  expect_equal(colorbar_ticks(NULL, c(3, 3), "linear"),
               data.frame(fraction = 0.5, value = 3))
})

test_that("a midpoint makes the limits symmetric", {
  expect_equal(resolve_limits(NULL, c(-1, 3), "linear", midpoint = 0), c(-3, 3))
  expect_equal(resolve_limits(c(0, 1), NULL, "linear", midpoint = 0.25), c(-0.5, 1))
  expect_equal(resolve_limits(NULL, c(0.5, 4), "log", midpoint = 1), c(0.25, 4))
  expect_error(resolve_limits(NULL, 1:3, "log", midpoint = 0), "must be positive")
  expect_error(resolve_limits(NULL, 1:3, "quantile", midpoint = 2), "quantile")
  expect_error(resolve_limits(NULL, 1:3, "linear", midpoint = "1"), "midpoint")
})

test_that("palettes are looked up by name", {
  expect_equal(hexamap_palette(), hexamap_palette("jet", 100))
  expect_equal(hexamap_palette("viridis", 7), grDevices::hcl.colors(7, "viridis"))
  expect_equal(hexamap_palette("JET", 3)[c(1, 3)], c("#000000", "#7F0000"))
  expect_length(hexamap_palette("Blue-Red 3", 1), 1)
  expect_error(hexamap_palette("rainbowz"), "Unknown palette")
  expect_error(hexamap_palette("viridis", 2.5), "whole number")
  expect_error(hexamap_palette(NA), "single palette name")
})

test_that("tick labels keep small and large values readable", {
  expect_equal(format_tick_labels(c(0, 0.00123, 4, 12345.6)),
               c("0", "0.00123", "4", "12,300"))
})
