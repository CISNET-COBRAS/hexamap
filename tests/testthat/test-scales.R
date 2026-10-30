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

test_that("colorbar tick values span the scale for any palette length", {
  f <- seq(0, 1, length.out = 11)
  expect_equal(colorbar_tick_values(NULL, c(0, 40), "linear", f), seq(0, 40, 4))
  expect_equal(colorbar_tick_values(NULL, c(1, 100), "log", f)[c(1, 6, 11)],
               c(1, 10, 100))
  v <- 1:101
  expect_equal(colorbar_tick_values(v, range(v), "quantile", f), seq(1, 101, 10))
})

test_that("tick labels keep small and large values readable", {
  expect_equal(format_tick_labels(c(0, 0.00123, 4, 12345.6)),
               c("0", "0.00123", "4", "12,300"))
})
