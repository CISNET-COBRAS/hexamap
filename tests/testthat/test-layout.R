test_that("the layout has one cell per age-period combination", {
  rates <- example_rates()
  rates[2, 3] <- NA
  lay <- hexamap_layout(rates, 15, 1999, 1)
  expect_s3_class(lay, "hexamap_layout")
  expect_equal(nrow(lay$cells), length(rates))
  expect_equal(lay$cells$value, as.vector(rates))
  # column-major order: ages vary fastest
  expect_equal(lay$cells$age[1:3], 15:17)
  expect_equal(lay$cells$period[c(1, 57)], c(1999, 2000))
  expect_equal(lay$cells$cohort, lay$cells$period - lay$cells$age)
  missing <- lay$cells$age == 16 & lay$cells$period == 2001
  expect_true(is.na(lay$cells$color[missing]))
  expect_equal(sum(is.na(lay$cells$color)), 1)
})

test_that("cell centers follow the hexamap equations", {
  lay <- hexamap_layout(example_rates(), 15, 1999, 1)
  expect_equal(lay$cells$x, lay$cells$period * sqrt(3) / 2)
  expect_equal(lay$cells$y, lay$cells$age - lay$cells$period / 2)
  expect_equal(apc_to_xy(lay$cells$age, lay$cells$period),
               list(x = lay$cells$x, y = lay$cells$y))
})

test_that("ages, periods and interval are taken from numeric dimnames", {
  rates <- example_rates(ages = seq(15, 70, 5), periods = seq(1980, 2015, 5))
  dimnames(rates) <- list(seq(15, 70, 5), seq(1980, 2015, 5))
  lay <- hexamap_layout(rates)
  expect_equal(lay$dims,
               list(first_age = 15, last_age = 70, first_period = 1980,
                    last_period = 2015, interval = 5))
  # explicit arguments take precedence over dimnames
  expect_equal(hexamap_layout(rates, first_age = 20)$dims$first_age, 20)
})

test_that("dims errors are informative", {
  rates <- example_rates()
  expect_error(hexamap_layout(rates), "Supply `first_age`")
  expect_error(hexamap_layout(rates, first_age = 15), "Supply `first_period`")
  bad <- example_rates(ages = c(15, 20, 30), periods = 2000:2002)
  dimnames(bad) <- list(c(15, 20, 30), 2000:2002)
  expect_error(hexamap_layout(bad, interval = 5), "equally spaced")
  uneven <- example_rates(ages = seq(15, 25, 5), periods = 2000:2002)
  dimnames(uneven) <- list(seq(15, 25, 5), 2000:2002)
  expect_error(hexamap_layout(uneven), "same width")
})

test_that("non-numeric dimnames are ignored", {
  rates <- example_rates(ages = 15:17, periods = 2000:2001)
  dimnames(rates) <- list(c("15-19", "20-24", "25-29"), c("a", "b"))
  expect_error(hexamap_layout(rates), "Supply `first_age`")
  expect_no_error(hexamap_layout(rates, 15, 2000, 1))
})

test_that("the extent covers the hexagons and isolines", {
  lay <- hexamap_layout(example_rates(), 15, 1999, 1, wrap_cohort_labels = FALSE)
  iso <- do.call(rbind, lay$isolines)
  expect_true(all(c(iso$x1, iso$x2) >= lay$extent[["xmin"]]))
  expect_true(all(c(iso$y1, iso$y2) >= lay$extent[["ymin"]]))
  expect_true(all(lay$cells$x + max(lay$hexagon$x) <= lay$extent[["xmax"]]))
  expect_true(all(lay$cells$y + max(lay$hexagon$y) <= lay$extent[["ymax"]]))
})

test_that("layouts print a summary", {
  rates <- example_rates()
  rates[1, 1] <- NA
  expect_snapshot(print(hexamap_layout(rates, 15, 1999, 1, colorbar_range = c(0, 40))))
})

test_that("the default palette is viridis, or diverging with a midpoint", {
  rates <- example_rates()
  expect_equal(hexamap_layout(rates, 15, 1999, 1)$scale$colors,
               hexamap_palette("viridis", 100))
  lay <- hexamap_layout(rates / mean(rates), 15, 1999, 1,
                        colorbar_scale = "log", midpoint = 1)
  expect_equal(lay$scale$colors, hexamap_palette("Blue-Red 3", 100))
  expect_equal(prod(lay$scale$limits), 1) # symmetric around 1 on the log scale
  # color_map overrides palette
  expect_equal(hexamap_layout(rates, 15, 1999, 1, color_map = c("red", "blue"),
                              palette = "jet")$scale$colors,
               c("red", "blue"))
  expect_length(hexamap_layout(rates, 15, 1999, 1, n_colors = 12)$scale$colors, 12)
})

test_that("an empty color_map is rejected", {
  expect_error(hexamap_layout(example_rates(), 15, 1999, 1, color_map = character()),
               "at least one color")
})

test_that("apc_to_xy() validates its inputs", {
  expect_error(apc_to_xy("40", 2000), "numeric")
})
