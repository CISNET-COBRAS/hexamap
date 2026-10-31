test_that("Lexis layouts use square tiles and diagonal cohort isolines", {
  lay <- hexamap_layout(example_rates(), 15, 1999, 1, tiles = "square")
  expect_equal(lay$tiles, "square")
  expect_equal(lay$cells$x, lay$cells$period)
  expect_equal(lay$cells$y, lay$cells$age)
  co <- lay$isolines$cohort
  expect_equal(co$y2 - co$y1, co$x2 - co$x1) # slope 1
  ag <- lay$isolines$age
  expect_equal(ag$y1, ag$y2) # horizontal
})

test_that("lexis_map() draws with the hexamap() options", {
  local_null_device()
  rates <- example_rates()
  rates[1:3, 1:2] <- NA
  expect_invisible(lay <- lexis_map(rates, first_age = 15, first_period = 1999,
                                    interval = 1, highlight = list(cohort = 1975),
                                    title_text = "Lexis"))
  expect_equal(lay$tiles, "square")
  old <- par(mfrow = c(1, 2))
  on.exit(par(old))
  expect_no_error(lexis_map(rates, 15, 1999, 1, legend = FALSE))
  expect_no_error(hexamap(rates, 15, 1999, 1))
})
