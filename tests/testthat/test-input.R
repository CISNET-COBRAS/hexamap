test_that("long data are reshaped onto a regular grid", {
  long <- expand.grid(age = c(20, 30, 50), period = c(2000, 2010))
  long$rate <- seq_len(nrow(long))
  m <- as_apc_matrix(long[-2, ], value = "rate")
  expect_equal(dimnames(m), list(c("20", "30", "40", "50"), c("2000", "2010")))
  expect_equal(m["20", "2000"], 1)
  expect_true(is.na(m["30", "2000"])) # dropped row
  expect_true(all(is.na(m["40", ])))  # gap in the grid
  expect_equal(m["50", "2010"], 6)
  # the result goes straight into hexamap_layout()
  expect_equal(hexamap_layout(m)$dims$interval, 10)
})

test_that("custom column names work", {
  d <- data.frame(Year = rep(2000:2001, each = 2), Age = rep(1:2, 2), Rate = 1:4)
  m <- as_apc_matrix(d, age = "Age", period = "Year", value = "Rate")
  expect_equal(unname(m), matrix(1:4, 2))
})

test_that("as_apc_matrix() reports bad input", {
  d <- data.frame(age = c(1, 1), period = c(1, 1), value = c(1, 2))
  expect_error(as_apc_matrix(d), "more than one row")
  expect_error(as_apc_matrix(as.matrix(d)), "data frame")
  expect_error(as_apc_matrix(d, value = "rate"), "`rate` not found")
  d2 <- data.frame(age = 1:2, period = 1, value = c("1.5", "Unreliable"))
  expect_error(as_apc_matrix(d2), "Unreliable")
  d3 <- data.frame(age = c(0, 1, 2.5), period = 1, value = 1)
  expect_error(as_apc_matrix(d3), "equally spaced")
  d4 <- data.frame(age = c(1, NA), period = 1, value = 1)
  expect_error(as_apc_matrix(d4), "without missing")
})

test_that("apc_rates() divides counts by population", {
  deaths <- matrix(c(5, 10, 0, 4), 2, dimnames = list(c(20, 30), c(2000, 2001)))
  pop <- matrix(c(1e5, 2e5, 0, NA), 2)
  r <- apc_rates(deaths, pop)
  expect_equal(r[1, 1], 5)
  expect_equal(r[2, 1], 5)
  expect_true(all(is.na(r[, 2])))
  expect_equal(dimnames(r), dimnames(deaths))
  expect_equal(apc_rates(deaths, pop, per = 1)[1, 1], 5e-5)
  expect_error(apc_rates(deaths, pop[, 1, drop = FALSE]), "same dimensions")
})
