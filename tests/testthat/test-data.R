test_that("the overdose datasets have the documented shape", {
  expect_equal(dim(overdose_white_men), c(56, 20))
  expect_equal(dimnames(overdose_white_men),
               list(as.character(15:70), as.character(1999:2018)))
  expect_equal(nrow(overdose_white_men_long), 1120)
  expect_equal(as.vector(table(overdose_white_men_long$flag)), c(1037, 63, 20))
})

test_that("no counts of 9 or fewer deaths are shipped", {
  d <- overdose_white_men_long
  suppressed <- d$flag == "suppressed"
  expect_true(all(is.na(d$deaths[suppressed]) & is.na(d$rate[suppressed])))
  expect_true(all(d$deaths[!suppressed] >= 10))
  expect_true(all(d$deaths[d$flag == "unreliable"] < 20))
  expect_equal(sum(is.na(overdose_white_men)), 20)
})

test_that("the matrix and the long data agree", {
  d <- overdose_white_men_long
  expect_equal(as.vector(overdose_white_men), d$rate[order(d$year, d$age)])
  ok <- !is.na(d$deaths)
  expect_equal(d$rate[ok], d$deaths[ok] / d$population[ok] * 1e5)
})

test_that("the paper's figure can be drawn", {
  local_null_device()
  expect_no_error(create_hexamap(overdose_white_men, 15, 1999, 1,
                                 first_age_isoline = 20, first_period_isoline = 2000,
                                 isoline_interval = 10, color_scale = c(0, 40)))
})
