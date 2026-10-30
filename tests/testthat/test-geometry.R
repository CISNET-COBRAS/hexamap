test_that("neighboring hexagon centers are one interval apart on every APC axis", {
  for (interval in c(1, 5)) {
    a <- 40
    p <- 2000
    x0 <- compute_xcoordinate(p)
    y0 <- compute_ycoordinate(p, a)
    # age, period and cohort neighbors, in both directions
    neighbors <- rbind(c(0, 1), c(0, -1),   # same period, next/previous age
                       c(1, 0), c(-1, 0),   # same age, next/previous period
                       c(1, 1), c(-1, -1))  # same cohort
    for (k in seq_len(nrow(neighbors))) {
      p_k <- p + neighbors[k, 1] * interval
      a_k <- a + neighbors[k, 2] * interval
      d <- sqrt((compute_xcoordinate(p_k) - x0)^2 +
                (compute_ycoordinate(p_k, a_k) - y0)^2)
      expect_equal(d, interval)
    }
  }
})

test_that("isolines meet at 60 degree angles", {
  angle <- function(dp, da) {
    atan2(compute_ycoordinate(dp, da) - compute_ycoordinate(0, 0),
          compute_xcoordinate(dp) - compute_xcoordinate(0)) * 180 / pi
  }
  expect_equal(angle(1, 0), -30) # age isoline: fixed age, period increases
  expect_equal(angle(0, 1), 90)  # period isoline: fixed period, age increases
  expect_equal(angle(1, 1), 30)  # cohort isoline: age and period increase
})
