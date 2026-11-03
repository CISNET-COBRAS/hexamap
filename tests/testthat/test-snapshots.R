# Visual regression tests; vdiffr skips them on CRAN. A small matrix keeps
# the SVG snapshots small while exercising every drawing element.
skip_if_not_installed("vdiffr")

small <- simulate_apc(ages = 20:39, periods = 2000:2011,
                      age_effect = function(a) -((a - 30) / 8)^2,
                      period_effect = function(p) 0.1 * pmax(p - 2006, 0),
                      cohort_effect = function(c) ifelse(c >= 1972 & c <= 1976, 0.6, 0))
small_na <- small
small_na[1:2, 1:2] <- NA

test_that("base graphics hexamaps look the same", {
  vdiffr::expect_doppelganger("eappendix interface", function() {
    create_hexamap(small, 20, 2000, 1, first_age_isoline = 20,
                   first_period_isoline = 2000, isoline_interval = 5,
                   color_scale = c(0, 15))
  })
  vdiffr::expect_doppelganger("lexis diagram", function() {
    lexis_map(small_na, legend = FALSE, highlight = list(cohort = 1975))
  })
  vdiffr::expect_doppelganger("labels highlight title", function() {
    hexamap(small_na, labels = c(age = "Age ", period = ""),
            highlight = list(cohort = 1975, age = 25, col = "white"),
            title_text = "Title", subtitle_text = "Subtitle", title_adj = 0,
            na_label = "Suppressed")
  })
  vdiffr::expect_doppelganger("quantile scale", function() {
    hexamap(small, colorbar_scale = "quantile", palette = "viridis")
  })
  vdiffr::expect_doppelganger("diverging log scale", function() {
    hexamap(small / small[, "2005"], colorbar_scale = "log", midpoint = 1,
            isolines = "cohort")
  })
  vdiffr::expect_doppelganger("five-year groups unwrapped", function() {
    rates <- simulate_apc(seq(0, 80, 5), seq(1950, 2015, 5),
                          age_effect = function(a) a / 30)
    hexamap(rates, wrap_cohort_labels = FALSE)
  })
})

test_that("ggplot2 hexamaps look the same", {
  skip_if_not_installed("ggplot2")
  vdiffr::expect_doppelganger("gg hexamap", {
    gg_hexamap(small_na, highlight = list(cohort = 1975))
  })
  vdiffr::expect_doppelganger("gg facets lexis", {
    gg_hexamap(list(A = small, B = small * 2), tiles = "square")
  })
})
