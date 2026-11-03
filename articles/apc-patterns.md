# Reading age, period and cohort patterns

This vignette shows what age, period and cohort effects look like on a
hexamap and on a traditional Lexis diagram, alone, in pairs, and all
three together. The rates are simulated with
[`simulate_apc()`](https://cisnet-cobras.github.io/hexamap/reference/simulate_apc.md)
from a log-linear model,

``` math
\log \mu_{ap} = \log(\text{baseline}) + f(a) + g(p) + h(c), \qquad c = p - a,
```

where $`f`$, $`g`$ and $`h`$ are the age, period and cohort effects on
the log scale (log rate ratios).

``` r

library(hexamap)
```

## The effects

Each effect has a shape that is easy to tell apart from the others:

- **Age**: rates peak at age 45.
- **Period**: rates rise steeply after 2010, as in an epidemic.
- **Cohort**: people born between 1955 and 1965 have twice the risk of
  other cohorts.

``` r

ages <- 15:70
periods <- 1999:2018

age <- function(a) -((a - 45) / 15)^2
period <- function(p) 0.12 * pmax(p - 2010, 0)
cohort <- function(c) ifelse(c >= 1955 & c <= 1965, log(2), 0)
```

Each pattern below is shown as a Lexis diagram (left) and a hexamap
(right), with the 1960 birth cohort highlighted. Each figure uses its
own color scale.

``` r

compare <- function(rates, title) {
  op <- par(mfrow = c(1, 2), oma = c(0, 0, 1.5, 0))
  on.exit(par(op))
  lexis_map(rates, legend = FALSE, highlight = list(cohort = 1960, col = "white"))
  hexamap(rates, highlight = list(cohort = 1960, col = "white"))
  mtext(title, outer = TRUE, font = 2, cex = 1.2)
}
```

## How to read the isolines

- **Age effects** follow the age isolines: on a hexamap they slope down
  to the right (−30°); on a Lexis diagram they are horizontal.
- **Period effects** follow the period isolines, which are vertical on
  both.
- **Cohort effects** follow the cohort isolines: on a hexamap they slope
  up to the right (+30°); on a Lexis diagram they are diagonal (45°).

On a hexamap the three directions are equally spaced, so a cohort band
is as easy to follow as an age or period band.

## One effect

``` r

compare(simulate_apc(ages, periods, age_effect = age), "Age effect")
```

![Lexis diagram and hexamap of an age effect: a band of high rates
around age 45 runs along the age
isolines.](apc-patterns_files/figure-html/age-1.png)

``` r

compare(simulate_apc(ages, periods, period_effect = period), "Period effect")
```

![Lexis diagram and hexamap of a period effect: rates increase from left
to right after 2010, in vertical
bands.](apc-patterns_files/figure-html/period-1.png)

``` r

compare(simulate_apc(ages, periods, cohort_effect = cohort), "Cohort effect")
```

![Lexis diagram and hexamap of a cohort effect: a band of high rates for
the 1955 to 1965 birth cohorts runs along the cohort
isolines.](apc-patterns_files/figure-html/cohort-1.png)

The cohort band is the same width as the others on the hexamap, while on
the Lexis diagram it is narrower across and drawn as a staircase of
squares.

## Two effects

``` r

compare(simulate_apc(ages, periods, age_effect = age, period_effect = period),
        "Age and period effects")
```

![Lexis diagram and hexamap of age and period effects combined: the age
band around 45 brightens after
2010.](apc-patterns_files/figure-html/age-period-1.png)

``` r

compare(simulate_apc(ages, periods, age_effect = age, cohort_effect = cohort),
        "Age and cohort effects")
```

![Lexis diagram and hexamap of age and cohort effects combined: the
high-risk cohort band is brightest where it crosses age
45.](apc-patterns_files/figure-html/age-cohort-1.png)

``` r

compare(simulate_apc(ages, periods, period_effect = period, cohort_effect = cohort),
        "Period and cohort effects")
```

![Lexis diagram and hexamap of period and cohort effects combined: the
high-risk cohort band brightens after
2010.](apc-patterns_files/figure-html/period-cohort-1.png)

## All three effects

``` r

apc <- simulate_apc(ages, periods, age_effect = age, period_effect = period,
                    cohort_effect = cohort)
compare(apc, "Age, period and cohort effects")
```

![Lexis diagram and hexamap of age, period and cohort effects combined:
the brightest cells are where the high-risk cohorts are around age 45
after 2010.](apc-patterns_files/figure-html/all-1.png)

Real data are noisy. With Poisson counts for 50,000 people per cell, the
three effects are still visible:

``` r

set.seed(2020)
noisy <- simulate_apc(ages, periods, age_effect = age, period_effect = period,
                      cohort_effect = cohort, population = 5e4)
compare(noisy, "Age, period and cohort effects, with noise")
```

![Lexis diagram and hexamap of the same three effects with Poisson
noise.](apc-patterns_files/figure-html/noise-1.png)

## What a hexamap cannot show

Because cohort = period − age, linear trends in age, period and cohort
cannot be separated: this is the APC identification problem. For
example, a linear increase with age and period gives exactly the same
rates as a steeper linear increase with age plus a linear increase with
cohort:

``` r

ap <- simulate_apc(ages, periods,
                   age_effect = function(a) 0.02 * a,
                   period_effect = function(p) 0.02 * p)
ac <- simulate_apc(ages, periods,
                   age_effect = function(a) 0.04 * a,
                   cohort_effect = function(c) 0.02 * c)
all.equal(ap, ac)
#> [1] TRUE
```

No display can tell these apart. A hexamap shows departures from
linearity, such as the peak, the surge and the high-risk cohorts above,
without distorting any of the three directions. Estimating the separate
effects needs an APC model with an identifying assumption.
