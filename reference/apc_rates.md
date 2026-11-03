# Compute rates from counts and population

Compute rates from counts and population

## Usage

``` r
apc_rates(counts, population, per = 1e+05)
```

## Arguments

- counts:

  Numeric matrix (or data frame of numeric columns) of event counts,
  with ages as rows and periods as columns.

- population:

  Matrix of the same dimensions with the population at risk.

- per:

  Rates are expressed per `per` persons.

## Value

A numeric matrix of rates per `per`, with the dimnames of `counts` (or
of `population`). Cells with zero or missing population are `NA`.

## Examples

``` r
deaths <- matrix(c(5, 12, 30, 8, 20, 41), nrow = 3,
                 dimnames = list(c(20, 30, 40), c(2000, 2001)))
pop <- matrix(c(9e4, 1e5, 1.1e5, 9e4, 1e5, 0), nrow = 3)
apc_rates(deaths, pop)
#>         2000      2001
#> 20  5.555556  8.888889
#> 30 12.000000 20.000000
#> 40 27.272727        NA
```
