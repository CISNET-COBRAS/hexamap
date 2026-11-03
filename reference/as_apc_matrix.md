# Convert long age-period data to a matrix

Reshapes a data frame with one row per age-period combination, such as
an export from CDC WONDER or SEER, into the age-by-period matrix used by
[`hexamap()`](https://cisnet-cobras.github.io/hexamap/reference/hexamap.md).
Combinations missing from `data` become `NA`.

## Usage

``` r
as_apc_matrix(data, age = "age", period = "period", value = "value")
```

## Arguments

- data:

  A data frame.

- age, period, value:

  Names of the columns with the ages, periods and values (for example,
  rates).

## Value

A numeric matrix with one row per age and one column per period, on a
regular grid from the first to the last age and period, with the ages
and periods as dimnames.

## Examples

``` r
long <- expand.grid(age = seq(20, 80, 10), period = seq(1990, 2020, 10))
long$rate <- with(long, age * (period - 1980) / 100)
as_apc_matrix(long, value = "rate")
#>    1990 2000 2010 2020
#> 20    2    4    6    8
#> 30    3    6    9   12
#> 40    4    8   12   16
#> 50    5   10   15   20
#> 60    6   12   18   24
#> 70    7   14   21   28
#> 80    8   16   24   32
```
