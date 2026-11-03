# Convert age and period to plot coordinates

Maps age-period pairs onto the Cartesian coordinates used by
[`hexamap()`](https://cisnet-cobras.github.io/hexamap/reference/hexamap.md),
with \\x = p\sqrt{3}/2\\ and \\y = a - p/2\\, or by
[`lexis_map()`](https://cisnet-cobras.github.io/hexamap/reference/lexis_map.md),
with \\x = p\\ and \\y = a\\. Use it to add points, lines or text to an
existing plot.

## Usage

``` r
apc_to_xy(age, period, tiles = c("hexagon", "square"))
```

## Arguments

- age:

  Numeric vector of ages.

- period:

  Numeric vector of periods, recycled to the length of `age`.

- tiles:

  `"hexagon"` for hexamaps or `"square"` for Lexis diagrams.

## Value

A list with numeric vectors `x` and `y`, which can be passed directly to
[`graphics::points()`](https://rdrr.io/r/graphics/points.html) or
[`graphics::text()`](https://rdrr.io/r/graphics/text.html).

## Examples

``` r
rates <- outer(15:70, 1999:2018, function(a, p) (a - 10) * (p - 1990) / 40)
hexamap(rates, first_age = 15, first_period = 1999, interval = 1)
# mark the 1975 birth cohort at age 40 (period 2015)
graphics::points(apc_to_xy(age = 40, period = 2015), pch = 19)
```
