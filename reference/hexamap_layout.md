# Compute the layout of a hexamap

Does all the computation behind
[`hexamap()`](https://cisnet-cobras.github.io/hexamap/reference/hexamap.md)
and
[`lexis_map()`](https://cisnet-cobras.github.io/hexamap/reference/lexis_map.md)
without drawing anything: tile centers and colors, isoline segments,
color scale and colorbar ticks. Draw the result with
[plot()](https://cisnet-cobras.github.io/hexamap/reference/plot.hexamap_layout.md),
or inspect it to build custom graphics.

## Usage

``` r
hexamap_layout(
  data,
  first_age = NULL,
  first_period = NULL,
  interval = NULL,
  first_age_isoline = NULL,
  first_period_isoline = NULL,
  isoline_interval = NULL,
  colorbar_range = NULL,
  colorbar_scale = "linear",
  color_map = NULL,
  palette = NULL,
  n_colors = 100,
  midpoint = NULL,
  isolines = c("age", "period", "cohort"),
  wrap_cohort_labels = TRUE,
  tiles = c("hexagon", "square")
)
```

## Arguments

- data:

  Numeric matrix (or data frame of numeric columns) with ages as rows
  and periods as columns. Missing and non-finite cells are not drawn.

- first_age:

  First age in `data`. Defaults to the first row name of `data`, if
  numeric.

- first_period:

  First period in `data`. Defaults to the first column name of `data`,
  if numeric.

- interval:

  Width of the age and period groups, which must be equal. Defaults to
  the spacing of the numeric row or column names of `data`, or 1.

- first_age_isoline, first_period_isoline:

  First age and period at which to draw an isoline. Default to the first
  age and period in the data that are multiples of `isoline_interval`
  (e.g. 20 and 2000 for data starting at age 15 in 1999 with isolines
  every 10 years).

- isoline_interval:

  Spacing between isolines. Defaults to the smallest round value (1, 2,
  5, 10, 20, 25, 50, ...) that is a multiple of `interval` and gives at
  most 8 isolines.

- colorbar_range:

  Numeric vector of length 2 with the lower and upper limits of the
  color scale. Values outside are truncated. Defaults to the range of
  `data`.

- colorbar_scale:

  How colors are mapped to values: `"linear"`, `"log"` or `"quantile"`
  (each color covers the same share of cells). The names `"Normal"` and
  `"Percentile"` used in earlier versions are also accepted.

- color_map:

  Vector of colors from low to high. Overrides `palette`.

- palette:

  Name of a palette passed to
  [`hexamap_palette()`](https://cisnet-cobras.github.io/hexamap/reference/hexamap_palette.md),
  used when `color_map` is `NULL`. Defaults to `"jet"`, the palette of
  Jalal and Burke (2020), or to the diverging `"Blue-Red 3"` when
  `midpoint` is set. `"viridis"` is a perceptually uniform alternative
  that is readable by people with color vision deficiencies.

- n_colors:

  Number of colors taken from `palette`.

- midpoint:

  Value at the center of a diverging color scale, such as 1 for rate
  ratios on a log scale or 0 for differences. The color limits are made
  symmetric around it.

- isolines:

  Which isolines to draw: any of `"age"`, `"period"` and `"cohort"`, or
  `NULL` for none.

- wrap_cohort_labels:

  Logical; whether cohort labels wrap around the bottom edge of the
  hexamap.

- tiles:

  `"hexagon"` for a hexamap or `"square"` for a traditional Lexis
  diagram, with periods on the x axis and ages on the y axis.

## Value

An object of class `hexamap_layout`, a list with:

- `cells`:

  Data frame with one row per age-period cell: `age`, `period`,
  `cohort`, `value`, tile center `x` and `y`, fill `color` (`NA` for
  cells that are not drawn), and `fraction`, the position of the value
  along the color scale from 0 to 1.

- `tiles`:

  `"hexagon"` or `"square"`.

- `tile`:

  List with the `x` and `y` offsets of the tile vertices relative to its
  center.

- `isolines`:

  List of data frames `age`, `period` and `cohort`, each with the
  isoline `value` and segment ends `x1`, `y1`, `x2`, `y2`.

- `scale`:

  List with the scale `type`, `limits`, `midpoint`, `colors` and
  colorbar `ticks` (data frame with `fraction` and `value`).

- `dims`:

  List with the first and last age and period, and the `interval`.

- `extent`:

  Numeric vector `xmin`, `xmax`, `ymin`, `ymax` covering the tiles and
  isolines.

- `wrap_cohort_labels`:

  The `wrap_cohort_labels` setting.

## Examples

``` r
rates <- outer(15:70, 1999:2018, function(a, p) (a - 10) * (p - 1990) / 40)
dimnames(rates) <- list(15:70, 1999:2018)
lay <- hexamap_layout(rates, isoline_interval = 10)
lay
#> <hexamap_layout> (hexagon tiles)
#>   Ages:    15-70  Periods: 1999-2018  Cohorts: 1929-2003
#>   Interval: 1
#>   Cells:   1120 (1120 drawn, 0 missing)
#>   Scale:   linear, 1.125 to 42, 100 colors
head(lay$cells)
#>   age period cohort value        x      y   color    fraction
#> 1  15   1999   1984 1.125 1731.185 -984.5 #000000 0.000000000
#> 2  16   1999   1983 1.350 1731.185 -983.5 #000000 0.005504587
#> 3  17   1999   1982 1.575 1731.185 -982.5 #00000B 0.011009174
#> 4  18   1999   1981 1.800 1731.185 -981.5 #00000B 0.016513761
#> 5  19   1999   1980 2.025 1731.185 -980.5 #000017 0.022018349
#> 6  20   1999   1979 2.250 1731.185 -979.5 #000017 0.027522936
plot(lay)
```
