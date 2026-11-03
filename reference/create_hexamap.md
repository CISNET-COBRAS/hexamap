# Create a hexamap (eAppendix interface)

Wrapper around
[`hexamap()`](https://cisnet-cobras.github.io/hexamap/reference/hexamap.md)
that keeps the argument names used in the eAppendix of Jalal and Burke
(2020), so published code runs unchanged. It uses the "jet" palette of
the paper by default. New code should call
[`hexamap()`](https://cisnet-cobras.github.io/hexamap/reference/hexamap.md)
directly.

## Usage

``` r
create_hexamap(
  data,
  first_age,
  first_period,
  interval,
  color_scale = NULL,
  palette = "jet",
  ...
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

- color_scale:

  Numeric vector of length 2 with the lower and upper limits of the
  color scale. Passed to `colorbar_range` in
  [`hexamap()`](https://cisnet-cobras.github.io/hexamap/reference/hexamap.md).

- palette:

  Palette name passed to
  [`hexamap()`](https://cisnet-cobras.github.io/hexamap/reference/hexamap.md).
  Defaults to `"jet"`, as in the paper.

- ...:

  Further arguments passed to
  [`hexamap()`](https://cisnet-cobras.github.io/hexamap/reference/hexamap.md).

## Value

The `hexamap_layout` object returned by
[`hexamap_layout()`](https://cisnet-cobras.github.io/hexamap/reference/hexamap_layout.md),
invisibly.

## Examples

``` r
rates <- outer(15:70, 1999:2018, function(a, p) (a - 10) * (p - 1990) / 40)
create_hexamap(data = rates,
               first_age = 15,
               first_period = 1999,
               interval = 1,
               first_age_isoline = 20,
               first_period_isoline = 2000,
               isoline_interval = 10,
               color_scale = c(0, 40))
```
