# Color palettes for hexamaps

Returns `n` colors from a named palette. The default, `"jet"`, is the
palette used in Jalal and Burke (2020). Any palette from
[`grDevices::hcl.pals()`](https://rdrr.io/r/grDevices/palettes.html) can
also be used, such as `"viridis"` (perceptually uniform and readable by
people with color vision deficiencies), `"Inferno"`, `"YlOrRd"`, or the
diverging `"Blue-Red 3"`.

## Usage

``` r
hexamap_palette(name = "jet", n = 100)
```

## Arguments

- name:

  Name of the palette: `"jet"` or a name from
  [`grDevices::hcl.pals()`](https://rdrr.io/r/grDevices/palettes.html).
  Matching ignores case and spaces.

- n:

  Number of colors.

## Value

A character vector of `n` colors, from low to high.

## Examples

``` r
hexamap_palette("jet", 5)
#> [1] "#000000" "#001FFF" "#3FFFBF" "#FF9E00" "#7F0000"
hexamap_palette("viridis", 5)
#> [1] "#4B0055" "#00588B" "#009B95" "#53CC67" "#FDE333"
hexamap_palette("Blue-Red 3", 5)
#> [1] "#002F70" "#879FDB" "#F6F6F6" "#DA8A8B" "#5F1415"
```
