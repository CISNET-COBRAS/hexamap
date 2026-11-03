# Draw a hexamap layout with base graphics

Draws a layout computed by
[`hexamap_layout()`](https://cisnet-cobras.github.io/hexamap/reference/hexamap_layout.md)
on the current device. The hexamap, its isoline labels and the colorbar
are sized to fit the current plotting region, so hexamaps can be
combined with `par(mfrow = )` or
[`graphics::layout()`](https://rdrr.io/r/graphics/layout.html).

## Usage

``` r
# S3 method for class 'hexamap_layout'
plot(
  x,
  title_text = NULL,
  subtitle_text = NULL,
  title_adj = 0.5,
  labels = c(age = "A: ", period = "P: ", cohort = "C: "),
  highlight = NULL,
  line_width = 0.5,
  line_color = "grey",
  label_size = 0.7,
  label_color = "black",
  scale_units = "Rate",
  legend = TRUE,
  colorbar_width = 0.15,
  colorbar_height = 0.5,
  na_color = "grey90",
  na_label = "Missing",
  hex_border = "fill",
  hex_border_width = 0.5,
  ...
)
```

## Arguments

- x:

  A `hexamap_layout` object.

- title_text, subtitle_text:

  Title and subtitle, drawn above the hexamap. `NULL` or `""` for none.

- title_adj:

  Horizontal alignment of the title and subtitle: 0 for left, 0.5 for
  centered, 1 for right.

- labels:

  Named character vector with the prefixes of the `age`, `period` and
  `cohort` isoline labels. Missing names keep their default, so
  `labels = c(period = "Year: ")` only changes the period labels.

- highlight:

  List of ages, periods and/or cohorts to draw as highlighted lines,
  with optional `col` and `lwd`, e.g. `list(cohort = 1975)` or
  `list(age = c(30, 50), col = "white", lwd = 2)`. Values need not be
  isolines but must be within the range of the data. Their labels are
  drawn in bold.

- line_width, line_color:

  Width and color of the isolines.

- label_size, label_color:

  Size and color of the isoline and colorbar labels.

- scale_units:

  Text displayed above the colorbar.

- legend:

  Logical; whether to draw the colorbar.

- colorbar_width:

  Width of the colorbar, in inches.

- colorbar_height:

  Height of the colorbar, as a fraction of the height of the hexamap.

- na_color:

  Color of cells with missing values (for example, suppressed counts).
  `NA` leaves them blank.

- na_label:

  Label of the missing-value key shown below the colorbar when some
  cells are missing.

- hex_border:

  Color of the hexagon borders. The default, `"fill"`, draws each border
  in the hexagon's own color, which avoids the thin white seams that
  many viewers show between adjacent tiles. Use `NA` for no border, or a
  color such as `"white"` to outline the tiles.

- hex_border_width:

  Line width of the hexagon borders.

- ...:

  Not used.

## Value

`x`, invisibly.

## Examples

``` r
rates <- outer(15:70, 1999:2018, function(a, p) (a - 10) * (p - 1990) / 40)
lay <- hexamap_layout(rates, first_age = 15, first_period = 1999,
                      interval = 1, isoline_interval = 10)
plot(lay, title_text = "Simulated rates", scale_units = "Rate")


# full-word labels and a highlighted birth cohort
plot(lay, labels = c(age = "Age ", period = "", cohort = "Born "),
     highlight = list(cohort = 1975, col = "white", lwd = 2))
```
