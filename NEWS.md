# hexamap 0.1.2

## Bug fixes

* The colorbar no longer draws shifted, overlapping color blocks
  (`2:ncol+1` was evaluated as `3:(ncol+1)`).
* Every palette color can now appear on the hexagons. Previously the top
  color was never used because `ncol` breaks gave only `ncol - 1` bins.
* `NA` cells are skipped instead of making the default color range fail
  with "'from' must be a finite number". Data with no finite values give
  a clear error.
* The log color scale no longer fails on zeros: values `<= 0` are drawn in
  the lowest color, with a warning. A non-positive `colorbar_range` with a
  log scale gives an error.
* `colorbar_scale` accepts `"linear"`, `"log"` and `"quantile"` (and the
  legacy names `"Normal"`, `"Log"`, `"Percentile"`) in any letter case.
  Unknown names give a clear error instead of "object 'data_cutpoints' not
  found".
* Colorbar tick labels are correct for palettes of any length (they
  assumed 100 colors). They now show 3 significant digits, so small rates
  are no longer rounded to 0.
* Percentile ticks are only drawn for the quantile scale.
* `hexamap()` restores the graphical parameters it changes.
* An empty `subtitle_text` is no longer drawn as "NA".
* `scale_units` is now displayed above the colorbar, as documented.
* Inputs are validated with informative errors. Data frames of numeric
  columns are accepted.

# hexamap 0.1.1

* Added GitHub Actions R CMD check on macOS, Windows and Ubuntu
  (R release, devel and oldrel-1).

# hexamap 0.1.0

* Initial package skeleton. `hexamap()` is the general version of the
  function from the original scripts; the figure-specific variant
  (`hexagon_apc_functions.R`) was removed.
* Added `create_hexamap()`, a wrapper that keeps the code from the
  Epidemiology (2020) eAppendix working.
