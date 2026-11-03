## Submission

This is a new submission. 'hexamap' implements the hexamap method for
visualizing age-period-cohort data described in Jalal and Burke (2020)
<doi:10.1097/EDE.0000000000001236>.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  New submission

## Test environments

* local macOS (aarch64), R 4.5.3
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel,
  release, oldrel-1)
* win-builder: R-devel
* mac-builder: R-release
* R-hub: linux, macos-arm64, windows, ubuntu-clang

## Notes for reviewers

* The bundled dataset `overdose_white_men` comes from CDC WONDER. Following
  its data use restrictions, death counts of 9 or fewer and the rates based
  on them are not included (they are `NA`).
* Visual regression tests (vdiffr) and tests that need 'ggplot2' are skipped
  on CRAN or when the package is not installed.
