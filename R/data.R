#' Drug overdose deaths among white men in the United States, 1999-2018
#'
#' Accidental drug overdose death rates among non-Hispanic and Hispanic white
#' men in the United States by single year of age (15-70) and calendar year
#' (1999-2018), the data shown in Figure 1C/D of Jalal and Burke (2020).
#' `overdose_white_men` is the age-by-period matrix of rates used by
#' [hexamap()]; `overdose_white_men_long` has one row per age and year, with
#' the deaths, population and reliability of each rate.
#'
#' Deaths are those with an underlying cause of death coded as ICD-10 X40-X44
#' (accidental poisoning by drugs, medicaments and biological substances).
#'
#' Following the CDC WONDER data use restrictions, death counts of 9 or fewer
#' and the rates based on them are not included: these 20 cells are `NA` and
#' flagged `"suppressed"`. CDC WONDER does not report rates based on fewer
#' than 20 deaths, which it considers unreliable; these 63 rates were
#' computed from the deaths and population and flagged `"unreliable"`.
#'
#' @format `overdose_white_men`: a numeric matrix with 56 rows (ages 15 to
#'   70) and 20 columns (years 1999 to 2018) of deaths per 100,000 men, with
#'   the ages and years as dimnames.
#'
#'   `overdose_white_men_long`: a data frame with 1,120 rows and 6 columns:
#'   \describe{
#'     \item{`year`}{Calendar year of death.}
#'     \item{`age`}{Age at death, in single years.}
#'     \item{`deaths`}{Number of deaths (`NA` when 9 or fewer).}
#'     \item{`population`}{Population of white men of that age, from the
#'       bridged-race estimates used by CDC WONDER.}
#'     \item{`rate`}{Deaths per 100,000 (`NA` when deaths are 9 or fewer).}
#'     \item{`flag`}{Factor: `"reliable"` (20 or more deaths),
#'       `"unreliable"` (10 to 19 deaths) or `"suppressed"` (9 or fewer).}
#'   }
#' @source Centers for Disease Control and Prevention, National Center for
#'   Health Statistics. Multiple Cause of Death 1999-2018 on CDC WONDER
#'   Online Database, released 2020. Data are from the Multiple Cause of
#'   Death Files, 1999-2018, as compiled from data provided by the 57 vital
#'   statistics jurisdictions through the Vital Statistics Cooperative
#'   Program. <https://wonder.cdc.gov/mcd-icd10.html>. The query is described
#'   in `data-raw/overdose.R` in the package source.
#' @references Jalal H, Burke DS. Hexamaps for age-period-cohort data
#'   visualization and implementation in R. *Epidemiology*.
#'   2020;31(6):e47-e49. \doi{10.1097/EDE.0000000000001236}
#' @examples
#' hexamap(overdose_white_men, colorbar_range = c(0, 40),
#'         scale_units = "Deaths per 100,000",
#'         highlight = list(cohort = 1975))
#'
#' table(overdose_white_men_long$flag)
"overdose_white_men"

#' @rdname overdose_white_men
"overdose_white_men_long"
