# Accidental drug overdose death rates among white men in the United States,
# 1999-2018, ages 15-70 (Jalal and Burke 2020, Figure 1C/D).
#
# Source: CDC WONDER, Multiple Cause of Death 1999-2018 (bridged race),
# https://wonder.cdc.gov/mcd-icd10.html. Query:
#   - Group results by: Year, Single-Year Ages
#   - Race: White; Gender: Male; all Hispanic origins, all states
#   - Underlying cause of death, ICD-10 codes: X40-X44 (accidental poisoning
#     by drugs, medicaments and biological substances)
#   - Export as a tab-delimited file to
#     data-raw/cdc_wonder_white_male_x40_x44_1999_2018.txt
#
# The raw export is not committed (see .gitignore): the CDC WONDER data use
# restrictions do not allow publishing death counts of 9 or fewer, or rates
# based on them. Those cells are stored as NA and flagged "suppressed".
# WONDER flags rates based on fewer than 20 deaths as "Unreliable" and does
# not report them; here they are computed from deaths and population and
# flagged "unreliable".

raw_file <- "data-raw/cdc_wonder_white_male_x40_x44_1999_2018.txt"
suppress_below <- 10 # deaths
unreliable_below <- 20 # deaths
ages <- 15:70
years <- 1999:2018

raw <- utils::read.delim(raw_file, colClasses = "character")
# keep ages 15-70 first: ages 85+ have no population ("Not Applicable")
raw <- raw[raw$Single.Year.Ages.Code %in% ages & raw$Year.Code %in% years, ]

long <- data.frame(year = as.integer(raw$Year.Code),
                   age = as.integer(raw$Single.Year.Ages.Code),
                   deaths = as.integer(raw$Deaths),
                   population = as.numeric(raw$Population))
long <- long[order(long$year, long$age), ]
stopifnot(nrow(long) == length(ages) * length(years),
          !anyNA(long$deaths), !anyNA(long$population))

long$rate <- long$deaths / long$population * 1e5
long$flag <- ifelse(long$deaths < suppress_below, "suppressed",
                    ifelse(long$deaths < unreliable_below, "unreliable", "reliable"))
long$flag <- factor(long$flag, levels = c("reliable", "unreliable", "suppressed"))
long$deaths[long$flag == "suppressed"] <- NA
long$rate[long$flag == "suppressed"] <- NA
rownames(long) <- NULL

# reported rates agree with WONDER's, which are rounded to 3 decimals
reported <- suppressWarnings(as.numeric(raw$Crude.Rate))
reported <- reported[match(paste(long$year, long$age),
                           paste(raw$Year.Code, raw$Single.Year.Ages.Code))]
stopifnot(max(abs(long$rate - reported), na.rm = TRUE) < 1e-3)

overdose_white_men_long <- long
overdose_white_men <- hexamap::as_apc_matrix(long, age = "age", period = "year",
                                             value = "rate")

usethis::use_data(overdose_white_men, overdose_white_men_long,
                  overwrite = TRUE, compress = "xz")
