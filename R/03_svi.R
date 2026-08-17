# 03_svi.R ----------------------------------------------------------------
# CDC/ATSDR Social Vulnerability Index, 2020, county level.
#
# Source: https://svi.cdc.gov/Documents/Data/2020/csv/states_counties/SVI_2020_US_county.csv
#
# WHY 2020 AND NOT 2022: SVI 2020 is built on the 2016-2020 ACS, so it brackets
# the 2021 analytic year, and it uses the OLD Connecticut county geography,
# matching VEHSS and the 2021 ACS. SVI 2022 switched CT to planning regions,
# which is the vintage mismatch that produced the workbook's 8 missing CT rows.
#
# Per the team's decision, SVI enters as a single overall control variable
# (RPL_THEMES, the overall percentile ranking), NOT as an index component and
# NOT as a study objective. The editor's objection was that "SVI predicts DR"
# is not a novel finding; it is here only to adjust for confounding.
#
# -999 is the CDC's missing-data sentinel and must be recoded, otherwise it
# silently becomes a large negative covariate.
# -------------------------------------------------------------------------

library(dplyr)
library(readr)

URL <- "https://svi.cdc.gov/Documents/Data/2020/csv/states_counties/SVI_2020_US_county.csv"
raw_path <- "data/SVI_2020_US_county.csv"
if (!file.exists(raw_path)) download.file(URL, raw_path, quiet = TRUE)

svi <- read_csv(raw_path, show_col_types = FALSE) %>%
  transmute(
    fips      = as.character(FIPS),
    svi_overall = RPL_THEMES,
    svi_ses     = RPL_THEME1,   # socioeconomic status
    svi_hhchar  = RPL_THEME2,   # household characteristics
    svi_minority= RPL_THEME3,   # racial & ethnic minority status
    svi_housing = RPL_THEME4    # housing type & transportation
  ) %>%
  mutate(across(starts_with("svi_"), ~ na_if(.x, -999)))

message("SVI 2020 counties: ", nrow(svi))
message("Missing svi_overall: ", sum(is.na(svi$svi_overall)))
message("Connecticut units: ", sum(substr(svi$fips, 1, 2) == "09"))

write_csv(svi, "data_derived/svi_2020_county.csv")
message("Wrote data_derived/svi_2020_county.csv")
