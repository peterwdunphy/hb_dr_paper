# 01_acs.R ----------------------------------------------------------------
# 2021 American Community Survey 5-year county estimates.
#
# WHY 2021 AND NOT 2023: the received workbook used 2023 ACS, which created two
# problems. First, the population denominator did not match the 2021 VEHSS
# prevalence year. Second, and less obviously, the 2023 vintage replaced
# Connecticut's eight counties with nine planning regions, which is what caused
# the 8 "missing" CT counties in the workbook's SVI merge. The 2021 vintage
# still uses the old CT county FIPS (09001-09015), matching VEHSS and SVI 2020.
# Fixing the year fixes Connecticut. No crosswalk is needed.
#
# Requires a Census API key in ~/.Renviron as CENSUS_API_KEY.
#   Sys.setenv(CENSUS_API_KEY = "...")  or  tidycensus::census_api_key(..., install = TRUE)
# -------------------------------------------------------------------------

library(tidycensus)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

if (Sys.getenv("CENSUS_API_KEY") == "") {
  stop("CENSUS_API_KEY not set. Put it in ~/.Renviron, do not hard-code it.")
}

YEAR <- 2021

# --- total population and age structure (B01001, sex by age) ----------------
b01001 <- get_acs(geography = "county", table = "B01001",
                  year = YEAR, survey = "acs5", cache_table = TRUE)

# B01001 age bands, mapped to the VEHSS strata boundaries we care about.
# Male 003-025, Female 027-049; the band structure is identical across sexes.
band_of <- function(v) {
  n <- as.integer(str_sub(v, -3))
  n <- if_else(n >= 27, n - 24L, n)          # fold female onto male numbering
  case_when(
    n %in% 3:6   ~ "a0_17",     # <5, 5-9, 10-14, 15-17
    n %in% 7:13  ~ "a18_39",    # 18-19, 20, 21, 22-24, 25-29, 30-34, 35-39
    n %in% 14:19 ~ "a40_64",    # 40-44 ... 60-61, 62-64
    n %in% 20:24 ~ "a65_84",    # 65-66, 67-69, 70-74, 75-79, 80-84
    n == 25      ~ "a85p",
    TRUE         ~ NA_character_             # 001 total, 002/026 sex totals
  )
}

age <- b01001 %>%
  filter(variable != "B01001_001") %>%
  mutate(band = band_of(variable)) %>%
  filter(!is.na(band)) %>%
  group_by(GEOID, band) %>%
  summarise(n = sum(estimate), .groups = "drop") %>%
  pivot_wider(names_from = band, values_from = n)

total <- b01001 %>%
  filter(variable == "B01001_001") %>%
  select(GEOID, NAME, total_pop = estimate)

acs <- total %>%
  left_join(age, by = "GEOID") %>%
  mutate(
    pct_65plus = 100 * (a65_84 + a85p) / total_pop,
    pct_under40 = 100 * (a0_17 + a18_39) / total_pop
  )

# --- race / ethnicity (B03002) for the descriptive high-priority profile -----
b03002 <- get_acs(geography = "county", table = "B03002",
                  year = YEAR, survey = "acs5", cache_table = TRUE)

race <- b03002 %>%
  filter(variable %in% c("B03002_001", "B03002_003", "B03002_004",
                         "B03002_005", "B03002_006", "B03002_007",
                         "B03002_012")) %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  transmute(
    GEOID,
    pct_nh_white  = 100 * B03002_003 / B03002_001,
    pct_nh_black  = 100 * B03002_004 / B03002_001,
    pct_nh_aian   = 100 * B03002_005 / B03002_001,
    pct_nh_asian  = 100 * B03002_006 / B03002_001,
    pct_nh_nhpi   = 100 * B03002_007 / B03002_001,
    pct_hispanic  = 100 * B03002_012 / B03002_001
  )

out <- acs %>% left_join(race, by = "GEOID") %>% rename(fips = GEOID)

message("ACS ", YEAR, " counties: ", nrow(out))
message("Connecticut units (expect 8 old-style counties): ",
        sum(str_sub(out$fips, 1, 2) == "09"))
print(out %>% filter(str_sub(fips, 1, 2) == "09") %>% select(fips, NAME))

write_csv(out, "data_derived/acs_2021_county.csv")
message("Wrote data_derived/acs_2021_county.csv")
