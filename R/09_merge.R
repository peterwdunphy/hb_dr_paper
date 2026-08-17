# 09_merge.R --------------------------------------------------------------
# Assemble the county-level analytic file, one row per county, merged on
# 5-digit FIPS.
#
# Inputs (all produced by scripts 01-08):
#   data_derived/vehss_outcomes_county.csv    outcomes (VEHSS 2021 / 2022)
#   data_derived/acs_2021_county.csv          population, age, race/ethnicity
#   data_derived/svi_2020_county.csv          social vulnerability (control)
#   data_derived/ahrf_2021_county.csv         eye-care workforce, rurality
#   data_derived/county_centroids.csv         pop-weighted centroids, land area
#   data_derived/county_nearest_program.csv   drive time to nearest program
#   data_derived/programs_geocoded.csv        program roster -> has_program
#
# Output: data_derived/county_analytic_2021.csv
# -------------------------------------------------------------------------

library(dplyr)
library(readr)
library(stringr)

rd <- function(f, ...) read_csv(f, show_col_types = FALSE,
                                col_types = cols(fips = "c", .default = col_guess()), ...)

out    <- rd("data_derived/vehss_outcomes_county.csv")
acs    <- rd("data_derived/acs_2021_county.csv")
svi    <- rd("data_derived/svi_2020_county.csv")
ahrf   <- rd("data_derived/ahrf_2021_county.csv")
cen    <- rd("data_derived/county_centroids.csv")
near   <- rd("data_derived/county_nearest_program.csv")

progs <- read_csv("data_derived/programs_geocoded.csv", show_col_types = FALSE) %>%
  mutate(yr = suppressWarnings(as.numeric(accred_year))) %>%
  filter(is.na(yr) | yr <= 2021)          # 2021 cross-section

prog_counties <- unique(progs$county_fips)
prog_n <- progs %>% count(county_fips, name = "n_programs")

df <- out %>%
  select(-county, -state) %>%
  left_join(acs %>% select(-NAME), by = "fips") %>%
  left_join(svi,  by = "fips") %>%
  left_join(ahrf %>% select(-ahrf_county_name), by = "fips") %>%
  left_join(cen %>% select(fips, county, state, pop2020, pw_lat, pw_lon,
                           land_area_sqmi), by = "fips") %>%
  left_join(near %>% select(fips, nearest_prog_id, nearest_drive_min,
                            nearest_gc_miles), by = "fips") %>%
  left_join(prog_n, by = c("fips" = "county_fips")) %>%
  mutate(
    has_program   = fips %in% prog_counties,
    n_programs    = coalesce(n_programs, 0L),
    # counties containing a program have zero travel burden by definition
    drive_min     = if_else(has_program, 0, nearest_drive_min),
    log_drive     = log1p(drive_min),
    # --- densities ---------------------------------------------------------
    pop_density   = total_pop / land_area_sqmi,
    log_pop_dens  = log(pop_density),
    ophth_per_100k = 1e5 * ophth_patient_care / total_pop,
    optom_per_100k = 1e5 * optom_npi / total_pop,
    # any eye-care provider at all: many rural counties have zero
    any_ophth     = ophth_patient_care > 0,
    # --- rurality ----------------------------------------------------------
    rucc          = as.integer(rucc_2013),
    metro         = cbsa_ind == 1,
    # --- diabetes burden, kept OUT of the outcome but useful as a covariate -
    dm_prev       = 100 * dm_pop / total_pop
  )

message("Merged rows: ", nrow(df))

# ---- completeness report ---------------------------------------------------
key_vars <- c("vtdr_share_crude", "vtdr_share_std", "dr_prev_dm", "total_pop",
              "svi_overall", "ophth_per_100k", "optom_per_100k",
              "drive_min", "pop_density", "rucc")
message("\nMissingness on analytic variables:")
for (v in key_vars) {
  message(sprintf("  %-18s missing %4d / %d", v, sum(is.na(df[[v]])), nrow(df)))
}

message("\nCounties with no routable program (island/remote), by state:")
print(df %>% filter(is.na(drive_min)) %>% count(state), n = 20)

# ---- complete-case analytic sample -----------------------------------------
analytic <- df %>% filter(if_all(all_of(key_vars), ~ !is.na(.)))
message("\nComplete-case analytic N: ", nrow(analytic))

write_csv(df, "data_derived/county_analytic_2021.csv")
write_csv(analytic, "data_derived/county_analytic_2021_complete.csv")
message("Wrote data_derived/county_analytic_2021.csv (all rows) and _complete.csv")

# ---- quick descriptive orientation -----------------------------------------
message("\nDrive time to nearest program (minutes), counties without a program:")
print(summary(df$drive_min[!df$has_program]))
message("\nCounties containing >=1 program: ", sum(df$has_program))
