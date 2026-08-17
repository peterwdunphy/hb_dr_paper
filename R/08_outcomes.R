# 08_outcomes.R -----------------------------------------------------------
# Build county-level outcome variables from the raw VEHSS pulls.
#
# PRIMARY OUTCOME: VTDR share = vision-threatening DR cases / all-stage DR
# cases, among adults with diabetes.
#
# Rationale (see analysis_plan.md section 2): VEHSS county estimates are driven
# by DIAGNOSED disease in Medicare/Medicaid claims, and the VEHSS authors
# adjusted their county random effects for ophthalmologists per capita. The
# LEVEL of diagnosed DR therefore partly measures detection, not burden. A
# severity share is far more robust: vision-threatening disease is symptomatic
# and presents for care even where routine screening is poor, so differential
# detection mostly inflates the mild-case denominator. It also divides out most
# of the diabetes-prevalence/SVI gradient the editor called "not novel".
#
# Two versions of the share are produced:
#   vtdr_share_crude   all-ages, VTDR cases / DR cases
#   vtdr_share_std     age-standardized, weighting each county's stratum-
#                      specific share by the NATIONAL distribution of DR cases
#                      across age strata. Standardizing to the case-mix of the
#                      diseased population is the right internal standard here;
#                      the 2000 US standard population is a general-population
#                      standard whose age bands do not align with the VEHSS
#                      strata (15-24 and 35-44 straddle the VEHSS cuts).
#                      This removes the age-cutoff problem entirely rather than
#                      trading one arbitrary threshold for another
#                      (Reviewer 1, comments 6 and 7).
#
# PARALLEL-CONSTRUCTION CONTROL: the identical share is built for glaucoma
# (vision affecting / all glaucoma). If access predicts the DR share but not
# the glaucoma share, the finding is DR-specific rather than an artifact of
# building a ratio out of VEHSS modeled estimates.
# -------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(readr)

ct <- cols(.default = "c", data_value = "d", numerator = "d",
           sample_size = "d", low_confidence_limit = "d",
           high_confidence_limit = "d")

# ---- diabetic retinopathy --------------------------------------------------
dr <- read_csv("data/vehss/vehss_dr_county_raw.csv", show_col_types = FALSE,
               col_types = ct) %>%
  filter(data_value_type == "Crude Prevalence") %>%
  mutate(stage = if_else(response == "Vision threatening stage", "vtdr", "dr")) %>%
  select(fips = locationid, county = locationdesc, state = stateabbr,
         age, stage, cases = numerator, dm_pop = sample_size,
         prev = data_value, lo = low_confidence_limit, hi = high_confidence_limit)

# wide by stage, keeping one denominator (identical across stages by construction)
dr_w <- dr %>%
  select(fips, county, state, age, stage, cases, dm_pop) %>%
  pivot_wider(names_from = stage, values_from = cases,
              names_glue = "{stage}_cases") %>%
  filter(!is.na(dr_cases), !is.na(vtdr_cases))

# national age weights = share of all US DR cases in each age stratum
age_w <- dr_w %>%
  filter(age != "All ages") %>%
  group_by(age) %>%
  summarise(dr_cases_nat = sum(dr_cases), .groups = "drop") %>%
  mutate(w = dr_cases_nat / sum(dr_cases_nat))

message("National DR case distribution used as the standard:")
print(age_w)

# age-standardized share: weight each county's stratum share by national weights
std <- dr_w %>%
  filter(age != "All ages") %>%
  mutate(share_a = vtdr_cases / dr_cases) %>%
  left_join(age_w %>% select(age, w), by = "age") %>%
  filter(!is.na(share_a)) %>%
  group_by(fips) %>%
  # renormalize in case a county is missing a stratum
  summarise(vtdr_share_std = sum(share_a * w) / sum(w),
            n_strata = n(), .groups = "drop")

crude <- dr_w %>%
  filter(age == "All ages") %>%
  transmute(fips, county, state,
            dm_pop,
            dr_cases, vtdr_cases,
            dr_prev_dm    = 100 * dr_cases / dm_pop,       # DR per 100 diabetics
            vtdr_prev_dm  = 100 * vtdr_cases / dm_pop,
            vtdr_share_crude = vtdr_cases / dr_cases)

# 65-84 stratum kept for the reviewer-facing age sensitivity analysis
age6584 <- dr_w %>%
  filter(age == "65-84 years") %>%
  transmute(fips,
            dr_cases_6584 = dr_cases, vtdr_cases_6584 = vtdr_cases,
            dm_pop_6584 = dm_pop,
            vtdr_share_6584 = vtdr_cases / dr_cases)

dr_out <- crude %>%
  left_join(std, by = "fips") %>%
  left_join(age6584, by = "fips")

# ---- glaucoma (parallel-construction control) ------------------------------
gl <- read_csv("data/vehss/vehss_glaucoma_county_raw.csv", show_col_types = FALSE,
               col_types = ct) %>%
  filter(data_value_type == "Crude Prevalence", age == "All ages") %>%
  mutate(stage = if_else(response == "Vision affecting glaucoma",
                         "vag", "allg")) %>%
  select(fips = locationid, stage, cases = numerator, pop = sample_size) %>%
  pivot_wider(names_from = stage, values_from = cases,
              names_glue = "{stage}_cases") %>%
  filter(!is.na(allg_cases), !is.na(vag_cases), allg_cases > 0) %>%
  transmute(fips,
            glauc_pop = pop,
            glauc_cases = allg_cases, vag_cases,
            glauc_share = vag_cases / allg_cases)

out <- dr_out %>% left_join(gl, by = "fips")

message("\nCounties with DR outcomes: ", nrow(out))
message("Counties with glaucoma control: ", sum(!is.na(out$glauc_share)))
message("\nOutcome distributions:")
print(summary(out %>% select(dr_prev_dm, vtdr_prev_dm, vtdr_share_crude,
                             vtdr_share_std, glauc_share)))

message("\nNational check (VEHSS published: DR 26.43% of diabetics, VTDR 5.06%,")
message("               VTDR share 1.84M/9.60M = 19.2%):")
message(sprintf("  pooled DR prev  = %.2f%%", 100 * sum(out$dr_cases) / sum(out$dm_pop)))
message(sprintf("  pooled VTDR prev= %.2f%%", 100 * sum(out$vtdr_cases) / sum(out$dm_pop)))
message(sprintf("  pooled VTDR share = %.1f%%", 100 * sum(out$vtdr_cases) / sum(out$dr_cases)))

write_csv(out, "data_derived/vehss_outcomes_county.csv")
message("\nWrote data_derived/vehss_outcomes_county.csv")
