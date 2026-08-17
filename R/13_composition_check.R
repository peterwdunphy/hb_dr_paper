# 13_composition_check.R --------------------------------------------------
# DECISIVE VALIDITY CHECK: is the county-level severity signal real, or is it
# an artifact of how VEHSS builds county estimates?
#
# THE PROBLEM. Lundeen et al. construct county estimates by POST-STRATIFICATION:
# "We estimated standardized rates by county as the expected prevalence for that
# county assuming the national distribution of age, sex and gender, and race."
# National race- and age-specific rates are applied to each county's actual
# demographic composition, then adjusted by a county random effect from claims.
#
# Consequence: a county's racial composition mechanically moves its estimate,
# because national race-specific DR and VTDR rates differ. Any regression of an
# "All races" county estimate on county racial composition (or on SVI, which
# contains a racial/ethnic minority theme) is therefore partly circular.
#
# THE TEST. Hold race and age FIXED. Using only non-Hispanic White adults aged
# 65-84, composition is constant by construction, so whatever county-to-county
# variation remains is the county random effect, i.e. the genuinely local
# signal. If SVI still predicts the severity share within this fixed stratum,
# the finding is real. If the variation collapses, the earlier result was
# compositional and must not be reported as substantive.
# -------------------------------------------------------------------------

library(jsonlite)
library(dplyr)
library(readr)
library(tidyr)

fetch <- function(where) {
  pages <- list()
  for (i in 1:6) {
    u <- paste0("https://data.cdc.gov/resource/qeru-k2y2.json?$where=",
                utils::URLencode(where, reserved = TRUE),
                "&$limit=50000&$offset=", format((i - 1) * 50000, scientific = FALSE),
                "&$order=locationid,response")
    p <- fromJSON(u, flatten = TRUE)
    if (length(p) == 0 || nrow(p) == 0) break
    pages[[i]] <- p
    if (nrow(p) < 50000) break
  }
  bind_rows(pages)
}

base <- paste0("geographiclevel='County' AND yearstart='2021' ",
               "AND question='Prevalence of Diabetic Retinopathy' ",
               "AND sex='Both sexes' AND riskfactorresponse='Yes' ",
               "AND data_value_type='Crude Prevalence' ",
               "AND response IN ('All DR stages','Vision threatening stage')")

get_stratum <- function(race, age) {
  x <- fetch(paste0(base, " AND raceethnicity='", race, "' AND age='", age, "'"))
  if (nrow(x) == 0) return(tibble())
  x %>%
    mutate(stage = if_else(response == "Vision threatening stage", "vtdr", "dr"),
           num = as.numeric(numerator), den = as.numeric(sample_size)) %>%
    select(fips = locationid, stage, num, den) %>%
    pivot_wider(names_from = stage, values_from = c(num, den)) %>%
    filter(!is.na(num_dr), !is.na(num_vtdr), num_dr > 0) %>%
    transmute(fips, dr_cases = num_dr, vtdr_cases = num_vtdr,
              dm_pop = den_dr, share = num_vtdr / num_dr)
}

strata <- list(
  `White NH 65-84`    = c("White, non-Hispanic", "65-84 years"),
  `Black NH 65-84`    = c("Black, non-Hispanic", "65-84 years"),
  `Hispanic 65-84`    = c("Hispanic, any race", "65-84 years"),
  `White NH 40-64`    = c("White, non-Hispanic", "40-64 years")
)

d <- read_csv("data_derived/county_analytic_2021_complete.csv",
              show_col_types = FALSE,
              col_types = cols(fips = "c", .default = col_guess()))
sup <- read_csv("data_derived/county_nearest_ophthalmologist.csv",
                show_col_types = FALSE,
                col_types = cols(fips = "c", supply_fips = "c", .default = col_guess()))
cov <- d %>%
  select(fips, svi_overall, svi_ses, svi_minority, ophth_per_100k, optom_per_100k,
         log_pop_dens, rucc, log_drive, pct_nh_white) %>%
  left_join(sup %>% select(fips, supply_drive_min), by = "fips")

out <- list()
for (nm in names(strata)) {
  s <- get_stratum(strata[[nm]][1], strata[[nm]][2])
  if (nrow(s) == 0) { message(nm, ": no rows"); next }
  message(sprintf("\n%-16s counties=%d  mean share=%.4f  sd=%.5f  CV=%.4f  range=%.4f-%.4f",
                  nm, nrow(s), mean(s$share), sd(s$share),
                  sd(s$share) / mean(s$share), min(s$share), max(s$share)))

  m <- s %>% inner_join(cov, by = "fips") %>%
    filter(!is.na(svi_overall), !is.na(supply_drive_min), is.finite(log_pop_dens)) %>%
    mutate(z_svi = as.numeric(scale(svi_overall)),
           z_ophth = as.numeric(scale(ophth_per_100k)),
           z_optom = as.numeric(scale(optom_per_100k)),
           z_popdens = as.numeric(scale(log_pop_dens)),
           z_rucc = as.numeric(scale(rucc)),
           z_acad = as.numeric(scale(log_drive)),
           z_supply = as.numeric(scale(log1p(supply_drive_min))),
           drnon = pmax(dr_cases - vtdr_cases, 0))
  f <- glm(cbind(vtdr_cases, drnon) ~ z_svi + z_ophth + z_optom + z_popdens +
             z_rucc + z_acad + z_supply, data = m, family = quasibinomial())
  cf <- summary(f)$coefficients
  message(sprintf("   SVI beta = %+.4f (se %.4f, p = %.3g)   n = %d",
                  cf["z_svi", 1], cf["z_svi", 2], cf["z_svi", 4], nrow(m)))
  message(sprintf("   academic drive beta = %+.4f (p = %.3g)",
                  cf["z_acad", 1], cf["z_acad", 4]))
  out[[nm]] <- tibble(stratum = nm, n = nrow(m), mean_share = mean(s$share),
                      cv = sd(s$share) / mean(s$share),
                      beta_svi = cf["z_svi", 1], se_svi = cf["z_svi", 2],
                      p_svi = cf["z_svi", 4],
                      beta_acad = cf["z_acad", 1], p_acad = cf["z_acad", 4])
}

res <- bind_rows(out)
message("\n===== SUMMARY =====")
print(as.data.frame(res %>% mutate(across(where(is.numeric), ~ signif(.x, 4)))))

# For contrast: the same coefficient on the composition-driven "All races" file
dall <- d %>% left_join(sup %>% select(fips, supply_drive_min), by = "fips") %>%
  filter(!is.na(supply_drive_min)) %>%
  mutate(z_svi = as.numeric(scale(svi_overall)),
         drnon = pmax(dr_cases - vtdr_cases, 0))
fall <- glm(cbind(vtdr_cases, drnon) ~ z_svi + scale(ophth_per_100k) +
              scale(optom_per_100k) + scale(log_pop_dens) + scale(rucc) +
              scale(log_drive) + scale(log1p(supply_drive_min)),
            data = dall, family = quasibinomial())
message(sprintf("\nAll races, all ages (composition varies): SVI beta = %+.4f",
                coef(fall)["z_svi"]))

write_csv(res, "output/composition_check.csv")
message("Wrote output/composition_check.csv")
