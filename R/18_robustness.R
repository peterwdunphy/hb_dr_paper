# 18_robustness.R ---------------------------------------------------------
# Robustness checks raised in internal review of the disparities manuscript.
#
# R1. WITHIN-COUNTY CLUSTERING. The analytic file stacks up to six race-by-age
#     cells per county, and every county-level covariate (SVI, drive time,
#     provider density) is repeated across a county's cells. The quasibinomial
#     dispersion parameter rescales all standard errors by a single scalar; it
#     does not model the correlation induced by repeated counties. Effective
#     sample size for a COUNTY-level covariate is nearer 3,111 counties than
#     13,035 cells. We therefore report cluster-robust (CR0) standard errors
#     clustered on county for all county-level covariates.
#
# R2. AGE-BAND COMPARISON. The age contrast must be estimated as an
#     interaction within a single model, not by comparing coefficients from two
#     separately fitted models with a two-sample z test: the two age bands are
#     drawn from the SAME counties, so the separate estimates are correlated and
#     that test is invalid. Note the clustered SE for the interaction is SMALLER
#     than the model-based one, because the age contrast is identified WITHIN
#     county and shared county-level noise cancels.
#
# R3. SVI THEME 3. The overall SVI contains a racial and ethnic minority status
#     theme. Even though within-stratum estimation removes the post-
#     stratification artifact, it is worth showing the association survives
#     without that component.
#
# R4. PROVIDER DENSITY. VEHSS already adjusted its county random effects for
#     ophthalmologists per capita, so our adjustment is partly a second pass.
#     We report the SVI coefficient with and without those terms.
#
# R5. MISSING CELLS. 13,035 of a possible 18,714 county-stratum cells.
# -------------------------------------------------------------------------

suppressMessages({library(dplyr); library(readr); library(sandwich)})
z <- function(x) as.numeric(scale(x))

d    <- read_csv("data_derived/county_analytic_2021_complete.csv", show_col_types = FALSE,
                 col_types = cols(fips = "c", .default = col_guess()))
long <- read_csv("data_derived/vehss_strata_long.csv", show_col_types = FALSE,
                 col_types = cols(fips = "c", .default = col_guess()))
sup  <- read_csv("data_derived/county_nearest_ophthalmologist.csv", show_col_types = FALSE,
                 col_types = cols(fips = "c", supply_fips = "c", .default = col_guess()))

dat <- long %>%
  inner_join(d %>% select(fips, svi_overall, svi_ses, svi_hhchar, svi_minority,
                          svi_housing, ophth_per_100k, optom_per_100k,
                          log_pop_dens, rucc, log_drive), by = "fips") %>%
  left_join(sup %>% select(fips, supply_drive_min), by = "fips") %>%
  filter(!is.na(svi_overall), !is.na(supply_drive_min), is.finite(log_pop_dens)) %>%
  mutate(stratum = paste(race, age), drnon = pmax(dr_cases - vtdr_cases, 0),
         age65 = as.integer(age == "65-84 years"),
         z_svi = z(svi_overall), z_acad = z(log_drive),
         z_supply = z(log1p(supply_drive_min)), z_ophth = z(ophth_per_100k),
         z_optom = z(optom_per_100k), z_popdens = z(log_pop_dens), z_rucc = z(rucc),
         svi_no3 = rowMeans(cbind(svi_ses, svi_hhchar, svi_housing), na.rm = TRUE),
         z_no3 = z(svi_no3), z_t1 = z(svi_ses), z_t2 = z(svi_hhchar),
         z_t3 = z(svi_minority), z_t4 = z(svi_housing))

CT <- "z_acad + z_supply + z_ophth + z_optom + z_popdens + z_rucc"
Y  <- "cbind(vtdr_cases, drnon)"
fitq <- function(rhs, dd = dat) glm(as.formula(paste(Y, "~", rhs)), dd, family = quasibinomial())
report <- function(m, term, dd = dat, label = "") {
  V  <- sandwich::vcovCL(m, cluster = dd$fips, type = "HC0")
  b  <- coef(m)[term]; se_m <- summary(m)$coefficients[term, 2]; se_c <- sqrt(V[term, term])
  tibble(spec = label, term = term, beta = b, se_model = se_m, se_cluster = se_c,
         infl = se_c / se_m, p_cluster = 2 * pnorm(-abs(b / se_c)),
         lo = b - 1.96 * se_c, hi = b + 1.96 * se_c)
}

m0 <- fitq(paste("z_svi +", CT, "+ factor(stratum)"))
r1 <- bind_rows(report(m0, "z_svi", label = "SVI, primary"),
                report(m0, "z_acad", label = "Academic drive time, primary"))
message("=== R1: model-based vs cluster-robust standard errors ==="); print(as.data.frame(r1))

mi <- fitq(paste("z_svi*age65 +", CT, "+ factor(race)"))
r2 <- report(mi, "z_svi:age65", label = "SVI x age65 interaction")
message("\n=== R2: age-band contrast as a within-model interaction ==="); print(as.data.frame(r2))

r3 <- bind_rows(
  report(fitq(paste("z_svi +", CT, "+ factor(stratum)")), "z_svi", label = "Overall SVI (4 themes)"),
  report(fitq(paste("z_no3 +", CT, "+ factor(stratum)")), "z_no3", label = "SVI excluding Theme 3"),
  report(fitq(paste("z_t1 +", CT, "+ factor(stratum)")), "z_t1", label = "Theme 1 socioeconomic alone"),
  report(fitq(paste("z_t3 +", CT, "+ factor(stratum)")), "z_t3", label = "Theme 3 minority status alone"))
mj <- fitq(paste("z_t1 + z_t2 + z_t3 + z_t4 +", CT, "+ factor(stratum)"))
r3 <- bind_rows(r3, lapply(c("z_t1","z_t2","z_t3","z_t4"),
                           function(t) report(mj, t, label = "all four jointly")) %>% bind_rows())
message("\n=== R3: SVI with and without Theme 3 ==="); print(as.data.frame(r3))

r4 <- bind_rows(
  report(fitq(paste("z_svi +", CT, "+ factor(stratum)")), "z_svi", label = "with provider density"),
  report(fitq("z_svi + z_acad + z_supply + z_popdens + z_rucc + factor(stratum)"),
         "z_svi", label = "without provider density"))
message("\n=== R4: provider-density double adjustment ==="); print(as.data.frame(r4))

message("\n=== R5: cell coverage ===")
message("cells ", nrow(dat), " of ", n_distinct(dat$fips) * 6, " possible across ",
        n_distinct(dat$fips), " counties")
print(dat %>% count(race, age))

dir.create("output", showWarnings = FALSE)
write_csv(bind_rows(r1 %>% mutate(block="R1"), r2 %>% mutate(block="R2"),
                    r3 %>% mutate(block="R3"), r4 %>% mutate(block="R4")),
          "output/robustness_checks.csv")
message("\nWrote output/robustness_checks.csv")
