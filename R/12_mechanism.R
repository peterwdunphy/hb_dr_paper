# 12_mechanism.R ----------------------------------------------------------
# What drives the social-vulnerability gradient in DR PROGRESSION?
#
# Established so far: social vulnerability is associated with the vision-
# threatening SHARE of diagnosed DR about six times more strongly than with
# whether DR occurs at all among people with diabetes. That is a progression /
# late-presentation finding, not a burden finding, and it is the paper's
# contribution. This script asks what mechanism it runs through.
#
# M1. THE MEDICARE TEST (age-stratified gradient).
#     If the vulnerability gradient in progression operates through insurance
#     and care access, it should be LARGER among adults aged 40-64, where
#     coverage varies steeply with socioeconomic status, and SMALLER among
#     adults 65-84, where Medicare is near-universal. If the gradient is
#     unchanged at 65+, insurance coverage is not the operative channel.
#     This also turns Reviewer 1's comment 7 from an objection into a design:
#     they argued an older cut "enriches the population more likely to have
#     health insurance", which is precisely what makes it a useful contrast.
#
# M2. WHICH DIMENSION OF VULNERABILITY?
#     SVI decomposes into four themes. Theme 4 (housing type & transportation)
#     contains no-vehicle-available and is the closest thing in the index to a
#     transport-access measure. If progression tracks theme 4 more than theme 1
#     (socioeconomic status), that is an access finding the geographic distance
#     measure failed to capture, and it would be actionable.
#
# M3. DOES GEOGRAPHY MATTER WHERE VULNERABILITY IS HIGH?
#     Access may be irrelevant on average yet bind in vulnerable counties.
#     Tests SVI x drive-time and SVI x supply-distance interactions.
#
# M4. DETECTION-ADJUSTED SPECIFICATION.
#     The severity share rises with specialist supply, consistent with the
#     detection signal the VEHSS authors warned about. This refits the SVI
#     gradient with explicit detection proxies to see whether it survives.
# -------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(readr)

d <- read_csv("data_derived/county_analytic_2021_complete.csv",
              show_col_types = FALSE,
              col_types = cols(fips = "c", .default = col_guess()))
sup <- read_csv("data_derived/county_nearest_ophthalmologist.csv",
                show_col_types = FALSE,
                col_types = cols(fips = "c", supply_fips = "c", .default = col_guess()))

# ---- age-stratified outcomes ----------------------------------------------
ct <- cols(.default = "c", numerator = "d", sample_size = "d")
dr_age <- read_csv("data/vehss/vehss_dr_county_raw.csv", show_col_types = FALSE,
                   col_types = ct) %>%
  filter(data_value_type == "Crude Prevalence",
         age %in% c("40-64 years", "65-84 years")) %>%
  mutate(stage = if_else(response == "Vision threatening stage", "vtdr", "dr")) %>%
  select(fips = locationid, age, stage, cases = numerator, dm_pop = sample_size) %>%
  pivot_wider(names_from = stage, values_from = cases, names_glue = "{stage}_cases") %>%
  filter(!is.na(dr_cases), !is.na(vtdr_cases), dr_cases > 0)

d2 <- d %>%
  select(fips, svi_overall, svi_ses, svi_hhchar, svi_minority, svi_housing,
         ophth_per_100k, optom_per_100k, ophth_patient_care, log_pop_dens,
         rucc, drive_min, log_drive, total_pop) %>%
  left_join(sup %>% select(fips, supply_drive_min), by = "fips") %>%
  filter(!is.na(supply_drive_min)) %>%
  mutate(z_svi = as.numeric(scale(svi_overall)),
         z_ophth = as.numeric(scale(ophth_per_100k)),
         z_optom = as.numeric(scale(optom_per_100k)),
         z_popdens = as.numeric(scale(log_pop_dens)),
         z_rucc = as.numeric(scale(rucc)),
         z_acad = as.numeric(scale(log_drive)),
         z_supply = as.numeric(scale(log1p(supply_drive_min))),
         no_local_ophth = ophth_patient_care == 0)

CTRL <- "z_ophth + z_optom + z_popdens + z_rucc + z_acad + z_supply"

# =========================================================================
message("===== M1: THE MEDICARE TEST =====")
message("SVI gradient in DR progression, ages 40-64 (coverage varies with SES)")
message("versus ages 65-84 (near-universal Medicare).\n")

m1_res <- list()
for (a in c("40-64 years", "65-84 years")) {
  da <- dr_age %>% filter(age == a) %>%
    inner_join(d2, by = "fips") %>%
    mutate(drnon = pmax(dr_cases - vtdr_cases, 0))
  fit <- glm(as.formula(paste("cbind(vtdr_cases, drnon) ~ z_svi +", CTRL)),
             data = da, family = quasibinomial())
  s <- summary(fit)$coefficients
  m1_res[[a]] <- tibble(
    age = a, n = nrow(da),
    mean_share = 100 * sum(da$vtdr_cases) / sum(da$dr_cases),
    beta_svi = s["z_svi", 1], se = s["z_svi", 2], p = s["z_svi", 4],
    or_svi = exp(s["z_svi", 1])
  )
}
m1 <- bind_rows(m1_res)
print(as.data.frame(m1 %>% mutate(across(where(is.numeric), ~ signif(.x, 4)))))

# formal test of whether the two SVI coefficients differ
z <- (m1$beta_svi[1] - m1$beta_svi[2]) / sqrt(m1$se[1]^2 + m1$se[2]^2)
message(sprintf("\nDifference in SVI gradient (40-64 minus 65-84): %.4f",
                m1$beta_svi[1] - m1$beta_svi[2]))
message(sprintf("z = %.2f, p = %.3g", z, 2 * pnorm(-abs(z))))
message(sprintf("Ratio of odds ratios: %.2f", m1$or_svi[1] / m1$or_svi[2]))

# =========================================================================
message("\n\n===== M2: WHICH DIMENSION OF VULNERABILITY? =====")
dall <- d %>% inner_join(d2 %>% select(fips, z_ophth, z_optom, z_popdens,
                                       z_rucc, z_acad, z_supply, no_local_ophth),
                         by = "fips") %>%
  mutate(drnon = pmax(dr_cases - vtdr_cases, 0))

themes <- c(svi_ses = "socioeconomic status",
            svi_hhchar = "household characteristics",
            svi_minority = "racial/ethnic minority status",
            svi_housing = "housing type & transportation")
m2 <- lapply(names(themes), function(v) {
  dall$zt <- as.numeric(scale(dall[[v]]))
  f <- glm(as.formula(paste("cbind(vtdr_cases, drnon) ~ zt +", CTRL)),
           data = dall, family = quasibinomial())
  s <- summary(f)$coefficients
  tibble(theme = themes[[v]], var = v, beta = s["zt", 1], se = s["zt", 2],
         p = s["zt", 4], or = exp(s["zt", 1]))
}) %>% bind_rows()
print(as.data.frame(m2 %>% mutate(across(where(is.numeric), ~ signif(.x, 4)))))

message("\nAll four themes entered jointly:")
fj <- glm(as.formula(paste("cbind(vtdr_cases, drnon) ~ scale(svi_ses) +",
                           "scale(svi_hhchar) + scale(svi_minority) +",
                           "scale(svi_housing) +", CTRL)),
          data = dall, family = quasibinomial())
print(round(summary(fj)$coefficients[2:5, ], 4))

# =========================================================================
message("\n\n===== M3: DOES GEOGRAPHY BIND WHERE VULNERABILITY IS HIGH? =====")
mi1 <- glm(as.formula(paste("cbind(vtdr_cases, drnon) ~ z_svi * z_acad +", CTRL)),
           data = dall %>% mutate(z_svi = as.numeric(scale(svi_overall))),
           family = quasibinomial())
print(round(summary(mi1)$coefficients[grep("z_svi|z_acad", rownames(summary(mi1)$coefficients)), ], 4))

mi2 <- glm(as.formula(paste("cbind(vtdr_cases, drnon) ~ z_svi * z_supply +", CTRL)),
           data = dall %>% mutate(z_svi = as.numeric(scale(svi_overall))),
           family = quasibinomial())
message("\nSVI x distance-to-any-ophthalmologist:")
print(round(summary(mi2)$coefficients[grep("z_svi|z_supply", rownames(summary(mi2)$coefficients)), ], 4))

# =========================================================================
message("\n\n===== M4: DETECTION-ADJUSTED SVI GRADIENT =====")
dall <- dall %>% mutate(z_svi = as.numeric(scale(svi_overall)))
base <- glm(cbind(vtdr_cases, drnon) ~ z_svi, data = dall, family = quasibinomial())
det  <- glm(cbind(vtdr_cases, drnon) ~ z_svi + z_ophth + z_optom +
              no_local_ophth + z_supply + z_acad + z_popdens + z_rucc,
            data = dall, family = quasibinomial())
cmp <- tibble(
  spec = c("SVI alone", "SVI + detection proxies"),
  beta = c(coef(base)["z_svi"], coef(det)["z_svi"]),
  se   = c(summary(base)$coefficients["z_svi", 2],
           summary(det)$coefficients["z_svi", 2])
) %>% mutate(or = exp(beta))
print(as.data.frame(cmp %>% mutate(across(where(is.numeric), ~ signif(.x, 4)))))

write_csv(m1, "output/mech_M1_medicare.csv")
write_csv(m2, "output/mech_M2_themes.csv")
message("\nWrote output/mech_M1_medicare.csv and mech_M2_themes.csv")
