# 16_external_tests.R -----------------------------------------------------
# Tests using data sources independent of VEHSS.
#
# N1  DETECTION STRESS TEST. The main threat is that mild DR goes undiagnosed
#     more often in disadvantaged counties, inflating the vision-threatening
#     share without any true progression difference. Medicare imaging,
#     evaluation-and-management, and diagnostic-test events per 1,000
#     beneficiaries measure how much diagnostic activity actually occurs in a
#     county, from claims rather than from provider counts. If the social
#     gradient survives their inclusion, detection is a weaker explanation.
#
# N2  SCREENING-PROPENSITY MECHANISM. Mammography screening completion is a
#     Medicare-claims measure of whether a county's population follows through
#     on a recommended preventive screen. It has nothing to do with eyes. If it
#     attenuates the gradient substantially, the mechanism is general
#     preventive-care engagement rather than anything ophthalmology-specific.
#
# N3  DIRECT COVERAGE TEST. The Medicare age contrast was an indirect test of
#     insurance. Here we use the county uninsured rate directly, and the
#     dual-eligible share, which identifies poverty WITHIN the fully insured
#     65+ population.
#
# N4  MEDICAID EXPANSION NATURAL EXPERIMENT. Expansion changed coverage for
#     low-income adults under 65 and left the 65+ population untouched. If
#     coverage drives the gradient, the social gradient among adults 40-64
#     should be smaller in expansion states, with the 65-84 band serving as a
#     built-in placebo. This is a triple contrast: SVI x expansion x age band.
#
# N5  CONVERGENT VALIDITY. Preventable hospital stays (ambulatory-care-sensitive
#     admissions) measure ambulatory care failure in a completely different
#     data system and organ system. If social vulnerability predicts that too,
#     the DR result is consistent with a general care-quality gradient rather
#     than an eye-specific measurement artifact.
# -------------------------------------------------------------------------

library(dplyr); library(readr); library(tidyr)

long <- read_csv("data_derived/vehss_strata_long.csv", show_col_types = FALSE,
                 col_types = cols(fips = "c", .default = col_guess()))
d    <- read_csv("data_derived/county_analytic_2021_complete.csv", show_col_types = FALSE,
                 col_types = cols(fips = "c", .default = col_guess()))
sup  <- read_csv("data_derived/county_nearest_ophthalmologist.csv", show_col_types = FALSE,
                 col_types = cols(fips = "c", supply_fips = "c", .default = col_guess()))
ext  <- read_csv("data_derived/external_county_2021.csv", show_col_types = FALSE,
                 col_types = cols(fips = "c", .default = col_guess()))

z <- function(x) as.numeric(scale(x))

base <- d %>%
  select(fips, svi_overall, ophth_per_100k, optom_per_100k, log_pop_dens,
         rucc, log_drive, total_pop, dr_cases, vtdr_cases, dm_pop) %>%
  left_join(sup %>% select(fips, supply_drive_min), by = "fips") %>%
  left_join(ext %>% select(-state), by = "fips")

dat <- long %>%
  inner_join(base %>% select(-dr_cases, -vtdr_cases, -dm_pop), by = "fips") %>%
  filter(!is.na(svi_overall), !is.na(supply_drive_min), is.finite(log_pop_dens)) %>%
  mutate(stratum = paste(race, age),
         age65   = as.integer(age == "65-84 years"),
         drnon   = pmax(dr_cases - vtdr_cases, 0),
         z_svi = z(svi_overall), z_ophth = z(ophth_per_100k),
         z_optom = z(optom_per_100k), z_popdens = z(log_pop_dens),
         z_rucc = z(rucc), z_acad = z(log_drive),
         z_supply = z(log1p(supply_drive_min)))

CTRL <- "z_acad + z_supply + z_ophth + z_optom + z_popdens + z_rucc + factor(stratum)"
Y <- "cbind(vtdr_cases, drnon)"

qb <- function(rhs, data = dat) glm(as.formula(paste(Y, "~", rhs)), data = data,
                                    family = quasibinomial())
svi_row <- function(m, lab) {
  s <- summary(m)$coefficients
  tibble(spec = lab, beta = s["z_svi", 1], se = s["z_svi", 2], p = s["z_svi", 4],
         n = length(m$residuals))
}

message("Analytic cells: ", nrow(dat))

# =========================================================================
message("\n===== N1: DETECTION STRESS TEST =====")
d1 <- dat %>% filter(!is.na(imaging_p1k), !is.na(em_p1k), !is.na(tests_p1k)) %>%
  mutate(z_img = z(imaging_p1k), z_em = z(em_p1k), z_tst = z(tests_p1k))
r1 <- bind_rows(
  svi_row(qb(paste("z_svi +", CTRL), d1), "base (same sample)"),
  svi_row(qb(paste("z_svi + z_img +", CTRL), d1), "+ Medicare imaging/1k"),
  svi_row(qb(paste("z_svi + z_em +", CTRL), d1), "+ Medicare E&M/1k"),
  svi_row(qb(paste("z_svi + z_img + z_em + z_tst +", CTRL), d1), "+ all three")
)
print(as.data.frame(r1 %>% mutate(across(c(beta,se), ~round(.x,4)), p = signif(p,3))))
m1f <- qb(paste("z_svi + z_img + z_em + z_tst +", CTRL), d1)
message("\n  detection-proxy coefficients in the full model:")
print(round(summary(m1f)$coefficients[c("z_img","z_em","z_tst"), ], 4))

# =========================================================================
message("\n===== N2: SCREENING-PROPENSITY MECHANISM =====")
d2 <- dat %>% filter(!is.na(mammography), !is.na(flu_vax)) %>%
  mutate(z_mam = z(mammography), z_flu = z(flu_vax))
r2 <- bind_rows(
  svi_row(qb(paste("z_svi +", CTRL), d2), "base (same sample)"),
  svi_row(qb(paste("z_svi + z_mam +", CTRL), d2), "+ mammography screening"),
  svi_row(qb(paste("z_svi + z_mam + z_flu +", CTRL), d2), "+ mammography + flu vax")
)
print(as.data.frame(r2 %>% mutate(across(c(beta,se), ~round(.x,4)), p = signif(p,3))))
m2f <- qb(paste("z_svi + z_mam + z_flu +", CTRL), d2)
message("\n  screening coefficients:")
print(round(summary(m2f)$coefficients[c("z_mam","z_flu"), ], 4))
message(sprintf("  correlation SVI with mammography: %.3f",
                cor(d2$svi_overall, d2$mammography, use = "complete.obs")))

# =========================================================================
message("\n===== N3: DIRECT COVERAGE TEST =====")
d3 <- dat %>% filter(!is.na(uninsured), !is.na(dual_pct)) %>%
  mutate(z_unins = z(uninsured), z_dual = z(dual_pct))
r3 <- bind_rows(
  svi_row(qb(paste("z_svi +", CTRL), d3), "base (same sample)"),
  svi_row(qb(paste("z_svi + z_unins +", CTRL), d3), "+ uninsured rate"),
  svi_row(qb(paste("z_svi + z_dual +", CTRL), d3), "+ dual-eligible share"),
  svi_row(qb(paste("z_svi + z_unins + z_dual +", CTRL), d3), "+ both")
)
print(as.data.frame(r3 %>% mutate(across(c(beta,se), ~round(.x,4)), p = signif(p,3))))
m3f <- qb(paste("z_svi + z_unins + z_dual +", CTRL), d3)
print(round(summary(m3f)$coefficients[c("z_unins","z_dual"), ], 4))

message("\n  dual-eligible share as the exposure, 65-84 only (poverty among the fully insured):")
d3b <- d3 %>% filter(age == "65-84 years")
m3b <- glm(as.formula(paste(Y, "~ z_dual + z_acad + z_supply + z_ophth + z_optom +",
                            "z_popdens + z_rucc + factor(stratum)")),
           data = d3b, family = quasibinomial())
print(round(summary(m3b)$coefficients["z_dual", , drop = FALSE], 4))

# =========================================================================
message("\n===== N4: MEDICAID EXPANSION NATURAL EXPERIMENT =====")
d4 <- dat %>% filter(medicaid_expansion %in% c("yes", "no")) %>%
  mutate(expanded = as.integer(medicaid_expansion == "yes"))
message("cells: ", nrow(d4), " | expansion counties: ",
        n_distinct(d4$fips[d4$expanded == 1]), " | non-expansion: ",
        n_distinct(d4$fips[d4$expanded == 0]))

message("\nSVI gradient by age band and expansion status:")
g4 <- d4 %>% group_by(age, medicaid_expansion) %>% group_modify(~{
  f <- glm(as.formula(paste(Y, "~ z_svi + z_acad + z_supply + z_ophth + z_optom +",
                            "z_popdens + z_rucc + factor(stratum)")),
           data = .x, family = quasibinomial())
  s <- summary(f)$coefficients
  tibble(n_cells = nrow(.x), beta_svi = s["z_svi",1], se = s["z_svi",2], p = s["z_svi",4])
}) %>% ungroup()
print(as.data.frame(g4 %>% mutate(across(where(is.numeric), ~signif(.x,4)))))

message("\nFormal triple interaction (SVI x expanded x age65):")
m4 <- glm(as.formula(paste(Y, "~ z_svi * expanded * age65 + z_acad + z_supply +",
                           "z_ophth + z_optom + z_popdens + z_rucc + factor(race)")),
          data = d4, family = quasibinomial())
keep <- grep("z_svi", rownames(summary(m4)$coefficients), value = TRUE)
print(round(summary(m4)$coefficients[keep, ], 4))

# =========================================================================
message("\n===== N5: CONVERGENT VALIDITY (preventable hospital stays) =====")
# county-level, one row per county; compare standardized SVI associations
cv <- d %>% select(fips, svi_overall, dr_cases, vtdr_cases, ophth_per_100k,
                   optom_per_100k, log_pop_dens, rucc, log_drive) %>%
  left_join(ext %>% select(fips, prevent_hosp, mammography, chr_diabetes), by = "fips") %>%
  filter(!is.na(prevent_hosp), !is.na(svi_overall)) %>%
  mutate(z_svi = z(svi_overall), z_ophth = z(ophth_per_100k), z_optom = z(optom_per_100k),
         z_popdens = z(log_pop_dens), z_rucc = z(rucc), z_acad = z(log_drive),
         y_ph = z(log(prevent_hosp)), y_mam = z(mammography))

f_ph  <- lm(y_ph  ~ z_svi + z_ophth + z_optom + z_popdens + z_rucc + z_acad, data = cv)
f_mam <- lm(y_mam ~ z_svi + z_ophth + z_optom + z_popdens + z_rucc + z_acad, data = cv)
message(sprintf("SVI -> preventable hospital stays (SD units): %+.3f (p %.3g)",
                coef(f_ph)["z_svi"], summary(f_ph)$coefficients["z_svi",4]))
message(sprintf("SVI -> mammography screening    (SD units): %+.3f (p %.3g)",
                coef(f_mam)["z_svi"], summary(f_mam)$coefficients["z_svi",4]))
message(sprintf("Spearman rho(preventable hosp stays, VTDR share): %.3f",
                cor(cv$prevent_hosp, cv$vtdr_cases/cv$dr_cases, method = "spearman",
                    use = "complete.obs")))

write_csv(bind_rows(r1 %>% mutate(test="N1 detection"),
                    r2 %>% mutate(test="N2 screening"),
                    r3 %>% mutate(test="N3 coverage")),
          "output/external_tests_N1_N3.csv")
write_csv(g4, "output/external_test_N4_expansion.csv")
message("\nWrote output/external_tests_N1_N3.csv and external_test_N4_expansion.csv")
