# 10_models.R -------------------------------------------------------------
# Primary analysis: does spatial access to academic ophthalmology predict the
# SEVERITY MIX of diagnosed diabetic retinopathy?
#
# Outcome  : VTDR cases / all-stage DR cases among adults with diabetes
#            (binomial: successes = vtdr_cases, trials = dr_cases)
# Exposure : log(1 + drive minutes) to the nearest ACGME ophthalmology
#            residency program, from the population-weighted county centroid
#
# Nested specification (Models 1-3 per the team's agreed structure):
#   M1  exposure only
#   M2  + provider supply (ophthalmologist and optometrist density),
#         population density, rurality
#   M3  + SVI  <- the specification Dr. Dunphy asked for: controlling for
#                 social vulnerability, what does access tell us
#
# Contrast model (this is the point of the paper):
#   L1-L3  identical specifications with the DR LEVEL as the outcome
#          (dr_cases / dm_pop). The original submission found that greater
#          distance predicted LOWER DR prevalence and interpreted it as
#          evidence that access barriers increase burden, which reads the sign
#          backwards. Under a detection account, poor access should lower the
#          diagnosed LEVEL while raising the SEVERITY SHARE. Running both on
#          the same data makes that testable rather than rhetorical.
#
# Specificity check:
#   G3     Model 3 on the glaucoma severity share. NOTE: glaucoma share has
#          about one tenth the relative variation of the VTDR share
#          (CV 0.024 vs 0.231), so effects are compared in STANDARDIZED units;
#          a raw-scale null there would mostly reflect compressed variance.
#
# Binomial GLM on county aggregates will be overdispersed. We use quasibinomial
# so the standard errors are not fictitiously small.
# -------------------------------------------------------------------------

library(dplyr)
library(readr)

d <- read_csv("data_derived/county_analytic_2021_complete.csv",
              show_col_types = FALSE, col_types = cols(fips = "c",
                                                       .default = col_guess()))
message("Analytic N: ", nrow(d))

d <- d %>%
  mutate(
    z_log_drive = as.numeric(scale(log_drive)),
    z_ophth     = as.numeric(scale(ophth_per_100k)),
    z_optom     = as.numeric(scale(optom_per_100k)),
    z_popdens   = as.numeric(scale(log_pop_dens)),
    z_svi       = as.numeric(scale(svi_overall)),
    z_rucc      = as.numeric(scale(rucc)),
    dr_noncases   = pmax(dr_cases - vtdr_cases, 0),
    dm_noncases   = pmax(dm_pop  - dr_cases,   0),
    glauc_noncases = pmax(glauc_cases - vag_cases, 0)
  )

f1 <- ~ z_log_drive
f2 <- ~ z_log_drive + z_ophth + z_optom + z_popdens + z_rucc
f3 <- ~ z_log_drive + z_ophth + z_optom + z_popdens + z_rucc + z_svi

fit <- function(resp, rhs, data) {
  glm(update(rhs, paste(resp, "~ .")), data = data, family = quasibinomial())
}

tidy_row <- function(m, label) {
  s <- summary(m)$coefficients
  keep <- rownames(s) != "(Intercept)"
  tibble::tibble(
    model = label,
    term  = rownames(s)[keep],
    beta  = s[keep, 1], se = s[keep, 2], p = s[keep, 4],
    or    = exp(s[keep, 1]),
    disp  = summary(m)$dispersion
  )
}

# ---- primary: severity share ----------------------------------------------
m1 <- fit("cbind(vtdr_cases, dr_noncases)", f1, d)
m2 <- fit("cbind(vtdr_cases, dr_noncases)", f2, d)
m3 <- fit("cbind(vtdr_cases, dr_noncases)", f3, d)

# ---- contrast: diagnosed DR level -----------------------------------------
l1 <- fit("cbind(dr_cases, dm_noncases)", f1, d)
l2 <- fit("cbind(dr_cases, dm_noncases)", f2, d)
l3 <- fit("cbind(dr_cases, dm_noncases)", f3, d)

# ---- specificity: glaucoma severity share ---------------------------------
g3 <- fit("cbind(vag_cases, glauc_noncases)", f3, d)

res <- bind_rows(
  tidy_row(m1, "M1 VTDR share"), tidy_row(m2, "M2 VTDR share"),
  tidy_row(m3, "M3 VTDR share"),
  tidy_row(l1, "L1 DR level"),   tidy_row(l2, "L2 DR level"),
  tidy_row(l3, "L3 DR level"),
  tidy_row(g3, "G3 glaucoma share")
)

message("\n================ EXPOSURE COEFFICIENT ACROSS MODELS ================")
print(res %>% filter(term == "z_log_drive") %>%
        mutate(across(c(beta, se, or, disp), ~ round(.x, 4)),
               p = signif(p, 3)) %>% as.data.frame())

message("\n================ FULL MODEL 3 (VTDR share) ================")
print(summary(m3))

message("\n================ FULL MODEL L3 (DR level) ================")
print(summary(l3)$coefficients)

write_csv(res, "output/model_coefficients.csv")
message("\nWrote output/model_coefficients.csv")

# ---- spatial autocorrelation of residuals ----------------------------------
if (requireNamespace("spdep", quietly = TRUE) &&
    requireNamespace("sf", quietly = TRUE)) {
  suppressMessages({library(spdep); library(sf)})
  shp <- st_read("data_derived/counties_2021.gpkg", quiet = TRUE) %>%
    filter(GEOID %in% d$fips)
  shp <- shp[match(d$fips, shp$GEOID), ]
  nb  <- poly2nb(shp, queen = TRUE)
  n_islands <- sum(card(nb) == 0)
  message("\nIslands with no queen neighbour: ", n_islands)
  lw  <- nb2listw(nb, style = "W", zero.policy = TRUE)
  mi  <- moran.test(residuals(m3, type = "pearson"), lw, zero.policy = TRUE)
  message("Moran's I on M3 residuals: I = ", round(mi$estimate[1], 4),
          ", p = ", signif(mi$p.value, 3))
} else {
  message("\nspdep not installed; skipping Moran's I.")
  message("  install.packages(c('spdep','CARBayes','spatialreg'))")
}
