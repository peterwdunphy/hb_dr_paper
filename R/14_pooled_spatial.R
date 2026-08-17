# 14_pooled_spatial.R -----------------------------------------------------
# PRIMARY SPECIFICATION and spatial confirmation.
#
# 13_composition_check.R showed that most of the apparent county-level social
# gradient in DR progression is a post-stratification artifact: VEHSS applies
# national race- and age-specific rates to each county's demographic mix, so an
# "All races" county estimate moves with composition by construction.
#
# The fix adopted here is to estimate the gradient WITHIN demographic strata.
# We stack county x race x age cells and include stratum fixed effects, so the
# social-vulnerability coefficient is identified only from variation between
# counties within the same race-by-age cell. Composition cannot contribute.
#
# Then, because Moran's I on the earlier residuals was large, we refit the
# largest single stratum with a Leroux CAR spatial random effect (CARBayes),
# which is the team's preferred spatial approach and takes binomial counts and
# trials directly.
# -------------------------------------------------------------------------

library(jsonlite); library(dplyr); library(readr); library(tidyr)

fetch <- function(where) {
  pages <- list()
  for (i in 1:6) {
    u <- paste0("https://data.cdc.gov/resource/qeru-k2y2.json?$where=",
                utils::URLencode(where, reserved = TRUE),
                "&$limit=50000&$offset=", format((i - 1) * 50000, scientific = FALSE),
                "&$order=locationid,response")
    p <- fromJSON(u, flatten = TRUE)
    if (length(p) == 0 || nrow(p) == 0) break
    pages[[i]] <- p; if (nrow(p) < 50000) break
  }
  bind_rows(pages)
}

base <- paste0("geographiclevel='County' AND yearstart='2021' ",
               "AND question='Prevalence of Diabetic Retinopathy' ",
               "AND sex='Both sexes' AND riskfactorresponse='Yes' ",
               "AND data_value_type='Crude Prevalence' ",
               "AND response IN ('All DR stages','Vision threatening stage')")

RACES <- c("White, non-Hispanic", "Black, non-Hispanic", "Hispanic, any race")
AGES  <- c("40-64 years", "65-84 years")

CACHE <- "data_derived/vehss_strata_long.csv"
if (file.exists(CACHE)) {
  long <- read_csv(CACHE, show_col_types = FALSE, col_types = cols(fips = "c", .default = col_guess()))
} else {
  long <- lapply(RACES, function(r) lapply(AGES, function(a) {
    x <- fetch(paste0(base, " AND raceethnicity='", r, "' AND age='", a, "'"))
    if (nrow(x) == 0) return(NULL)
    x %>% mutate(stage = if_else(response == "Vision threatening stage", "vtdr", "dr"),
                 num = as.numeric(numerator), den = as.numeric(sample_size)) %>%
      select(fips = locationid, stage, num, den) %>%
      pivot_wider(names_from = stage, values_from = c(num, den)) %>%
      filter(!is.na(num_dr), !is.na(num_vtdr), num_dr > 0) %>%
      transmute(fips, race = r, age = a, dr_cases = num_dr,
                vtdr_cases = num_vtdr, dm_pop = den_dr)
  })) %>% bind_rows()
  write_csv(long, CACHE)
}
message("Stratum-level cells: ", nrow(long), " over ", n_distinct(long$fips), " counties")

d   <- read_csv("data_derived/county_analytic_2021_complete.csv", show_col_types = FALSE,
                col_types = cols(fips = "c", .default = col_guess()))
sup <- read_csv("data_derived/county_nearest_ophthalmologist.csv", show_col_types = FALSE,
                col_types = cols(fips = "c", supply_fips = "c", .default = col_guess()))

cov <- d %>% select(fips, svi_overall, svi_ses, ophth_per_100k, optom_per_100k,
                    ophth_patient_care, log_pop_dens, rucc, log_drive, total_pop) %>%
  left_join(sup %>% select(fips, supply_drive_min), by = "fips")

dat <- long %>% inner_join(cov, by = "fips") %>%
  filter(!is.na(svi_overall), !is.na(supply_drive_min), is.finite(log_pop_dens)) %>%
  mutate(stratum = paste(race, age),
         drnon   = pmax(dr_cases - vtdr_cases, 0),
         z_svi   = as.numeric(scale(svi_overall)),
         z_ophth = as.numeric(scale(ophth_per_100k)),
         z_optom = as.numeric(scale(optom_per_100k)),
         z_popdens = as.numeric(scale(log_pop_dens)),
         z_rucc  = as.numeric(scale(rucc)),
         z_acad  = as.numeric(scale(log_drive)),
         z_supply = as.numeric(scale(log1p(supply_drive_min))))

message("Analytic cells: ", nrow(dat), " | strata: ", n_distinct(dat$stratum))

# ---- primary: pooled with stratum fixed effects ----------------------------
message("\n===== PRIMARY MODEL: within-stratum social gradient in DR progression =====")
m <- glm(cbind(vtdr_cases, drnon) ~ z_svi + z_acad + z_supply + z_ophth +
           z_optom + z_popdens + z_rucc + factor(stratum),
         data = dat, family = quasibinomial())
cf <- summary(m)$coefficients
print(round(cf[!grepl("factor\\(stratum\\)", rownames(cf)), ], 4))
message("dispersion: ", round(summary(m)$dispersion, 2))

# per-SD effect on the percentage-point scale, at the White NH 65-84 baseline
b <- coef(m)["z_svi"]; p0 <- 0.1417
o1 <- (p0 / (1 - p0)) * exp(b)
message(sprintf("\nSVI: OR = %.4f per SD; %.2f%% -> %.2f%% (%.2f pp per SD)",
                exp(b), 100 * p0, 100 * o1 / (1 + o1), 100 * (o1 / (1 + o1) - p0)))

# contrast with the composition-contaminated estimate
message(sprintf("Composition-contaminated 'All races' estimate was %+.4f; ", 0.1460),
        sprintf("within-stratum estimate is %+.4f (%.0f%% of it was artifact)",
                b, 100 * (1 - b / 0.1460)))

# ---- heterogeneity: is the gradient the same in every stratum? -------------
message("\n===== Gradient by stratum (consistency check) =====")
het <- dat %>% group_by(stratum) %>% group_modify(~{
  f <- glm(cbind(vtdr_cases, drnon) ~ z_svi + z_acad + z_supply + z_ophth +
             z_optom + z_popdens + z_rucc, data = .x, family = quasibinomial())
  s <- summary(f)$coefficients
  tibble(n = nrow(.x), beta_svi = s["z_svi", 1], se = s["z_svi", 2],
         p = s["z_svi", 4], beta_acad = s["z_acad", 1], p_acad = s["z_acad", 4])
}) %>% ungroup()
print(as.data.frame(het %>% mutate(across(where(is.numeric), ~ signif(.x, 4)))))

write_csv(het, "output/gradient_by_stratum.csv")

# ---- spatial model: Leroux CAR on the largest stratum ----------------------
message("\n===== SPATIAL MODEL (CARBayes, Leroux CAR, binomial) =====")
suppressMessages({library(spdep); library(sf); library(CARBayes)})

one <- dat %>% filter(stratum == "White, non-Hispanic 65-84 years")
shp <- st_read("data_derived/counties_2021.gpkg", quiet = TRUE) %>%
  filter(GEOID %in% one$fips)
one <- one[match(shp$GEOID, one$fips), ]
stopifnot(identical(one$fips, shp$GEOID))

nb <- poly2nb(shp, queen = TRUE)
iso <- which(card(nb) == 0)
message("Counties with no queen neighbour (islands): ", length(iso))
if (length(iso) > 0) {
  # attach each island to its nearest neighbour by centroid so W is connected
  ctr <- st_coordinates(st_point_on_surface(st_geometry(shp)))
  for (i in iso) {
    dd <- sqrt((ctr[, 1] - ctr[i, 1])^2 + (ctr[, 2] - ctr[i, 2])^2); dd[i] <- Inf
    j <- which.min(dd)
    nb[[i]] <- as.integer(j); nb[[j]] <- sort(unique(c(nb[[j]], as.integer(i))))
  }
  message("  islands linked to nearest county so the adjacency graph is connected")
}
W <- nb2mat(nb, style = "B", zero.policy = TRUE)

lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
gl <- glm(cbind(vtdr_cases, drnon) ~ z_svi + z_acad + z_supply + z_ophth +
            z_optom + z_popdens + z_rucc, data = one, family = quasibinomial())
mi <- moran.test(residuals(gl, type = "pearson"), lw, zero.policy = TRUE)
message(sprintf("Moran's I on non-spatial residuals: I = %.4f, p = %.3g",
                mi$estimate[1], mi$p.value))

set.seed(42)
# CARBayes binomial takes successes on the LHS and an explicit `trials` vector,
# not a cbind(successes, failures) response.
fit <- S.CARleroux(
  formula = vtdr_cases ~ z_svi + z_acad + z_supply + z_ophth +
    z_optom + z_popdens + z_rucc,
  family = "binomial", trials = one$dr_cases, W = W, data = one,
  burnin = 20000, n.sample = 140000, thin = 20, verbose = FALSE)

print(fit$summary.results)
saveRDS(fit, "output/carbayes_whiteNH6584.rds")
message("\nWrote output/gradient_by_stratum.csv and output/carbayes_whiteNH6584.rds")
