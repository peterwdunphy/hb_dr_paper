# 11_unique_academic.R ----------------------------------------------------
# What, if anything, is UNIQUE about academic ophthalmology centers?
#
# The identification problem: academic programs sit where ophthalmologists
# already are. Raw "distance to nearest academic program" therefore conflates
# two things, proximity to eye care in general and proximity to academic
# tertiary capacity specifically. Reviewer 1 said this outright ("academic
# training centers cluster in populated cities which typically also have higher
# density of health care providers"), and Reviewer 2 asked for a direct link.
#
# Four tests, from weakest to strongest claim:
#
#  T1 CONDITIONAL EFFECT. Put academic drive time and general-ophthalmologist
#     drive time in the same model. The academic coefficient is then the effect
#     of academic proximity holding general eye-care proximity fixed. If it
#     collapses toward zero, academic centers are simply a marker of where eye
#     care is, and there is nothing unique to report.
#
#  T2 DISCORDANT COUNTIES. The 2x2 Reviewer 2 described: counties with vs
#     without a local ophthalmologist, crossed with near vs far from an
#     academic center. The cell that answers the reviewer is "has local
#     ophthalmologists but far from academic".
#
#  T3 EFFECT MODIFICATION. Does academic proximity matter more where general
#     supply is absent? If academic centers provide something distinctive
#     (subspecialty retina capacity: PRP, anti-VEGF, vitrectomy), their reach
#     should matter most exactly where there is no local alternative.
#
#  T4 CAPACITY, NOT JUST PROXIMITY. Programs differ in size. Using ophthalmology
#     resident counts as a capacity weight, build a gravity-style access score
#     (a simplified E2SFCA) and test whether capacity-weighted access does
#     better than nearest-program distance.
#
# All models are quasibinomial on (VTDR cases, DR cases - VTDR cases), which is
# the severity-mix outcome; overdispersion in these county aggregates is large
# (~16), so binomial standard errors would be badly understated.
# -------------------------------------------------------------------------

library(dplyr)
library(readr)

d <- read_csv("data_derived/county_analytic_2021_complete.csv",
              show_col_types = FALSE,
              col_types = cols(fips = "c", .default = col_guess()))

sup <- read_csv("data_derived/county_nearest_ophthalmologist.csv",
                show_col_types = FALSE,
                col_types = cols(fips = "c", supply_fips = "c",
                                 .default = col_guess()))

d <- d %>%
  inner_join(sup %>% select(fips, supply_drive_min), by = "fips") %>%
  filter(!is.na(supply_drive_min)) %>%
  mutate(
    log_acad   = log1p(drive_min),
    log_supply = log1p(supply_drive_min),
    z_acad     = as.numeric(scale(log_acad)),
    z_supply   = as.numeric(scale(log_supply)),
    z_ophth    = as.numeric(scale(ophth_per_100k)),
    z_optom    = as.numeric(scale(optom_per_100k)),
    z_popdens  = as.numeric(scale(log_pop_dens)),
    z_rucc     = as.numeric(scale(rucc)),
    z_svi      = as.numeric(scale(svi_overall)),
    drnon      = pmax(dr_cases - vtdr_cases, 0),
    no_local_ophth = ophth_patient_care == 0
  )

message("Analytic N for uniqueness tests: ", nrow(d))

CTRL <- "z_ophth + z_optom + z_popdens + z_rucc + z_svi"
qb <- function(f) glm(as.formula(f), data = d, family = quasibinomial())
grab <- function(m, terms) {
  s <- summary(m)$coefficients
  tibble::tibble(term = terms, beta = s[terms, 1], se = s[terms, 2],
                 p = s[terms, 4])
}

Y <- "cbind(vtdr_cases, drnon)"

# ---- T1: conditional effect ------------------------------------------------
message("\n===== T1: is the academic effect unique to academic centers? =====")
mA <- qb(paste(Y, "~ z_acad +", CTRL))
mB <- qb(paste(Y, "~ z_acad + z_supply +", CTRL))
mS <- qb(paste(Y, "~ z_supply +", CTRL))

t1 <- bind_rows(
  grab(mA, "z_acad")   %>% mutate(model = "A: academic only"),
  grab(mS, "z_supply") %>% mutate(model = "S: general supply only"),
  grab(mB, c("z_acad", "z_supply")) %>% mutate(model = "B: both")
) %>% select(model, term, beta, se, p)
print(as.data.frame(t1 %>% mutate(across(c(beta, se), ~round(.x, 4)), p = signif(p, 3))))

message("\ncor(academic drive time, general-supply drive time) = ",
        round(cor(d$log_acad, d$log_supply), 3))

# ---- T2: discordant counties ----------------------------------------------
message("\n===== T2: the 2x2 Reviewer 2 asked for =====")
med_acad <- median(d$drive_min)
tab <- d %>%
  mutate(local = if_else(no_local_ophth, "no local ophthalmologist",
                         "has local ophthalmologist"),
         acad  = if_else(drive_min > med_acad, "far from academic",
                         "near academic")) %>%
  group_by(local, acad) %>%
  summarise(n = n(),
            vtdr_share = round(100 * sum(vtdr_cases) / sum(dr_cases), 2),
            dr_level   = round(100 * sum(dr_cases) / sum(dm_pop), 2),
            median_acad_min = round(median(drive_min), 1),
            median_svi = round(median(svi_overall), 3),
            .groups = "drop")
print(as.data.frame(tab))

# ---- T3: effect modification ----------------------------------------------
message("\n===== T3: does academic proximity matter more where supply is absent? =====")
mI <- qb(paste(Y, "~ z_acad * no_local_ophth + z_supply +", CTRL))
print(round(summary(mI)$coefficients, 4))

# ---- T4: capacity-weighted access -----------------------------------------
message("\n===== T4: capacity-weighted access vs simple proximity =====")
if (file.exists("data_derived/county_program_drivetimes.csv")) {
  pairs <- read_csv("data_derived/county_program_drivetimes.csv",
                    show_col_types = FALSE,
                    col_types = cols(fips = "c", .default = col_guess()))
  progs <- read_csv("data_derived/programs_geocoded.csv", show_col_types = FALSE)
  ahrf  <- read_csv("data_derived/ahrf_2021_county.csv", show_col_types = FALSE,
                    col_types = cols(fips = "c", .default = col_guess()))

  # program capacity = ophthalmology residents in the program's county,
  # split across programs in that county
  cap <- progs %>%
    left_join(ahrf %>% select(fips, ophth_residents),
              by = c("county_fips" = "fips")) %>%
    group_by(county_fips) %>%
    mutate(capacity = coalesce(ophth_residents, 0) / n()) %>%
    ungroup() %>%
    select(prog_id, capacity)

  # gravity access: sum of capacity discounted by a 120-minute Gaussian decay
  BETA <- 120
  acc <- pairs %>%
    filter(!is.na(drive_min)) %>%
    left_join(cap, by = "prog_id") %>%
    mutate(w = exp(-(drive_min^2) / (2 * BETA^2))) %>%
    group_by(fips) %>%
    summarise(access_score = sum(capacity * w, na.rm = TRUE), .groups = "drop")

  d2 <- d %>% left_join(acc, by = "fips") %>%
    mutate(access_score = coalesce(access_score, 0),
           z_access = as.numeric(scale(log1p(access_score))))

  mC <- glm(as.formula(paste(Y, "~ z_access + z_supply +", CTRL)),
            data = d2, family = quasibinomial())
  print(round(summary(mC)$coefficients, 4))
  message("cor(capacity access, -log academic drive time) = ",
          round(cor(log1p(d2$access_score), -d2$log_acad, use = "complete.obs"), 3))
} else {
  message("county_program_drivetimes.csv not found; run 07_drivetime.R first.")
}

# ---- T5: university-based vs community-hospital programs -------------------
# If what is special about academic centers is subspecialty depth (retina
# capacity for PRP, anti-VEGF, vitrectomy) rather than the mere presence of
# residents, then proximity to a UNIVERSITY-based program should matter more
# than proximity to a community-hospital program. This is a within-exposure
# test of the mechanism using only the roster we already have, and it is the
# most direct evidence available for Reviewer 1's comment 8 ("state clearly why
# access to academic ophthalmology residency programs is important").
message("\n===== T5: university-based vs community-hospital programs =====")
if (file.exists("data_derived/county_program_drivetimes.csv")) {
  pairs <- read_csv("data_derived/county_program_drivetimes.csv",
                    show_col_types = FALSE,
                    col_types = cols(fips = "c", .default = col_guess()))
  progs <- read_csv("data_derived/programs_geocoded.csv", show_col_types = FALSE) %>%
    mutate(yr = suppressWarnings(as.numeric(accred_year))) %>%
    filter(is.na(yr) | yr <= 2021) %>%
    mutate(univ = grepl("School of Medicine|University|College of Medicine",
                        name, ignore.case = TRUE))
  message("University-based programs: ", sum(progs$univ),
          " | community/hospital: ", sum(!progs$univ))

  nearest_by_type <- function(is_univ) {
    ids <- progs$prog_id[progs$univ == is_univ]
    pairs %>% filter(prog_id %in% ids, !is.na(drive_min)) %>%
      group_by(fips) %>% summarise(m = min(drive_min), .groups = "drop")
  }
  du <- nearest_by_type(TRUE)  %>% rename(univ_min = m)
  dc <- nearest_by_type(FALSE) %>% rename(comm_min = m)

  d3 <- d %>% left_join(du, by = "fips") %>% left_join(dc, by = "fips") %>%
    mutate(univ_min = if_else(has_program & fips %in%
                                progs$county_fips[progs$univ], 0, univ_min),
           comm_min = if_else(has_program & fips %in%
                                progs$county_fips[!progs$univ], 0, comm_min)) %>%
    filter(!is.na(univ_min), !is.na(comm_min)) %>%
    mutate(z_univ = as.numeric(scale(log1p(univ_min))),
           z_comm = as.numeric(scale(log1p(comm_min))))

  message("N with both program types routable: ", nrow(d3))
  m5 <- glm(as.formula(paste(Y, "~ z_univ + z_comm + z_supply +", CTRL)),
            data = d3, family = quasibinomial())
  print(round(summary(m5)$coefficients, 4))
  message("cor(univ distance, community distance) = ",
          round(cor(d3$z_univ, d3$z_comm), 3))
} else {
  message("drive-time pairs not found; run 07_drivetime.R first.")
}

write_csv(t1, "output/unique_academic_T1.csv")
write_csv(tab, "output/unique_academic_T2.csv")
message("\nWrote output/unique_academic_T1.csv and T2.csv")
