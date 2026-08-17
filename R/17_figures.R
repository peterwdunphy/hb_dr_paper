# 17_figures.R ------------------------------------------------------------
# Figures for both manuscripts. Written to paper/figs/ as PDFs.
#
# Design rule: every figure has to be readable by a clinical audience without
# reading the methods. Effects are shown on the percentage-point scale wherever
# possible rather than as log-odds.
# -------------------------------------------------------------------------

library(dplyr); library(readr); library(tidyr); library(ggplot2); library(sf)

dir.create("paper/figs", recursive = TRUE, showWarnings = FALSE)
z <- function(x) as.numeric(scale(x))

th <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title = element_text(size = 9),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 8.5, colour = "grey30"),
        plot.caption = element_text(size = 7.5, colour = "grey40", hjust = 0))
theme_set(th)

# long titles overflow the device; wrap them at a fixed width
wr  <- function(x, n = 78) paste(strwrap(x, n), collapse = "\n")
wrc <- function(x, n = 118) paste(strwrap(x, n), collapse = "\n")

d    <- read_csv("data_derived/county_analytic_2021_complete.csv", show_col_types = FALSE,
                 col_types = cols(fips = "c", .default = col_guess()))
long <- read_csv("data_derived/vehss_strata_long.csv", show_col_types = FALSE,
                 col_types = cols(fips = "c", .default = col_guess()))
sup  <- read_csv("data_derived/county_nearest_ophthalmologist.csv", show_col_types = FALSE,
                 col_types = cols(fips = "c", supply_fips = "c", .default = col_guess()))
ext  <- read_csv("data_derived/external_county_2021.csv", show_col_types = FALSE,
                 col_types = cols(fips = "c", .default = col_guess()))

dat <- long %>%
  inner_join(d %>% select(fips, svi_overall, ophth_per_100k, optom_per_100k,
                          log_pop_dens, rucc, log_drive, drive_min,
                          ophth_patient_care, has_program), by = "fips") %>%
  left_join(sup %>% select(fips, supply_drive_min), by = "fips") %>%
  left_join(ext %>% select(fips, mammography, uninsured, dual_pct,
                           imaging_p1k, em_p1k, tests_p1k, medicaid_expansion), by = "fips") %>%
  filter(!is.na(svi_overall), !is.na(supply_drive_min), is.finite(log_pop_dens)) %>%
  mutate(stratum = paste(race, age), drnon = pmax(dr_cases - vtdr_cases, 0),
         z_svi = z(svi_overall), z_acad = z(log_drive),
         z_supply = z(log1p(supply_drive_min)), z_ophth = z(ophth_per_100k),
         z_optom = z(optom_per_100k), z_popdens = z(log_pop_dens), z_rucc = z(rucc))

CTRL <- "z_acad + z_supply + z_ophth + z_optom + z_popdens + z_rucc + factor(stratum)"
Y <- "cbind(vtdr_cases, drnon)"
qb <- function(rhs, data = dat) glm(as.formula(paste(Y, "~", rhs)), data = data, family = quasibinomial())
getc <- function(m, t) { s <- summary(m)$coefficients; c(s[t,1], s[t,2]) }

# =========================================================================
# FIGURE A1  VTDR share by drive time to nearest academic program
# =========================================================================
binned <- d %>%
  filter(!is.na(drive_min)) %>%
  mutate(bin = cut(drive_min, breaks = c(-1, 30, 60, 90, 120, 180, 240, Inf),
                   labels = c("0-30", "31-60", "61-90", "91-120", "121-180",
                              "181-240", ">240"))) %>%
  mutate(cshare = 100 * vtdr_cases / dr_cases) %>%
  group_by(bin) %>%
  summarise(n = n(),
            share = mean(cshare),
            se    = sd(cshare) / sqrt(n()), .groups = "drop")

pA1 <- ggplot(binned, aes(bin, share)) +
  geom_col(fill = "grey75", width = .68) +
  geom_errorbar(aes(ymin = share - 1.96*se, ymax = share + 1.96*se), width = .16) +
  geom_text(aes(label = paste0(format(n, big.mark=","), "\ncounties"), y = 1.6),
            size = 2.5, colour = "grey25") +
  labs(title = "Severity of diabetic retinopathy does not rise with distance from an academic program",
       subtitle = wr("Share of diagnosed DR that is vision-threatening, by drive time to the nearest ophthalmology residency program", 95),
       x = "Drive time to nearest academic ophthalmology residency program (minutes)",
       y = "Vision-threatening share of\ndiagnosed DR (%)",
       caption = wrc("3,207 US counties, 2021. Bars are mean county shares; whiskers are 95% confidence intervals for the mean, reflecting between-county variation. If proximity to academic ophthalmology protected against progression, this would slope upward from left to right.")) +
  coord_cartesian(ylim = c(0, 24))
ggsave("paper/figs/figA1_drivetime.pdf", pA1, width = 7.6, height = 3.9)

# =========================================================================
# FIGURE A2  the 2x2 discordance
# =========================================================================
med <- median(d$drive_min, na.rm = TRUE)
tab <- d %>% filter(!is.na(drive_min)) %>%
  mutate(local = ifelse(ophth_patient_care == 0, "No local\nophthalmologist", "Has local\nophthalmologist"),
         acad  = ifelse(drive_min > med, "Far from academic", "Near academic")) %>%
  group_by(local, acad) %>%
  summarise(n = n(), share = 100*sum(vtdr_cases)/sum(dr_cases), .groups = "drop")

pA2 <- ggplot(tab, aes(local, share, fill = acad)) +
  geom_col(position = position_dodge(.72), width = .62) +
  geom_text(aes(label = sprintf("%.1f%%", share)),
            position = position_dodge(.72), vjust = -0.5, size = 3) +
  geom_text(aes(label = paste0("n=", format(n, big.mark=","))),
            position = position_dodge(.72), vjust = 1.6, size = 2.4, colour = "grey20") +
  scale_fill_manual(values = c("Near academic" = "grey35", "Far from academic" = "grey78"), name = NULL) +
  labs(title = wr("Counties with local eye care but far from an academic centre are not worse off"),
       subtitle = wr("The comparison requested in peer review: local ophthalmologist supply crossed with academic proximity", 95),
       x = NULL, y = "Vision-threatening share of\ndiagnosed DR (%)",
       caption = wrc("The vertical contrast (academic proximity) is small and favours the near-academic group only slightly. The horizontal contrast (having any local ophthalmologist) is larger, and in the direction expected of greater diagnostic intensity.")) +
  coord_cartesian(ylim = c(0, 24)) + theme(legend.position = "top")
ggsave("paper/figs/figA2_discordance.pdf", pA2, width = 7.6, height = 4.0)

# =========================================================================
# FIGURE A3  forest of academic-distance coefficient across specifications
# =========================================================================
d1 <- dat %>% filter(!is.na(imaging_p1k), !is.na(em_p1k), !is.na(tests_p1k)) %>%
  mutate(z_img = z(imaging_p1k), z_em = z(em_p1k), z_tst = z(tests_p1k))
specs <- list(
  "Unadjusted"                         = qb("z_acad + factor(stratum)"),
  "+ provider supply and geography"    = qb(paste("z_acad + z_supply + z_ophth + z_optom + z_popdens + z_rucc + factor(stratum)")),
  "+ social vulnerability"             = qb(paste("z_svi +", CTRL)),
  "+ Medicare diagnostic volume"       = glm(as.formula(paste(Y, "~ z_svi + z_img + z_em + z_tst +", CTRL)), data = d1, family = quasibinomial()),
  "+ preventive screening"             = glm(as.formula(paste(Y, "~ z_svi + scale(mammography) +", CTRL)), data = dat %>% filter(!is.na(mammography)), family = quasibinomial())
)
fa <- lapply(names(specs), function(n) { v <- getc(specs[[n]], "z_acad")
  tibble(spec = n, beta = v[1], se = v[2]) }) %>% bind_rows() %>%
  mutate(spec = factor(spec, levels = rev(names(specs))))

pA3 <- ggplot(fa, aes(beta, spec)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey45") +
  geom_errorbarh(aes(xmin = beta - 1.96*se, xmax = beta + 1.96*se), height = .14) +
  geom_point(size = 2.4) +
  labs(title = wr("The academic-distance association is small, negative, and stable across specifications"),
       subtitle = wr("Coefficient on drive time to nearest residency program, per SD of log drive time (log-odds of vision-threatening disease)", 95),
       x = "Coefficient (negative = counties farther away have LESS severe disease)", y = NULL,
       caption = wrc("Every specification points the opposite way to the access hypothesis, which predicts a positive coefficient. Adjustment does not move it toward zero, and its magnitude is a fraction of the social-vulnerability coefficient."))
ggsave("paper/figs/figA3_forest.pdf", pA3, width = 8.0, height = 3.4)

# =========================================================================
# FIGURE A4  university vs community programs
# =========================================================================
pairs <- read_csv("data_derived/county_program_drivetimes.csv", show_col_types = FALSE,
                  col_types = cols(fips = "c", .default = col_guess()))
progs <- read_csv("data_derived/programs_geocoded.csv", show_col_types = FALSE) %>%
  mutate(yr = suppressWarnings(as.numeric(accred_year))) %>% filter(is.na(yr) | yr <= 2021) %>%
  mutate(univ = grepl("School of Medicine|University|College of Medicine", name, ignore.case = TRUE))
nb <- function(u) pairs %>% filter(prog_id %in% progs$prog_id[progs$univ == u], !is.na(drive_min)) %>%
  group_by(fips) %>% summarise(m = min(drive_min), .groups = "drop")
d4 <- dat %>%
  left_join(nb(TRUE) %>% rename(univ_min = m), by = "fips") %>%
  left_join(nb(FALSE) %>% rename(comm_min = m), by = "fips") %>%
  filter(!is.na(univ_min), !is.na(comm_min)) %>%
  mutate(z_univ = z(log1p(univ_min)), z_comm = z(log1p(comm_min)))
m4 <- glm(as.formula(paste(Y, "~ z_univ + z_comm + z_svi + z_supply + z_ophth + z_optom + z_popdens + z_rucc + factor(stratum)")),
          data = d4, family = quasibinomial())
f4 <- bind_rows(
  tibble(term = "Distance to nearest\nUNIVERSITY program", beta = getc(m4,"z_univ")[1], se = getc(m4,"z_univ")[2]),
  tibble(term = "Distance to nearest\nCOMMUNITY HOSPITAL program", beta = getc(m4,"z_comm")[1], se = getc(m4,"z_comm")[2]))

pA4 <- ggplot(f4, aes(beta, term)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey45") +
  geom_errorbarh(aes(xmin = beta-1.96*se, xmax = beta+1.96*se), height = .1) +
  geom_point(size = 2.6) +
  labs(title = wr("Only proximity to university programs tracks disease severity"),
       subtitle = wr("Both distances entered in the same model; they correlate 0.61, so they are separately identified", 95),
       x = "Coefficient per SD of log drive time", y = NULL,
       caption = sprintf("n = %s counties with both program types routable. Community-hospital proximity is associated with nothing at all,\nwhich is difficult to reconcile with a training-capacity mechanism and easier to reconcile with subspecialty diagnostic intensity.", format(n_distinct(d4$fips), big.mark=",")))
ggsave("paper/figs/figA4_progtype.pdf", pA4, width = 7.6, height = 2.9)

# =========================================================================
# FIGURE B1  the post-stratification artifact
# =========================================================================
m_un <- glm(cbind(vtdr_cases, pmax(dr_cases-vtdr_cases,0)) ~ z_svi + scale(ophth_per_100k) +
              scale(optom_per_100k) + scale(log_pop_dens) + scale(rucc) + scale(log_drive),
            data = d %>% mutate(z_svi = z(svi_overall)), family = quasibinomial())
m_st <- qb(paste("z_svi +", CTRL))
art <- tibble(spec = c("Published county estimates\n(demographic composition varies)",
                       "Within race x age strata\n(composition held fixed)"),
              beta = c(coef(m_un)["z_svi"], coef(m_st)["z_svi"]),
              se   = c(summary(m_un)$coefficients["z_svi",2], summary(m_st)$coefficients["z_svi",2])) %>%
  mutate(spec = factor(spec, levels = spec))

pB1 <- ggplot(art, aes(spec, beta)) +
  geom_col(fill = c("grey78","grey35"), width = .5) +
  geom_errorbar(aes(ymin = beta-1.96*se, ymax = beta+1.96*se), width = .1) +
  geom_text(aes(label = sprintf("%.3f", beta)), vjust = -1.3, size = 3.4) +
  annotate("segment", x = 1.28, xend = 1.72, y = .112, yend = .112,
           arrow = arrow(length = unit(2,"mm")), colour = "grey20") +
  annotate("text", x = 1.5, y = .122, label = "76% of the apparent\ngradient was artifact",
           size = 2.9, colour = "grey20") +
  labs(title = wr("Most of the apparent social gradient in county DR estimates is a measurement artifact"),
       subtitle = wr("Social vulnerability coefficient on the vision-threatening share, before and after holding demographic composition fixed", 95),
       x = NULL, y = "Coefficient per SD of\nsocial vulnerability (log-odds)",
       caption = wrc("VEHSS builds county estimates by applying national race- and age-specific rates to each county's population mix, so county demographics move the estimate mechanically. Estimating within race x age strata removes that pathway by design.")) +
  coord_cartesian(ylim = c(0, .165))
ggsave("paper/figs/figB1_artifact.pdf", pB1, width = 7.6, height = 3.9)

# =========================================================================
# FIGURE B2  gradient by stratum
# =========================================================================
het <- dat %>% group_by(stratum, race, age) %>% group_modify(~{
  f <- glm(as.formula(paste(Y, "~ z_svi + z_acad + z_supply + z_ophth + z_optom + z_popdens + z_rucc")),
           data = .x, family = quasibinomial())
  s <- summary(f)$coefficients
  tibble(n = nrow(.x), beta = s["z_svi",1], se = s["z_svi",2])
}) %>% ungroup() %>%
  mutate(lab = paste0(gsub(", non-Hispanic","", gsub("Hispanic, any race","Hispanic", race)), ", ", sub(" years","", age)))

pooled <- coef(m_st)["z_svi"]
pB2 <- ggplot(het, aes(beta, reorder(lab, beta))) +
  geom_vline(xintercept = pooled, linetype = 2, colour = "grey45") +
  geom_errorbarh(aes(xmin = beta-1.96*se, xmax = beta+1.96*se), height = .13) +
  geom_point(size = 2.4) +
  labs(title = wr("The social gradient replicates in every demographic stratum"),
       subtitle = wr("Social vulnerability coefficient estimated separately within each race-by-age group", 95),
       x = "Coefficient per SD of social vulnerability (log-odds)", y = NULL,
       caption = sprintf("Dashed line is the pooled within-stratum estimate (%.3f). The same association appears in White, Black and Hispanic\npopulations and in both age bands, which argues against its being an artifact of any one population.", pooled))
ggsave("paper/figs/figB2_strata.pdf", pB2, width = 7.6, height = 3.2)

# =========================================================================
# FIGURE B3  mechanism: what moves the gradient and what does not
# =========================================================================
d2 <- dat %>% filter(!is.na(mammography), !is.na(uninsured), !is.na(dual_pct),
                     !is.na(imaging_p1k), !is.na(em_p1k), !is.na(tests_p1k)) %>%
  mutate(z_img = z(imaging_p1k), z_em = z(em_p1k), z_tst = z(tests_p1k),
         z_mam = z(mammography), z_unins = z(uninsured), z_dual = z(dual_pct))
mech <- bind_rows(
  tibble(step = "Base model",                         v = list(getc(glm(as.formula(paste(Y,"~ z_svi +",CTRL)), d2, family=quasibinomial()),"z_svi"))),
  tibble(step = "+ insurance coverage\n(uninsured, dual-eligible)", v = list(getc(glm(as.formula(paste(Y,"~ z_svi + z_unins + z_dual +",CTRL)), d2, family=quasibinomial()),"z_svi"))),
  tibble(step = "+ diagnostic volume\n(Medicare imaging, E&M, tests)", v = list(getc(glm(as.formula(paste(Y,"~ z_svi + z_img + z_em + z_tst +",CTRL)), d2, family=quasibinomial()),"z_svi"))),
  tibble(step = "+ preventive screening\n(mammography completion)", v = list(getc(glm(as.formula(paste(Y,"~ z_svi + z_mam +",CTRL)), d2, family=quasibinomial()),"z_svi")))
) %>% mutate(beta = sapply(v, `[`, 1), se = sapply(v, `[`, 2),
             step = factor(step, levels = rev(step))) %>% select(-v)

pB3 <- ggplot(mech, aes(beta, step)) +
  geom_vline(xintercept = mech$beta[1], linetype = 3, colour = "grey55") +
  geom_errorbarh(aes(xmin = beta-1.96*se, xmax = beta+1.96*se), height = .13) +
  geom_point(size = 2.6) +
  geom_text(aes(label = sprintf("%.3f", beta)), vjust = -1.1, size = 3) +
  labs(title = wr("Preventive-screening engagement moves the gradient; coverage and diagnostic volume do not"),
       subtitle = wr("Social vulnerability coefficient after adding each set of external covariates, common sample", 95),
       x = "Coefficient per SD of social vulnerability (log-odds)", y = NULL,
       caption = wrc("Mammography completion has no ophthalmic content; it measures whether a county's population completes recommended screening at all. It absorbs ~40% of the gradient, while insurance coverage and diagnostic volume absorb essentially none."))
ggsave("paper/figs/figB3_mechanism.pdf", pB3, width = 8.6, height = 3.8)

# =========================================================================
# FIGURE B4  map of the vision-threatening share
# =========================================================================
shp <- st_read("data_derived/counties_2021.gpkg", quiet = TRUE) %>%
  filter(!STATEFP %in% c("02","15","60","66","69","72","78")) %>%
  left_join(d %>% transmute(GEOID = fips, share = 100*vtdr_cases/dr_cases), by = "GEOID")

pB4 <- ggplot(shp) +
  geom_sf(aes(fill = share), colour = NA) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey92",
                       name = "Vision-threatening\nshare of DR (%)") +
  coord_sf(crs = 5070, datum = NA) +
  labs(title = wr("Geography of diabetic retinopathy severity, not burden"),
       subtitle = wr("Share of diagnosed DR that is vision-threatening, among adults with diabetes, 2021", 95),
       caption = wrc("Continental US shown; Alaska, Hawaii and Puerto Rico omitted. Darker counties have a higher proportion of DR that has reached a vision-threatening stage. This is a case-mix measure and is nearly uncorrelated with DR prevalence itself.")) +
  theme(axis.text = element_blank(), panel.grid = element_blank(),
        legend.position = c(.93,.28), legend.key.width = unit(3.2,"mm"),
        legend.key.height = unit(7,"mm"), legend.title = element_text(size = 7.5),
        legend.text = element_text(size = 7))
ggsave("paper/figs/figB4_map.pdf", pB4, width = 7.6, height = 5.0)

message("Figures written to paper/figs/:")
print(list.files("paper/figs"))
