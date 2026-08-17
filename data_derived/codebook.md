# Codebook: `county_analytic_2021.csv`

One row per US county / county-equivalent. Merge key is 5-digit FIPS (`fips`).
Built by `R/01`–`R/09`. Every variable's provenance is given below, which also
supplies the data-source citations Reviewer 2 asked for twice (their comments
on lines 119–122 and 133–135).

## Geographic scope

- Analytic universe is the **3,139 counties with VEHSS diabetic retinopathy
  estimates**. Puerto Rico (78 county-equivalents) is absent from the VEHSS
  source entirely and cannot be recovered; this is a genuine source limitation
  and belongs in Limitations.
- **Washington, DC is present.** It was missing from the earlier workbook only
  because DC's `CountyName` field is blank in the raw CDC export. It should not
  be described alongside the Puerto Rico limitation.
- **Connecticut appears as the 8 pre-2022 counties** (09001–09015). The 2021 ACS
  and SVI 2020 both still use that geography. The "8 missing CT counties" in the
  earlier workbook were an artifact of pulling 2023 ACS, whose planning-region
  FIPS do not match. No crosswalk is required at the 2021 vintage.

## Identifiers

| Variable | Description | Source |
|---|---|---|
| `fips` | 5-digit county FIPS | — |
| `county`, `state` | County and state name | Census Centers of Population 2020 |

## Outcomes (VEHSS)

Source: CDC Vision & Eye Health Surveillance System, Composite Prevalence
Estimates, Socrata dataset `qeru-k2y2` (`data.cdc.gov`).
Method citation: Lundeen EA, Burke-Conte Z, Rein DB, et al. Prevalence of
diabetic retinopathy in the US in 2021. *JAMA Ophthalmol.* 2023;141(8):747–754.

All DR variables use `riskfactorresponse = "Yes"`, so the **denominator is
people with diabetes**, and `Crude Prevalence` rows, so numerator and
denominator are internally consistent.

| Variable | Description |
|---|---|
| `dm_pop` | Modeled population with diabetes (VEHSS `sample_size`) |
| `dr_cases` | Modeled all-stage DR cases (VEHSS `numerator`) |
| `vtdr_cases` | Modeled vision-threatening DR cases |
| `dr_prev_dm` | 100 × `dr_cases` / `dm_pop`. DR per 100 people with diabetes |
| `vtdr_prev_dm` | 100 × `vtdr_cases` / `dm_pop` |
| **`vtdr_share_crude`** | **Primary outcome.** `vtdr_cases` / `dr_cases`, all ages |
| `vtdr_share_std` | Age-standardized version, weighting stratum-specific shares by the national distribution of DR cases across VEHSS age strata |
| `vtdr_share_6584`, `dr_cases_6584`, `vtdr_cases_6584`, `dm_pop_6584` | 65–84 stratum, for the age sensitivity analysis |
| `glauc_pop`, `glauc_cases`, `vag_cases`, `glauc_share` | Glaucoma equivalents (vision-affecting / all glaucoma), **2022 vintage** |

**Validation.** Pooling county numerators reproduces the published national
figures: DR 26.43% of people with diabetes (published 26.43%), VTDR 5.05%
(published 5.06%), VTDR share 19.1% (published 1.84M/9.60M = 19.2%).

**Vintage mismatch to disclose.** Each VEHSS indicator is published for exactly
one year and the years differ: DR is 2021, glaucoma is 2022. This is a source
constraint, not a choice.

**Interpretation caveat that must appear in Methods.** VEHSS county estimates
are modeled from *diagnosed* disease in Medicare and Medicaid claims, and the
county random effects were estimated controlling for ophthalmologists per
capita. The VEHSS authors write that "claims diagnoses are likely influenced by
access to ophthalmologists... our model may mistake these factors for true
variation in prevalence." Consequences: (a) the DR *level* partly measures
detection, which is why the severity share is the primary outcome; (b) including
ophthalmologist density as a covariate is partly a double adjustment. VEHSS does
**not** use poverty or SDOH predictors, so SVI adjustment is not circular.

## Exposures

| Variable | Description | Source |
|---|---|---|
| `drive_min` | Drive minutes to the nearest ACGME ophthalmology residency program, from the population-weighted centroid. 0 for counties containing a program | OSRM road routing |
| `log_drive` | `log1p(drive_min)` | derived |
| `nearest_prog_id`, `nearest_gc_miles` | Nearest program and its great-circle distance | derived |
| `has_program`, `n_programs` | Contains ≥1 program accredited ≤2021 | FRIEDA/ACGME roster |
| `supply_drive_min` | Drive minutes to the nearest county with ≥1 patient-care ophthalmologist. Comparison exposure isolating what is unique to academic centers | OSRM + AHRF |

Program roster: 128 programs from FRIEDA/ACGME; **123 accredited ≤2021, in 90
counties**, used for the 2021 cross-section. Geocoded via the Census geocoder
(104), Nominatim (17), and manual facility lookup (7); counties assigned by
point-in-polygon against TIGER 2021, not by geocoder self-report.

Routing limitations for Methods: OSRM car routing ignores traffic and ferries,
and **public transit is not modeled at all**. Island and remote counties with no
road route are recorded as `NA` rather than dropped silently.

## Covariates

| Variable | Description | Source |
|---|---|---|
| `total_pop` | Total population | 2021 ACS 5-year, B01001 |
| `a0_17` … `a85p`, `pct_65plus` | Age structure | 2021 ACS 5-year, B01001 |
| `pct_nh_white` … `pct_hispanic` | Race/ethnicity composition | 2021 ACS 5-year, B03002 |
| `svi_overall` | **Control variable.** Overall SVI percentile (`RPL_THEMES`) | CDC/ATSDR SVI 2020 (2016–2020 ACS) |
| `svi_ses`, `svi_hhchar`, `svi_minority`, `svi_housing` | SVI sub-themes | same |
| `ophth_patient_care`, `ophth_per_100k` | Patient-care ophthalmologists (`md_nf_ophth_all_pc_21`) | AHRF 2022–2023 |
| `ophth_total`, `ophth_residents`, `ophth_teaching` | Total, residents, teaching ophthalmologists, 2021 | AHRF 2022–2023 |
| `optom_npi`, `optom_per_100k` | Optometrists with an NPI, 2021 | AHRF 2022–2023 |
| `rucc`, `metro` | Rural-Urban Continuum Code 2013; metro indicator | AHRF 2022–2023 |
| `land_area_sqmi`, `pop_density`, `log_pop_dens` | Land area and population density, answering Reviewer 2's county-area objection | TIGER 2021 + ACS |
| `pw_lat`, `pw_lon` | Population-weighted centroid | Census Centers of Population 2020 |
| `dm_prev` | 100 × `dm_pop` / `total_pop` | derived |

SVI `-999` missing codes are recoded to `NA`. SVI 2020 has **zero** missing
counties in the 50 states plus DC.

**AHRF note.** The eye-care workforce columns come from the AHRF **2022–2023**
release, which is the vintage carrying 2021-stamped counts. The newer 2024–2025
release only reaches back to 2022 and would not align with the analytic year.
National totals: 19,674 ophthalmologists, 58,285 optometrists, 2,363
ophthalmology residents. Resident counts derive from the AMA Physician
Masterfile and are recorded at address of record, so they scatter modestly
beyond program counties (79% fall in counties with an accredited program); they
are used as a capacity weight, not as a program locator.

## Data sources, full URLs

- VEHSS: `https://data.cdc.gov/resource/qeru-k2y2.json`
- ACS 2021 5-year: Census API, `https://api.census.gov/data/2021/acs/acs5`
- CDC/ATSDR SVI 2020: `https://svi.cdc.gov/Documents/Data/2020/csv/states_counties/SVI_2020_US_county.csv`
- AHRF 2022–2023: `https://data.hrsa.gov/DataDownload/AHRF/AHRF_SAS_2022-2023.zip`
- Census Centers of Population 2020: `https://www2.census.gov/geo/docs/reference/cenpop2020/county/CenPop2020_Mean_CO.txt`
- TIGER/Line 2021 county boundaries: via `tigris`
- OSRM routing: `https://router.project-osrm.org`
