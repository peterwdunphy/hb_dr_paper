# Revision Analysis Plan: DR Burden and Access to Academic Ophthalmology

Status: proposed, 2026-08-16. Target: JAMA Ophthalmology. Analytic year: 2021.

---

## 0. Summary of feasibility checks already run

Everything below was tested live against the actual endpoints on 2026-08-16, not assumed.

| Need | Status | Where it comes from |
|---|---|---|
| Ophthalmologist / optometrist density (the "blocking gap") | **Solved** | AHRF 2022-2023, single national county file, already downloaded |
| DR case counts + denominators + CIs | **Solved** | CDC VEHSS Socrata API, dataset `qeru-k2y2` |
| Vision-threatening vs non-vision-threatening DR | **Available, not previously used** | Same VEHSS dataset |
| 2021 ACS population | **Solved** | Census API, key works |
| Population-weighted centroids | **Solved, no tract work needed** | Census 2020 Centers of Population file |
| SVI | **Solved** | CDC/ATSDR SVI 2020 county CSV, direct download |
| Drive times | **Feasible with chunking** | Public OSRM, 100-coordinate cap per request |
| Connecticut geography mismatch | **Dissolves on its own** | Caused by using 2023 ACS; 2021 ACS uses old county FIPS |

Nothing on this list requires a manual point-and-click download or a data use agreement.

---

## 1. Two findings that change the study

### 1.1 The AHRF gap was never real

`data_protocol.MD` records that AHRF appears to require downloading each state-year separately. That is wrong. HRSA publishes one national county-level file per vintage. The 2022-2023 vintage carries **2021-stamped** columns, which is exactly the year we want:

- `md_nf_ophth_21` total non-federal ophthalmologists
- `md_nf_ophth_all_pc_21` patient-care ophthalmologists (use this for density)
- `md_nf_ophth_pc_rsdnt_21` **ophthalmology residents, by county**
- `md_nf_ophth_teach_21` ophthalmologists whose primary activity is teaching
- `opto_npi_21` optometrists with an NPI

3,231 rows, 4,306 columns. Downloaded and verified. The last two columns matter more than the density request that prompted the search: they let us measure academic ophthalmology capacity as a continuous quantity instead of asserting that a residency program implies better care.

### 1.2 The outcome variable has a defect neither reviewer caught

VEHSS county estimates are not survey measurements. Per Lundeen et al. (JAMA Ophthalmol 2023), the methods paper behind the data:

> "We estimated county-level random effects using 2018 Medicaid claims and 2017 to 2019 claims in the Medicare Part B fee-for-service program."

> "We controlled for county-level number of ophthalmologists per capita, which could affect access to diagnosis."

> "Claims diagnoses are likely influenced by access to ophthalmologists and local variation in the Medicare and Medicaid covered population... our model may mistake these factors for true variation in prevalence."

Three consequences, in order of severity:

1. **County DR prevalence measures diagnosed disease.** Where access is worse, less DR gets diagnosed. The expected association between poor access and measured DR burden runs *backwards* from the paper's premise. The current framing (high DR + far from an academic center = unmet need) may be picking up the opposite of what it claims.
2. **Ophthalmologist density is already partially inside the outcome.** Reviewer 2 asked us to add it as a covariate. Doing so is a partial double-adjustment that will attenuate the access coefficient by construction. We should still do it, but we have to say this out loud in Methods rather than let a JAMA Ophthalmology reviewer find it.
3. **The good news:** VEHSS does *not* use poverty, SVI, or any SDOH predictor. So using SVI as a control is clean and non-circular. Dr. Dunphy's instinct on SVI holds up.

This is a larger threat to the manuscript than anything in the rejection letter, and it will be caught on the next round if we do not get in front of it.

---

## 2. Proposed design change

### 2.1 Change the outcome from burden to severity case-mix

**Primary outcome:** among adults with diabetes and diagnosed DR, the proportion whose disease is vision-threatening.

```
VTDR share = VTDR cases / all-stage DR cases
```

Both numerators come from VEHSS at the county level, stratified by age.

Why this fixes multiple problems at once:

- **It survives the detection artifact.** The *level* of diagnosed DR is driven by who gets screened. The *severity mix* is far less so, because vision-threatening disease is symptomatic and presents for care even where routine screening is poor. Differential detection mostly inflates the denominator of mild cases, which pushes the share down in well-served counties and up in poorly served ones. The artifact becomes the signal instead of the bias.
- **It answers the editor's "not novel" objection.** Diabetes prevalence and SVI largely divide out of a ratio. We are no longer re-reporting that poor counties have more diabetic eye disease. We are asking a different question: conditional on having DR, is it caught later in some places?
- **It answers "what is special about academic centers."** This is the question Dr. Dunphy flagged as the biggest unaddressed item, and it is the one the current draft cannot answer. Vision-threatening DR is the stage that requires retina subspecialty intervention: panretinal photocoagulation, intravitreal anti-VEGF, vitrectomy. That is capacity a general ophthalmologist or an optometrist does not supply. The claim becomes specific and testable rather than an assertion about "anchoring tertiary referral networks." And with `md_nf_ophth_pc_rsdnt_21` and `md_nf_ophth_teach_21` we can *measure* academic capacity instead of asserting it from a binary flag.
- **It gives a single primary objective,** per Reviewer 1 comment 2.

**Honest caveat to write into Limitations:** VTDR and all-stage DR are modeled from overlapping claims, so the ratio is not fully immune to detection effects. It is better, not clean. We should say so.

### 2.2 Add a negative-control outcome

Reviewer 2's central complaint was that the index was never validated. A negative control is cheap here and directly responsive.

Run the identical model on **glaucoma prevalence** and **uncorrected refractive error**, both in the same VEHSS table for the same counties. Neither is a plausible target of academic ophthalmology referral capacity in the way VTDR is. If distance to an academic program "predicts" all three equally, we are looking at a detection artifact and we should know that before submission, not after. If it predicts VTDR share specifically, that is a real validation result and a strong paragraph.

### 2.3 Handle age by standardization, not by a cutoff

Reviewer 1 comments 6 and 7 attack the age-60 cutoff. Moving to 65 (the current plan) does not actually answer the objection, it just picks a different arbitrary line, and it walks straight into the "enriches the insured population" criticism since 65 *is* Medicare eligibility.

Better: VEHSS gives numerators and denominators by age stratum (0-17, 18-39, 40-64, 65-84, 85+). That means we can compute **directly age-standardized** rates to the 2000 US standard population. No cutoff, no arbitrary threshold, no age-composition confounding. This retires both comments completely rather than partially.

### 2.4 Drop the Priority Index

Reviewers said validate or drop. Validating a composite with no external criterion is not achievable on this timeline. Drop the index as an analytic object. Keep one choropleth map as a descriptive figure with no composite score attached. This also removes the title problem in Reviewer 1 comment 1.

---

## 3. Data build

Directory layout:

```
hb_dr_paper/
  data_raw/        # untouched downloads, gitignored (large)
  data_derived/    # analytic files, committed (small)
  R/               # numbered build scripts
  output/          # tables, figures
  paper/           # LaTeX
```

### Step 1. ACS 2021 5-year (API, key already works)

Pull `B01001` (age-sex) for all counties, 2021 ACS 5-year. Gives total population and any age aggregation we want. Critically, the 2021 vintage uses **old Connecticut county FIPS** (09001-09015), which matches VEHSS and SVI 2020. The 8 missing CT counties in the current workbook are an artifact of the 2023 ACS pull, not a real gap, and no crosswalk is needed once the year is corrected.

Also pull county land area for density (from the TIGER shapefile's ALAND, or `B01003` plus the gazetteer file).

### Step 2. VEHSS (API, no key needed)

Dataset `qeru-k2y2`. Filter: `geographiclevel = 'County'`, `yearstart = '2021'`, `question = 'Prevalence of Diabetic Retinopathy'`, `sex = 'Both sexes'`, `raceethnicity = 'All races'`, `riskfactorresponse = 'Yes'` (denominator is people with diabetes). Keep both `response = 'All DR stages'` and `response = 'Vision threatening stage'`, across all five age strata.

Retain `numerator` and `sample_size`, which are the case count and the diabetes-population denominator the reviewers asked for. Retain `low_confidence_limit` / `high_confidence_limit`, which are populated for all counties in the raw source. The Alabama-only CI problem in the workbook was an export artifact.

Coverage confirmed: **3,141 counties**. Puerto Rico genuinely absent from the source, so the existing decision to document it in Limitations stands. DC is present.

Repeat the same pull for glaucoma and URE for the negative control.

### Step 3. SVI 2020 (direct download)

`https://svi.cdc.gov/Documents/Data/2020/csv/states_counties/SVI_2020_US_county.csv`. Verified live, 2.3 MB. SVI 2020 is built on 2016-2020 ACS and uses old CT counties, so it lines up with steps 1 and 2. Use `RPL_THEMES` (overall percentile ranking) only, per the decision to keep SVI as a single control.

### Step 4. AHRF (already downloaded)

`AHRF_SAS_2022-2023.zip`, currently in the session scratchpad. Move it into `data_raw/`. Read with `haven::read_sas()`. Keep `fips_st_cnty` plus the six 2021 columns listed in section 1.1.

Note the AHRF DUA `.doc` in the zip. It is a click-through acknowledgment, not a restrictive agreement, but read it before the data goes in a public repo.

### Step 5. Population-weighted centroids (direct download)

`https://www2.census.gov/geo/docs/reference/cenpop2020/county/CenPop2020_Mean_CO.txt`. Verified live, 171 KB.

This is the Census Bureau's own 2020 Center of Population file. It is exactly the tract-weighted calculation Dr. Dunphy described, already computed and published by Census. Using the official file is both less work and more defensible than rolling our own, and it is what the geospatial health services literature cites.

### Step 6. Program locations and drive times

Two sub-tasks.

**6a. Geocode the 128 programs.** 23 of the 128 rows in `Ophtho_Programs_Reference` have the full address collapsed into the `Address` field with `City`/`State`/`Zipcode` blank, so those need parsing first. Then geocode through the Census Geocoder batch API (free, no key, up to 10,000 addresses per submission). Hand-check any failures.

Also filter to programs with ACGME accreditation year <= 2021. The list includes at least one 2021 program (UT Austin Dell), and anything accredited after 2021 should not be in a 2021 cross-section.

**6b. Drive times.** Public OSRM works. Verified: both `router.project-osrm.org` and `routing.openstreetmap.de/routed-car` return `/table` responses, capped at **100 total coordinates per request** (150 returns HTTP 400). Two options:

- *Option A, public server.* Prefilter county-program pairs by great-circle distance (drive time cannot be less than haversine at ~70 mph, so anything beyond ~200 miles cannot be inside a 120-minute catchment). Then chunk into requests of roughly 90 counties plus 10 programs. Estimated a few hundred requests, on the order of an hour with polite rate limiting. No cost, no key. Risk: it is a shared community server with a fair-use policy, and it can throttle.
- *Option B, local OSRM.* Docker is installed on this machine. Pull the US OSM extract and run `osrm-extract` / `osrm-partition` / `osrm-customize` locally. Heavier setup (large download, several hours of preprocessing, substantial RAM) but fully reproducible, unlimited queries, and it is what a methods reviewer would prefer to see cited.

Recommendation: start with Option A to get numbers moving, and stand up Option B in parallel if we commit to the drive-time framing for the final submission.

On Google Maps: the Distance Matrix API free tier is roughly $200/month of credit, about 40,000 elements. We would need on the order of hundreds of thousands of elements before prefiltering, so it would likely cross into paid. OSRM is the better answer and is also more citable.

**Public transit is out of scope** and goes in Limitations, as Dr. Dunphy noted. OSRM car routing also ignores traffic, which is worth one more sentence.

### Step 7. Merge

Left join everything on 5-digit county FIPS. Deliverable is `data_derived/county_analytic_2021.csv`, one row per county, with a companion `codebook.md` giving source, vintage, and construction for every column. Reviewer 2 asked for source citations for the CDC and medical school data (their minor comments), so the codebook doubles as the raw material for that.

---

## 4. Models

Primary exposure: spatial access to academic ophthalmology capacity. Primary outcome: age-standardized VTDR share.

**Model 1.** Binomial GLM, VTDR cases as successes out of all-stage DR cases, exposure only. Binomial is the right family here because we have real numerators and denominators, which is the whole point of pulling the raw VEHSS counts. Check for overdispersion and move to quasi-binomial or beta-binomial if needed.

**Model 2.** Add ophthalmologist density, optometrist density, county population density, rurality.

**Model 3.** Add SVI (`RPL_THEMES`). This is Dr. Dunphy's specification: controlling for social vulnerability, what does access tell us.

**Model 4, spatial.** CARBayes, per Dr. Dunphy's preference. `CARBayes::S.CARleroux()` with a binomial likelihood and a queen-contiguity neighborhood from the TIGER county shapefile takes counts and trials directly, so it fits the outcome without transformation. Given Moran's I = 0.45 in the original, we should assume from the start that the spatial model is the headline specification and the OLS-style models are the build-up, not the reverse.

Two practical notes on CARBayes: it needs `spdep` for the neighbor list, and islands (Nantucket, Hawaii counties, and similar) have no queen neighbors and will need explicit handling. Neither is hard, both need a decision recorded in Methods.

**Supplement.** The categorical versions Dr. Dunphy approved: distance quartiles and a high-burden binary, so the AJO-style table exists for anyone who wants it.

**Sensitivity analyses.** Alternative catchment thresholds if we go the E2SFCA route; age-stratum-specific models; the DR *level* model reported alongside the severity model. That last contrast is worth foregrounding rather than burying, because if level and severity move in opposite directions, that pattern *is* the detection story and it is the most interesting thing in the paper.

**Correlations.** Reviewer 2 asked us to pick one. Use Spearman throughout. County health data is skewed and we already know it.

---

## 5. What I need decided before building

1. **Outcome switch to VTDR severity share.** This is the load-bearing decision. Everything in section 4 assumes it. If we keep DR prevalence as the primary outcome, we should expect the same "not novel" response plus a new detection-artifact critique.
2. **Denominator.** People with diabetes (VEHSS `sample_size`) rather than total county population. Recommended, and it is what makes the severity framing coherent.
3. **Ophthalmologist density as a covariate**, given that it is partially inside the outcome. Recommendation: include it, report the model with and without, and disclose the double-adjustment in Methods.
4. **OSRM public vs local.** Affects timeline more than results.
5. **How hard we commit to E2SFCA.** The severity framing does not strictly require it. A simpler capacity-weighted access measure may be easier to defend and easier to explain, and Reviewer 1 already complained the paper was hard to follow. Worth a conversation.
6. **Author-level question:** is anyone on the team able to speak to the clinical claim that VTDR management specifically requires subspecialty capacity? The argument is standard, but the paper needs a real clinical citation behind it, not a methods-driven assertion.

---

## 6. Setup commands

R packages currently missing on this machine:

```r
install.packages(c("spdep", "CARBayes", "osrm", "readxl", "spatialreg",
                   "haven", "janitor", "httr2", "lmtest"))
```

Already installed: `sf`, `tidycensus`, `tigris`, `dplyr`, `data.table`, `ggplot2`, `fixest`, `MASS`, `tidyr`, `sandwich`.

Census API key, once per machine. Request a free key at
<https://api.census.gov/data/key_signup.html>, then:

```r
tidycensus::census_api_key("YOUR_KEY_HERE", install = TRUE)
```

This writes the key to `~/.Renviron` as `CENSUS_API_KEY`, which is where
`R/01_acs.R` reads it from. Never paste the key into a script or a tracked file:
this repository is public, and a key committed once stays in the git history
even after it is deleted.

Repo setup. This directory is currently inside the home-directory git repo, whose remote points at `peterdunphy.github.io`, so committing from here would push into the wrong repository. Initialize the paper repo separately:

```bash
# from a clean location, e.g. ~/Documents
git clone https://github.com/peterwdunphy/hb_dr_paper.git
cd hb_dr_paper
mkdir -p data_raw data_derived R output paper

# keep large raw downloads out of version control
printf 'data_raw/\n.Renviron\n.Rhistory\n.RData\n*.sas7bdat\n' > .gitignore

git add .gitignore
git commit -m "Scaffold analysis repo"
git push
```

Then copy `analysis_plan.md`, `data_protocol.MD`, and the received workbook into it.

---

## 7. Mapping back to the reviewer comments

| Comment | Response |
|---|---|
| R1.1 title confusing | New title follows from the single objective |
| R1.2 single primary objective | VTDR severity vs access. Index dropped |
| R1.3 vague claim about academic centers | Replaced with a specific, measured claim about subspecialty capacity for vision-threatening disease |
| R1.4 no evidence academic-poor counties lack resources; confounding with urban provider density | Ophthalmologist and optometrist density, population density, and rurality now enter directly |
| R1.5 unclear "unmet need" sentence | Removed with the index |
| R1.6, R1.7 age-60 cutoff arbitrary, enriches insured | Direct age standardization, no cutoff at all |
| R1.8 why do residency programs matter | Section 2.1 argument plus AHRF resident and teaching counts as the measure |
| R1.9, R1.10 SVI findings well known | SVI demoted to a single control. Not a result |
| R2 index not validated | Index dropped. Negative-control outcome added |
| R2 high ophthalmologist density but far from academic center | Directly modeled |
| R2 line 141 rate vs count ambiguity | VEHSS numerator and denominator carried explicitly through to the codebook |
| R2 line 145-147 index construction unclear | Moot |
| R2 county area not accounted for | Population density included; centroids population-weighted |
| R2 references out of order, wording, DC/PR repetition, missing citations | Editorial pass; codebook supplies the data citations |
| R2 pick Pearson or Spearman | Spearman |
