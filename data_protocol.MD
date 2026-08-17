# Data Collection Protocol for Hannah Becker

## Request (07-20-2026)

This is a practical follow-up about publicly available data sources — no medical literature search is needed, as the answer refines information already discussed.

All of the following databases are freely available to the public with no data use agreement, institutional affiliation, or fee required.

1. AHRF (Area Health Resources File) — Best Starting Point

- URL: data.hrsa.gov (https://data.hrsa.gov/) → search "Area Health Resources Files"

- Format: Downloadable .csv or SAS flat file with accompanying data dictionary

- What it provides: County-level counts of nonfederal ophthalmologists, optometrists, and other specialists; county population; sociodemographic variables

- Update cycle: Annual

- Cost: Free, no registration required

2. NPPES (National Plan and Provider Enumeration System)

- URL: download.cms.gov/nppes/NPIFiles.html (https://download.cms.gov/nppes/NPI%3C/em%3EFiles.html)

- Format: Large .csv file (full NPI registry, ~8 GB uncompressed); also available via API

- What it provides: Every NPI-registered provider with specialty taxonomy code, practice address (ZIP-level), and credential type

- Key filter: Taxonomy code 207W00000X (ophthalmology), 152W00000X (optometry)

- Limitation: Includes inactive/retired providers — cross-reference with Medicare claims data (below) to confirm active practice

- Cost: Free, no registration

3. CMS Medicare Physician & Other Practitioners Utilization Data

- URL: data.cms.gov (https://data.cms.gov/) → search "Medicare Physician & Other Practitioners"

- Format: Downloadable .csv files by year

- What it provides: Provider NPI, specialty, practice ZIP, number of beneficiaries seen, total claims — confirms who is actively practicing and approximate volume

- Key filter: Specialty description = "Ophthalmology" or "Optometry"

- Cost: Free, no registration

Free Supporting Datasets for Merging

- ZIP-to-County (FIPS) Crosswalk:

- HUD USPS ZIP Code Crosswalk Files → huduser.gov/portal/datasets/uspscrosswalk.html (https://www.huduser.gov/portal/datasets/usps%3C/em%3Ecrosswalk.html) (free, requires free HUD account)

- Census Bureau ZCTA-to-County Relationship File → census.gov (https://www.census.gov/geographies/reference-files.html)

- County Population Estimates:

- Census Bureau Annual Estimates → https://www.census.gov/programs-surveys/popest.html

- Also included within the AHRF file itself

- Urban-Rural Classification:

- NCHS Urban-Rural Classification (6-level) → cdc.gov/nchs/dataaccess/urbanrural.htm (https://www.cdc.gov/nchs/data%3Cem%3Eaccess/urban%3C/em%3Erural.htm)

- USDA Rural-Urban Continuum Codes (9-level) → ers.usda.gov (https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/)

- County Boundary Shapefiles (for mapping):

- Census TIGER/Line Files → https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html

Recommended Workflow Using Only Free Data

1. Download the AHRF flat file — this alone gives ophthalmologist and optometrist counts per county with population denominators already included

2. Merge with NCHS urban-rural codes by FIPS for stratification

3. Calculate density: `(provider count / population) × 100,000`

4. For more granular or current data, download NPPES + CMS Medicare utilization, cross-reference to identify actively practicing ophthalmologists, geocode by ZIP-to-FIPS crosswalk, and aggregate to county

5. Map results using Census TIGER/Line shapefiles in R (`sf` + `ggplot2`), Python (`geopandas`), or QGIS (all free)

The AHRF alone is sufficient to replicate the methodology of most published studies on this topic.[1] (https://pubmed.ncbi.nlm.nih.gov/32445703)[2] (https://pubmed.ncbi.nlm.nih.gov/42390569) Adding NPPES + CMS data allows for more current and granular analysis similar to the approach used by Soares et al.[3] (https://pubmed.ncbi.nlm.nih.gov/36427602)

Would you like to explore how to identify specific ophthalmology subspecialties (e.g., retina, glaucoma) within these datasets, as taxonomy codes allow for subspecialty-level filtering?

References
    1    National Trends in the United States Eye Care Workforce From 1995 to 2017 (https://pubmed.ncbi.nlm.nih.gov/32445703). Feng PW, Ahluwalia A, Feng H, Adelman RA. American Journal of Ophthalmology. 2020;218:128-135. doi:10.1016/j.ajo.2020.05.018.
    2    Eye Care Workforce Density by Provider Type in Urban and Rural Counties in the United States, 2010 to 2021 (https://pubmed.ncbi.nlm.nih.gov/42390569). Shah J, Ahmed H, Weiss MB, et al. Graefe's Archive for Clinical and Experimental Ophthalmology = Albrecht Von Graefes Archiv Fur Klinische Und Experimentelle Ophthalmologie. 2026;:10.1007/s00417-026-07356-4. doi:10.1007/s00417-026-07356-4.
    3    Patient Accessibility to Eye Care in the United States (https://pubmed.ncbi.nlm.nih.gov/36427602). Soares RR, Mokhashi N, Sharpe J, et al. Ophthalmology. 2023;130(4):354-360. doi:10.1016/j.ophtha.2022.11.017.

## Area Health Resources Files: 2019 - 2024

The ARHF are located at the following website: https://data.hrsa.gov/topics/health-workforce/nchwa/ahrf.

As of 07-21-2026, the 2024 opthamology data is not updated. There are files for 2019-2023. 

It seems that to get county-level data, the user must download the data for each state-year individually (e.g., you have to download 2023 data for Alabama, then Alaska, etc.). I can attempt to do this entire process as an API call, which may do it instanteously, but it will take me 20-30 minutes to set up. 

A potential option is to see how stable the per capita data is from 2019-2023 to see how sticky provider rates are (how much they change on a year-to-year basis), which will help us understand how generalizable they are outside the date range.

## National Plan and Provider Enumeration System

To bulk download the data, it seems like there are mostly NPPES and NPI files (https://download.cms.gov/nppes/NPI_Files.html). It doesn't look like there is historical data, or at least that's not readily available per a few minute check. If that's the case, we will only be able to get the most recent snapshot. 

## CMS Data

There's annual state-level data (https://data.cms.gov/provider-summary-by-type-of-service/medicare-physician-other-practitioners/medicare-physician-other-practitioners-by-geography-and-service) available from 2019-2024. You'll need to do a bit more digging to see if there's any information on the county-level.

## Supporting Datasets

All of these datasets are readily available and I have experience using them in the past for public health projects.