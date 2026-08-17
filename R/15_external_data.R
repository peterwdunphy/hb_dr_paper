# 15_external_data.R ------------------------------------------------------
# Assemble external validation data from sources independent of VEHSS.
#
# The central threat to the main finding is DETECTION: if mild diabetic
# retinopathy goes undiagnosed more often in disadvantaged counties, the
# vision-threatening SHARE rises without any true difference in progression.
# Provider density is a weak proxy for that. These sources give us better ones,
# and give us outcomes measured in entirely different data systems.
#
# SOURCE 1 -- CMS Medicare Geographic Variation, county level, 2021.
#   Gives, for the Original Medicare population in each county:
#     BENE_DUAL_PCT             share dually eligible for Medicaid. Within the
#                               65+ population everyone has Medicare, so this
#                               isolates POVERTY among the fully insured.
#     IMGNG_EVNTS_PER_1000      imaging events per 1,000 beneficiaries
#     EM_EVNTS_PER_1000         evaluation & management events per 1,000
#     TESTS_EVNTS_PER_1000      diagnostic tests per 1,000
#   The three utilization measures are direct county-level indices of how much
#   diagnostic activity happens, independent of VEHSS. They are the strongest
#   available control for differential detection.
#
# SOURCE 2 -- County Health Rankings 2021.
#     v085 uninsured rate            direct coverage measure
#     v050 mammography screening     completion of a recommended preventive
#                                    screen, from Medicare claims. A county's
#                                    general screening follow-through.
#     v155 flu vaccination           second preventive-service proxy
#     v005 preventable hospital stays ambulatory-care-sensitive admissions: an
#                                    independent measure of ambulatory care
#                                    failure, in a different organ system and a
#                                    different data system from VEHSS
#     v060 diabetes prevalence       independent diabetes denominator check
#     v004 primary care physicians, v063 median income, v058 % rural,
#     v137 long commute, v166 broadband
#
# SOURCE 3 -- Medicaid expansion status as of 2021 (KFF).
#   Twelve states had not adopted expansion as of 2021. Missouri and Oklahoma
#   implemented mid-2021 and are flagged separately rather than forced into
#   either group.
# -------------------------------------------------------------------------

library(dplyr); library(readr); library(jsonlite); library(stringr)

dir.create("data_derived", showWarnings = FALSE)

# ---- 1. CMS Medicare Geographic Variation ---------------------------------
cms_raw <- "data/cms_geovar_county_2021.json"
if (!file.exists(cms_raw)) {
  u <- paste0("https://data.cms.gov/data-api/v1/dataset/",
              "6219697b-8f6c-4164-bed4-cd9317c58ebc/data",
              "?size=5000&offset=0",
              "&filter%5BYEAR%5D=2021",
              "&filter%5BBENE_GEO_LVL%5D=County",
              "&filter%5BBENE_AGE_LVL%5D=All")
  download.file(u, cms_raw, quiet = TRUE)
}
cms <- fromJSON(cms_raw, flatten = TRUE)
message("CMS county rows: ", nrow(cms))

num <- function(x) suppressWarnings(as.numeric(ifelse(x %in% c("*", "NA", ""), NA, x)))

cms2 <- cms %>%
  transmute(
    fips          = BENE_GEO_CD,
    mcr_benes     = num(BENES_TOTAL_CNT),
    mcr_avg_age   = num(BENE_AVG_AGE),
    dual_pct      = num(BENE_DUAL_PCT),
    imaging_p1k   = num(IMGNG_EVNTS_PER_1000_BENES),
    em_p1k        = num(EM_EVNTS_PER_1000_BENES),
    tests_p1k     = num(TESTS_EVNTS_PER_1000_BENES)
  ) %>%
  filter(!is.na(fips), nchar(fips) == 5)

message("  dual_pct present: ", sum(!is.na(cms2$dual_pct)),
        " | imaging: ", sum(!is.na(cms2$imaging_p1k)))

# ---- 2. County Health Rankings 2021 ---------------------------------------
# Row 1 is human-readable labels and row 2 is the variable codes, so skip = 1
# makes the code row the header.
chr <- read_csv("data/chr_2021.csv", skip = 1, show_col_types = FALSE,
                locale = locale(encoding = "latin1"),
                col_types = cols(.default = col_character()))

chr2 <- chr %>%
  transmute(
    fips             = str_pad(fipscode, 5, "left", "0"),
    uninsured        = as.numeric(v085_rawvalue),
    mammography      = as.numeric(v050_rawvalue),
    flu_vax          = as.numeric(v155_rawvalue),
    prevent_hosp     = as.numeric(v005_rawvalue),
    chr_diabetes     = as.numeric(v060_rawvalue),
    pcp_ratio        = as.numeric(v004_rawvalue),
    median_income    = as.numeric(v063_rawvalue),
    pct_rural        = as.numeric(v058_rawvalue),
    long_commute     = as.numeric(v137_rawvalue),
    broadband        = as.numeric(v166_rawvalue)
  ) %>%
  filter(nchar(fips) == 5, fips != "00000", str_sub(fips, 3, 5) != "000")

for (v in setdiff(names(chr2), "fips")) {
  message(sprintf("  CHR %-14s present: %d / %d", v, sum(!is.na(chr2[[v]])), nrow(chr2)))
}

# ---- 3. Medicaid expansion status as of 2021 ------------------------------
# Source: KFF, Status of State Medicaid Expansion Decisions.
non_expansion_2021 <- c("AL","FL","GA","KS","MS","NC","SC","SD","TN","TX","WI","WY")
mid_year_2021      <- c("MO","OK")   # implemented July and October 2021

state_lookup <- read_csv("data_derived/county_centroids.csv", show_col_types = FALSE,
                         col_types = cols(fips = "c", .default = col_guess())) %>%
  select(fips, state)

st_abb <- setNames(state.abb, state.name)
expansion <- state_lookup %>%
  mutate(state_abb = ifelse(state == "District of Columbia", "DC", st_abb[state]),
         medicaid_expansion = case_when(
           state_abb %in% non_expansion_2021 ~ "no",
           state_abb %in% mid_year_2021      ~ "midyear2021",
           is.na(state_abb)                  ~ NA_character_,
           TRUE                              ~ "yes"))

message("\nMedicaid expansion, counties by status:")
print(count(expansion, medicaid_expansion))

# ---- merge ----------------------------------------------------------------
ext <- state_lookup %>%
  left_join(cms2, by = "fips") %>%
  left_join(chr2, by = "fips") %>%
  left_join(expansion %>% select(fips, state_abb, medicaid_expansion), by = "fips")

message("\nExternal file rows: ", nrow(ext))
write_csv(ext, "data_derived/external_county_2021.csv")
message("Wrote data_derived/external_county_2021.csv")
