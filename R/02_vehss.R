# 02_vehss.R --------------------------------------------------------------
# CDC Vision & Eye Health Surveillance System (VEHSS) Composite Prevalence
# Estimates, county level.
#
# Source: https://data.cdc.gov/resource/qeru-k2y2.json  (Socrata, no key needed)
# Citation: Lundeen EA, Burke-Conte Z, Rein DB, et al. Prevalence of diabetic
#   retinopathy in the US in 2021. JAMA Ophthalmol. 2023;141(8):747-754.
#
# NOTE ON VINTAGES: each VEHSS indicator is published for exactly one year, and
# the years differ by indicator. Diabetic retinopathy is 2021; glaucoma is 2022.
# This is a source constraint, not a choice, and must be disclosed in Methods.
#
# PRIMARY (diabetic retinopathy, 2021):
#   riskfactorresponse == "Yes"  -> denominator is people WITH DIABETES.
#     `sample_size` = modeled diabetes population (the transparent denominator
#     Reviewer 2 asked for); `numerator` = modeled case count.
#   response in {All DR stages, Vision threatening stage}
#     -> VTDR / all-DR is the new primary outcome (severity case mix).
#   all age strata retained so rates can be directly age-standardized instead
#     of gated on an arbitrary cutoff (Reviewer 1, comments 6 and 7).
#
# PARALLEL-CONSTRUCTION CONTROL (glaucoma, 2022):
#   Glaucoma carries the same severity split ("Vision affecting glaucoma" vs
#   "All Glaucoma"), so the identical ratio can be built on a different disease.
#   If access predicts the DR severity share but not the glaucoma severity
#   share, the result is DR-specific rather than an artifact of the method.
#   Caveat for Methods: glaucoma care is itself partly access-sensitive, so this
#   is a parallel-construction check, not a pure negative control.
#
# Uncorrected refractive error was considered and REJECTED as a control: it is
# published only for 2024 and its risk-factor strata are poverty and education,
# i.e. the very SES gradient we are trying to hold apart from access.
# -------------------------------------------------------------------------

library(jsonlite)
library(dplyr)
library(readr)

out_dir <- "data/vehss"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

BASE <- "https://data.cdc.gov/resource/qeru-k2y2.json"

# Socrata caps a page at 50k rows; page until a short page comes back.
# format(scientific = FALSE) matters: R will otherwise send offset=1e+05.
fetch_vehss <- function(where, page_size = 50000, max_pages = 40) {
  pages <- list()
  for (i in seq_len(max_pages)) {
    off <- format((i - 1) * page_size, scientific = FALSE)
    url <- paste0(
      BASE, "?$where=", utils::URLencode(where, reserved = TRUE),
      "&$limit=", page_size, "&$offset=", off,
      "&$order=locationid,age,response"
    )
    pg <- jsonlite::fromJSON(url, flatten = TRUE)
    if (length(pg) == 0 || nrow(pg) == 0) break
    pages[[i]] <- pg
    message(sprintf("  page %d, %d rows", i, nrow(pg)))
    if (nrow(pg) < page_size) break
  }
  bind_rows(pages)
}

common <- "geographiclevel='County' AND sex='Both sexes' AND raceethnicity='All races'"

specs <- list(
  dr = paste0(
    common, " AND yearstart='2021'",
    " AND question='Prevalence of Diabetic Retinopathy'",
    " AND riskfactorresponse='Yes'",
    " AND response IN ('All DR stages','Vision threatening stage')"
  ),
  glaucoma = paste0(
    common, " AND yearstart='2022'",
    " AND question='Prevalence of Glaucoma'",
    " AND riskfactorresponse='Total'",
    " AND response IN ('All Glaucoma','Vision affecting glaucoma')"
  )
)

for (nm in names(specs)) {
  message("Fetching: ", nm)
  raw <- fetch_vehss(specs[[nm]])
  message(sprintf("  -> %d rows, %d counties, ages: %s",
                  nrow(raw), dplyr::n_distinct(raw$locationid),
                  paste(sort(unique(raw$age)), collapse = " | ")))
  write_csv(raw, file.path(out_dir, paste0("vehss_", nm, "_county_raw.csv")))
}

message("Done. Raw VEHSS pulls written to ", out_dir)
