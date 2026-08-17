# 06d_programs_fix.R ------------------------------------------------------
# Targeted correction found by QA (see below), applied after 06/06b/06c.
#
# QA procedure: for every geocoded program, compare the STATE of the county it
# landed in (via point-in-polygon) against the state listed in the FRIEDA
# roster. Exactly one program disagreed.
#
#   prog_id 97, University of Wisconsin School of Medicine and Public Health,
#   listed as Madison, WI. The Census geocoder matched the place name "Madison"
#   to MADISON COUNTY, ILLINOIS (17119) instead of Dane County, Wisconsin
#   (55025) -- roughly 350 miles off, and it would have removed Wisconsin's
#   flagship academic program from the exposure surface for the whole upper
#   Midwest.
#
# Corrected coordinates are UW Health University Hospital, 600 Highland Ave,
# Madison WI 53792, resolved via Nominatim.
#
# Cross-check that motivated the catch: AHRF recorded 14 ophthalmology
# residents in Dane County in 2021, but our roster showed no program there.
#
# Roster completeness was audited the same way. Eleven states have no program
# in the roster (AK, DE, HI, ID, ME, MT, NV, ND, SD, VT, WY); AHRF shows only
# 0-2 ophthalmology residents in any county of those states, consistent with
# Masterfile address noise rather than an unlisted residency. No roster gaps.
# -------------------------------------------------------------------------

library(dplyr)
library(readr)
library(sf)

progs <- read_csv("data_derived/programs_geocoded.csv", show_col_types = FALSE)

FIX_ID <- 97
stopifnot(any(progs$prog_id == FIX_ID))

message("Before: prog_id ", FIX_ID, " at county ",
        progs$county_fips[progs$prog_id == FIX_ID])

progs <- progs %>%
  mutate(
    lat = if_else(prog_id == FIX_ID, 43.07638, lat),
    lon = if_else(prog_id == FIX_ID, -89.43200, lon),
    geocode_source = if_else(prog_id == FIX_ID,
                             "manual (QA fix: geocoder matched Madison, IL)",
                             geocode_source)
  )

options(tigris_use_cache = TRUE)
cty <- tigris::counties(year = 2021, cb = TRUE, progress_bar = FALSE) %>%
  st_transform(4326) %>% select(fips_new = GEOID)

pts <- progs %>% filter(!is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

progs <- progs %>%
  left_join(st_join(pts, cty, join = st_within) %>% st_drop_geometry() %>%
              select(prog_id, fips_new),
            by = "prog_id") %>%
  mutate(county_fips = coalesce(fips_new, county_fips)) %>%
  select(-fips_new)

message("After:  prog_id ", FIX_ID, " at county ",
        progs$county_fips[progs$prog_id == FIX_ID], " (expect 55025, Dane WI)")
message("Distinct program counties (all): ",
        dplyr::n_distinct(progs$county_fips, na.rm = TRUE))
message("Distinct program counties (accredited <=2021): ",
        progs %>% mutate(yr = suppressWarnings(as.numeric(accred_year))) %>%
          filter(is.na(yr) | yr <= 2021) %>% pull(county_fips) %>%
          dplyr::n_distinct(na.rm = TRUE))

write_csv(progs, "data_derived/programs_geocoded.csv")
message("Updated data_derived/programs_geocoded.csv")
