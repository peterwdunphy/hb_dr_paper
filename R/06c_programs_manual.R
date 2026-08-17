# 06c_programs_manual.R ---------------------------------------------------
# Manual coordinate overrides for the 7 programs that neither the Census
# geocoder (06) nor the first-pass Nominatim query (06b) could resolve.
#
# Each coordinate below was obtained from Nominatim using an institution- or
# facility-specific query rather than the campus mailing address, and is
# recorded here so the pipeline stays fully reproducible. The `source_query`
# column documents exactly what was looked up.
#
# All 7 are large, unambiguous academic medical centers; the failures were
# geocoder address-range limitations, not genuine ambiguity about location.
# -------------------------------------------------------------------------

library(dplyr)
library(readr)
library(sf)

manual <- tribble(
  ~prog_id, ~lat,      ~lon,        ~source_query,
  9L,       37.43269,  -122.17554,  "300 Pasteur Drive, Stanford, CA 94305",
  11L,      37.76307,  -122.45740,  "505 Parnassus Avenue, San Francisco, CA 94143",
  51L,      36.00702,   -78.93735,  "Duke University Hospital, Durham, North Carolina",
  54L,      41.25508,   -95.97695,  "University of Nebraska Medical Center, Omaha, Nebraska",
  57L,      35.08738,  -106.61757,  "2211 Lomas Blvd NE, Albuquerque, NM 87106",
  68L,      40.90827,   -73.11519,  "Stony Brook University Hospital, Stony Brook, New York",
  81L,      18.39837,   -66.07453,  "Recinto de Ciencias Medicas, San Juan, Puerto Rico"
)

progs <- read_csv("data_derived/programs_geocoded.csv", show_col_types = FALSE)

progs <- progs %>%
  left_join(manual, by = "prog_id", suffix = c("", ".man")) %>%
  mutate(
    # `fips` is non-missing only for rows the Census geocoder matched in 06;
    # everything else with coordinates came from the Nominatim pass in 06b.
    geocode_source = case_when(
      !is.na(lat.man) ~ "manual (Nominatim, facility query)",
      !is.na(fips)    ~ "census geocoder",
      !is.na(lat)     ~ "nominatim (name + city)",
      TRUE            ~ NA_character_
    ),
    lat = coalesce(lat.man, lat),
    lon = coalesce(lon.man, lon)
  ) %>%
  select(-lat.man, -lon.man)

message("Unmatched after manual pass: ", sum(is.na(progs$lat)))

# --- re-assign county by point-in-polygon -----------------------------------
options(tigris_use_cache = TRUE)
cty <- tigris::counties(year = 2021, cb = TRUE, progress_bar = FALSE) %>%
  st_transform(4326) %>% select(fips_pip2 = GEOID)

pts <- progs %>% filter(!is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

joined <- st_join(pts, cty, join = st_within) %>% st_drop_geometry() %>%
  select(prog_id, fips_pip2)

progs <- progs %>%
  left_join(joined, by = "prog_id") %>%
  mutate(county_fips = coalesce(fips_pip2, county_fips)) %>%
  select(-fips_pip2)

message("Programs with a county: ", sum(!is.na(progs$county_fips)), "/", nrow(progs))
message("Distinct program counties: ", dplyr::n_distinct(progs$county_fips, na.rm = TRUE))
message("Programs accredited <= 2021: ", sum(progs$accred_year <= 2021, na.rm = TRUE))

write_csv(progs, "data_derived/programs_geocoded.csv")
message("Updated data_derived/programs_geocoded.csv")
