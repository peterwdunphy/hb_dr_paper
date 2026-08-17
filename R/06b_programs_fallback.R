# 06b_programs_fallback.R -------------------------------------------------
# Second-pass geocoding for programs the Census geocoder could not match.
#
# The Census geocoder matches against address ranges and fails on large
# institutional campuses ("885 Tiverton Drive", "9500 Gilman Drive"). OSM's
# Nominatim resolves these because the institutions themselves are named
# features. We query institution name + city/state, then assign county by
# point-in-polygon against the TIGER 2021 county layer rather than trusting
# any geocoder's own county field.
#
# Nominatim usage policy: 1 request/second, identifying User-Agent required.
# -------------------------------------------------------------------------

library(jsonlite)
library(dplyr)
library(readr)
library(stringr)
library(sf)

progs <- read_csv("data_derived/programs_geocoded.csv", show_col_types = FALSE)

nominatim <- function(q) {
  url <- paste0("https://nominatim.openstreetmap.org/search?format=json&limit=1&q=",
                utils::URLencode(q, reserved = TRUE))
  con <- url(url, headers = c(`User-Agent` = "hb-dr-paper/0.1 (academic research; peterwdunphy@gmail.com)"))
  res <- try(jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = "")), silent = TRUE)
  try(close(con), silent = TRUE)
  if (inherits(res, "try-error") || length(res) == 0 || nrow(res) == 0)
    return(c(NA_real_, NA_real_))
  c(as.numeric(res$lon[1]), as.numeric(res$lat[1]))
}

need <- which(is.na(progs$lat))
message("Retrying ", length(need), " programs via Nominatim...")

for (i in need) {
  # institution name plus city/state is more reliable than a campus street
  q <- if (!is.na(progs$city[i])) {
    paste0(progs$name[i], ", ", progs$city[i], ", ", progs$state[i], ", USA")
  } else {
    paste0(progs$name[i], ", ", progs$query[i], ", USA")
  }
  xy <- nominatim(q)
  if (is.na(xy[1])) {                       # retry on the raw address string
    xy <- nominatim(paste0(progs$query[i], ", USA"))
  }
  progs$lon[i] <- xy[1]; progs$lat[i] <- xy[2]
  message(sprintf("  [%3d] %-55s -> %s",
                  progs$prog_id[i], substr(progs$name[i], 1, 55),
                  ifelse(is.na(xy[1]), "FAIL", paste0(round(xy[2],4), ", ", round(xy[1],4)))))
  Sys.sleep(1.1)
}

still <- sum(is.na(progs$lat))
message(sprintf("Remaining unmatched: %d", still))

# --- assign county by point-in-polygon for every program --------------------
options(tigris_use_cache = TRUE)
cty <- tigris::counties(year = 2021, cb = TRUE, progress_bar = FALSE) %>%
  st_transform(4326) %>%
  select(fips_pip = GEOID)

pts <- progs %>%
  filter(!is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

joined <- st_join(pts, cty, join = st_within) %>%
  st_drop_geometry() %>%
  select(prog_id, fips_pip)

progs <- progs %>%
  left_join(joined, by = "prog_id") %>%
  mutate(county_fips = coalesce(fips_pip, fips))

message("Programs with a county assigned: ", sum(!is.na(progs$county_fips)), "/", nrow(progs))
message("Distinct counties containing >=1 program: ", dplyr::n_distinct(progs$county_fips, na.rm = TRUE))

# disagreement between geocoder county and point-in-polygon county
disagree <- progs %>% filter(!is.na(fips), !is.na(fips_pip), fips != fips_pip)
if (nrow(disagree) > 0) {
  message("NOTE: geocoder/PIP county disagreement on ", nrow(disagree), " program(s):")
  print(disagree %>% select(prog_id, name, fips, fips_pip))
}

write_csv(progs, "data_derived/programs_geocoded.csv")
message("Updated data_derived/programs_geocoded.csv")
