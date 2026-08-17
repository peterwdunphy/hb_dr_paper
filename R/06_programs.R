# 06_programs.R -----------------------------------------------------------
# Geocode ACGME-accredited ophthalmology residency programs and assign each
# to a county (FIPS).
#
# Input : data/ophtho_programs_raw.csv  (128 programs, exported from the
#         Ophtho_Programs_Reference sheet of the received workbook; original
#         source FRIEDA/ACGME)
# Output: data_derived/programs_geocoded.csv
#
# Notes:
#  * 23 of 128 rows have the full address collapsed into the Address field with
#    City/State/Zipcode blank. Rather than parse street-vs-city (genuinely
#    ambiguous without a comma, e.g. "2200 Bergquist Dr JBSA Lackland AFB"),
#    we hand the whole string to the Census one-line geocoder.
#  * We use the `geographies` endpoint so the geocoder returns the 2020 county
#    FIPS directly. That is what defines has_program, replacing the workbook's
#    precomputed flag.
#  * Accreditation-year filtering to <= 2021 happens in 08_build_analytic.R,
#    not here, so the full roster stays available for sensitivity analyses.
# -------------------------------------------------------------------------

library(jsonlite)
library(dplyr)
library(readr)
library(stringr)

dir.create("data_derived", showWarnings = FALSE)

prog <- read_csv("data/ophtho_programs_raw.csv", show_col_types = FALSE) %>%
  rename(name = Name, address = Address, city = City, state = State,
         zip = Zipcode, accred_year = `Date of ACGME Accedidation`) %>%
  mutate(
    zip = str_pad(str_extract(as.character(zip), "^\\d+"), 5, "left", "0"),
    # well-formed rows get assembled; collapsed rows already carry everything
    query = if_else(
      is.na(city),
      str_squish(address),
      str_squish(paste0(address, ", ", city, ", ", state, " ", zip))
    ),
    prog_id = row_number()
  )

geocode_one <- function(q) {
  url <- paste0(
    "https://geocoding.geo.census.gov/geocoder/geographies/onelineaddress",
    "?address=", utils::URLencode(q, reserved = TRUE),
    "&benchmark=Public_AR_Census2020&vintage=Census2020_Census2020",
    "&layers=Counties&format=json"
  )
  res <- try(jsonlite::fromJSON(url, flatten = TRUE), silent = TRUE)
  if (inherits(res, "try-error")) return(tibble(lon = NA_real_, lat = NA_real_,
                                                fips = NA_character_,
                                                matched = NA_character_))
  m <- res$result$addressMatches
  if (is.null(m) || length(m) == 0 || nrow(m) == 0) {
    return(tibble(lon = NA_real_, lat = NA_real_, fips = NA_character_,
                  matched = NA_character_))
  }
  cty <- m$geographies.Counties[[1]]
  tibble(
    lon = m$coordinates.x[1],
    lat = m$coordinates.y[1],
    fips = if (!is.null(cty) && nrow(cty) > 0)
      paste0(cty$STATE[1], cty$COUNTY[1]) else NA_character_,
    matched = m$matchedAddress[1]
  )
}

message("Geocoding ", nrow(prog), " programs via Census geocoder...")
geo <- vector("list", nrow(prog))
for (i in seq_len(nrow(prog))) {
  geo[[i]] <- geocode_one(prog$query[i])
  if (i %% 20 == 0) message("  ", i, "/", nrow(prog))
  Sys.sleep(0.15)
}

out <- bind_cols(prog, bind_rows(geo))

n_fail <- sum(is.na(out$lat))
message(sprintf("Geocoded %d/%d (%d failures)",
                nrow(out) - n_fail, nrow(out), n_fail))
if (n_fail > 0) {
  message("Failed rows (need manual coordinates):")
  print(out %>% filter(is.na(lat)) %>% select(prog_id, name, query))
}

write_csv(out, "data_derived/programs_geocoded.csv")
message("Wrote data_derived/programs_geocoded.csv")
