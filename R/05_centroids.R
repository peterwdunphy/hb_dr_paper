# 05_centroids.R ----------------------------------------------------------
# Population-weighted county centroids.
#
# Source: US Census Bureau, Centers of Population, 2020 Census
#   https://www2.census.gov/geo/docs/reference/cenpop2020/county/CenPop2020_Mean_CO.txt
#
# This is the tract-population-weighted calculation described in the revision
# plan, already computed and published by Census. Using the official file is
# both less work and more citable than deriving it ourselves, and it is the
# standard input in health-services geospatial work.
#
# Also pulls TIGER county geometry for land area (population density, which
# answers Reviewer 2's "small county vs county 5x the area" objection) and for
# the neighbor structure the spatial model needs later.
# -------------------------------------------------------------------------

library(dplyr)
library(readr)
library(sf)

dir.create("data", showWarnings = FALSE)
dir.create("data_derived", showWarnings = FALSE)

URL <- "https://www2.census.gov/geo/docs/reference/cenpop2020/county/CenPop2020_Mean_CO.txt"
raw_path <- "data/CenPop2020_Mean_CO.txt"
if (!file.exists(raw_path)) download.file(URL, raw_path, quiet = TRUE)

# Latin-1: some county names carry accented characters (e.g. Doña Ana, NM)
cen <- read_csv(raw_path, show_col_types = FALSE,
                locale = locale(encoding = "latin1")) %>%
  transmute(
    fips     = paste0(sprintf("%02d", as.integer(STATEFP)),
                      sprintf("%03d", as.integer(COUNTYFP))),
    county   = COUNAME,
    state    = STNAME,
    pop2020  = POPULATION,
    pw_lat   = LATITUDE,
    pw_lon   = LONGITUDE
  )

message("Population-weighted centroids: ", nrow(cen), " counties")

# --- land area + geometry ---------------------------------------------------
options(tigris_use_cache = TRUE)
cty <- tigris::counties(year = 2021, cb = TRUE, progress_bar = FALSE)

area <- cty %>%
  st_drop_geometry() %>%
  transmute(fips = GEOID,
            land_area_sqmi = ALAND / 2589988.11)   # m^2 -> square miles

out <- cen %>% left_join(area, by = "fips")

message("Missing land area: ", sum(is.na(out$land_area_sqmi)))

write_csv(out, "data_derived/county_centroids.csv")
st_write(cty, "data_derived/counties_2021.gpkg", delete_dsn = TRUE, quiet = TRUE)
message("Wrote data_derived/county_centroids.csv and counties_2021.gpkg")
