# 07_drivetime.R ----------------------------------------------------------
# Road-network drive times from population-weighted county centroids to
# ACGME-accredited ophthalmology residency programs, via OSRM.
#
# Replaces the Euclidean distance the reviewers flagged as "a simplification of
# true travel burden".
#
# Strategy: routing every county-program pair would be 3,221 x 123 = 396,183
# routes. Almost all are irrelevant (a county in Maine has no business routing
# to a program in Arizona). We prefilter on great-circle distance, which is a
# strict lower bound on road distance:
#   * keep any pair within 200 great-circle miles (covers a 150-minute
#     catchment with headroom, since sustained road speed rarely exceeds 70mph)
#   * plus the 5 nearest programs per county regardless of distance, so every
#     county still gets a nearest-program drive time even in the remote West
# That leaves ~24,500 pairs, about 307 OSRM requests.
#
# The public OSRM /table endpoint accepts at most 100 coordinates per request,
# so we send 1 program (source) against <=99 counties (destinations) at a time.
#
# Results are checkpointed after every request so the run can resume.
#
# KNOWN LIMITATION (for Methods): OSRM car routing ignores traffic, ferries are
# unreliable, and public transit is not modeled at all. Island counties
# (Hawaii, some Alaska boroughs, Puerto Rico) will return no route; they are
# recorded as NA and handled explicitly in 08_build_analytic.R rather than
# silently dropped.
# -------------------------------------------------------------------------

library(dplyr)
library(readr)
library(jsonlite)

OSRM_HOST  <- "https://router.project-osrm.org"
CHUNK      <- 99          # OSRM public cap is 100 coords per request
PREFILTER  <- 200         # great-circle miles
N_NEAREST  <- 5           # always route this many nearest programs
SLEEP      <- 0.6         # be polite to a shared community server
CKPT       <- "data_derived/drivetime_checkpoint.rds"

cen <- read_csv("data_derived/county_centroids.csv", show_col_types = FALSE)
pg  <- read_csv("data_derived/programs_geocoded.csv", show_col_types = FALSE) %>%
  mutate(yr = suppressWarnings(as.numeric(accred_year))) %>%
  filter(is.na(yr) | yr <= 2021)          # 2021 cross-section; MSU has yr "??"

message(sprintf("Counties: %d   Programs (accredited <=2021): %d",
                nrow(cen), nrow(pg)))

# --- great-circle prefilter -------------------------------------------------
hav <- function(lat1, lon1, lat2, lon2) {
  R <- 3958.8; p <- pi / 180
  a <- sin((lat2 - lat1) * p / 2)^2 +
    cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}

D <- outer(seq_len(nrow(cen)), seq_len(nrow(pg)),
           Vectorize(function(i, j) hav(cen$pw_lat[i], cen$pw_lon[i],
                                        pg$lat[j], pg$lon[j])))
keep <- D <= PREFILTER
for (i in seq_len(nrow(D))) keep[i, order(D[i, ])[seq_len(N_NEAREST)]] <- TRUE
message(sprintf("Pairs to route: %d (of %d possible)", sum(keep), length(D)))

# --- OSRM table query -------------------------------------------------------
# NOTE: coordinates MUST be rounded before being pasted into the URL. R prints
# full double precision (e.g. -86.64273899999999), which pushed a 99-destination
# request past the server's URI length limit; the server then returned an HTML
# error instead of JSON and the whole chunk came back NA. Rounding to 5 decimals
# is ~1 metre of precision and keeps the URL under 2,000 characters.
rc <- function(x) round(x, 5)

osrm_table <- function(src_lon, src_lat, dst_lon, dst_lat, tries = 3) {
  coords <- paste0(
    paste0(rc(src_lon), ",", rc(src_lat)), ";",
    paste(paste0(rc(dst_lon), ",", rc(dst_lat)), collapse = ";")
  )
  url <- paste0(OSRM_HOST, "/table/v1/driving/", coords,
                "?sources=0&annotations=duration")
  for (t in seq_len(tries)) {
    res <- try(jsonlite::fromJSON(url), silent = TRUE)
    if (!inherits(res, "try-error") && !is.null(res$code) && res$code == "Ok") {
      d <- as.numeric(res$durations[1, ])
      return(d[-1])                      # drop source-to-source (0)
    }
    Sys.sleep(2 * t)
  }
  rep(NA_real_, length(dst_lon))
}

results <- if (file.exists(CKPT)) readRDS(CKPT) else list()

# A chunk counts as done only if it actually returned durations. Chunks that
# came back entirely NA were the URI-length failures described above and are
# re-requested. Genuinely unroutable pairs (islands) survive as NA inside an
# otherwise-populated chunk, so they are not retried forever.
done <- names(results)[vapply(results, function(x) !all(is.na(x$drive_min)),
                              logical(1))]
message("Checkpoint chunks: ", length(results),
        " | usable: ", length(done),
        " | to re-request: ", length(results) - length(done))

req_n <- 0; t0 <- Sys.time()
for (j in seq_len(nrow(pg))) {
  idx <- which(keep[, j])
  if (!length(idx)) next
  chunks <- split(idx, ceiling(seq_along(idx) / CHUNK))
  for (k in seq_along(chunks)) {
    key <- paste0(pg$prog_id[j], "_", k)
    if (key %in% done) next
    ci <- chunks[[k]]
    dur <- osrm_table(pg$lon[j], pg$lat[j], cen$pw_lon[ci], cen$pw_lat[ci])
    results[[key]] <- tibble(
      fips = cen$fips[ci],
      prog_id = pg$prog_id[j],
      drive_min = dur / 60,
      gc_miles = D[ci, j]
    )
    req_n <- req_n + 1
    if (req_n %% 10 == 0) {
      saveRDS(results, CKPT)
      message(sprintf("  %d requests, %.1f min elapsed",
                      req_n, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    }
    Sys.sleep(SLEEP)
  }
}
saveRDS(results, CKPT)

pairs <- bind_rows(results)
message(sprintf("Routed pairs: %d   unroutable (NA): %d",
                nrow(pairs), sum(is.na(pairs$drive_min))))

write_csv(pairs, "data_derived/county_program_drivetimes.csv")

# --- nearest-program summary per county -------------------------------------
nearest <- pairs %>%
  filter(!is.na(drive_min)) %>%
  group_by(fips) %>%
  slice_min(drive_min, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(fips, nearest_prog_id = prog_id,
         nearest_drive_min = drive_min, nearest_gc_miles = gc_miles)

nearest <- cen %>% select(fips, county, state) %>% left_join(nearest, by = "fips")

message(sprintf("Counties with a drive time: %d / %d",
                sum(!is.na(nearest$nearest_drive_min)), nrow(nearest)))
message("Counties with NO routable program (island/remote):")
print(nearest %>% filter(is.na(nearest_drive_min)) %>% count(state), n = 30)

write_csv(nearest, "data_derived/county_nearest_program.csv")
message("Wrote data_derived/county_program_drivetimes.csv and county_nearest_program.csv")
