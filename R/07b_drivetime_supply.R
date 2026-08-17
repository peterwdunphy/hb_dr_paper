# 07b_drivetime_supply.R --------------------------------------------------
# Drive time from each county's population-weighted centroid to the nearest
# county containing at least one patient-care OPHTHALMOLOGIST of any kind
# (AHRF 2021), regardless of academic affiliation.
#
# WHY THIS EXISTS: it is the comparison exposure that makes the academic
# question answerable. Reviewer 2 asked directly: "It is not clear to me why a
# county with a high density of ophthalmologists but also distant from an
# academic medical center is not receiving good ophthalmologic care."
#
# With both exposures in one model, the coefficient on academic drive time is
# the effect of academic proximity HOLDING GENERAL EYE-CARE PROXIMITY FIXED.
# That is the quantity that isolates what is unique about academic centers,
# as opposed to the fact that academic centers sit where ophthalmologists are.
#
# EFFICIENCY: 3,221 sources x 1,202 supply counties is far too many routes to
# request one at a time. Instead we cluster source counties geographically
# (k-means on the population-weighted centroids), and for each cluster request
# a single OSRM table of ~30 sources x ~45 candidate destinations. Because
# cluster members are spatially close, the 55 supply counties nearest the
# cluster centroid reliably contain each member's true nearest. The script
# verifies that assumption explicitly at the end rather than assuming it.
# -------------------------------------------------------------------------

library(dplyr)
library(readr)
library(jsonlite)

OSRM_HOST <- "https://router.project-osrm.org"
# URL LENGTH is the binding constraint, not OSRM's 100-coordinate cap. Each
# coordinate costs ~19 characters, and an explicit `destinations=` index list
# costs another ~250. A 45+55 request overran the server's URI limit, which is
# returned as an HTML error page (not JSON) and silently became a chunk of NAs.
# We therefore (a) omit `destinations` entirely, letting it default to all
# points and slicing the columns we want, and (b) keep the total well under 100.
N_SRC     <- 30     # sources per request
N_DST     <- 45     # candidate destinations per request (75 coords total)
SLEEP     <- 0.6
CKPT      <- "data_derived/drivetime_supply_checkpoint.rds"

cen  <- read_csv("data_derived/county_centroids.csv", show_col_types = FALSE,
                 col_types = cols(fips = "c", .default = col_guess()))
ahrf <- read_csv("data_derived/ahrf_2021_county.csv", show_col_types = FALSE,
                 col_types = cols(fips = "c", .default = col_guess()))

sup_fips <- ahrf %>% filter(ophth_patient_care > 0) %>% pull(fips)
sup <- cen %>% filter(fips %in% sup_fips)
message(sprintf("Sources: %d counties | Supply counties (>=1 ophthalmologist): %d",
                nrow(cen), nrow(sup)))

hav <- function(lat1, lon1, lat2, lon2) {
  R <- 3958.8; p <- pi / 180
  a <- sin((lat2 - lat1) * p / 2)^2 +
    cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}

# --- geographic clustering of sources ---------------------------------------
set.seed(1)
k <- ceiling(nrow(cen) / N_SRC)
km <- kmeans(cbind(cen$pw_lon, cen$pw_lat), centers = k, iter.max = 50, nstart = 5)
cen$cluster <- km$cluster
message("Clusters: ", k, " (max size ", max(table(cen$cluster)), ")")

osrm_table <- function(src, dst, tries = 3) {
  all_pts <- rbind(src[, c("pw_lon", "pw_lat")], dst[, c("pw_lon", "pw_lat")])
  all_pts$pw_lon <- round(all_pts$pw_lon, 5); all_pts$pw_lat <- round(all_pts$pw_lat, 5)
  coords <- paste(paste0(all_pts$pw_lon, ",", all_pts$pw_lat), collapse = ";")
  si <- paste(seq_len(nrow(src)) - 1, collapse = ";")
  # no `destinations` param: it defaults to all points, and we slice below
  url <- paste0(OSRM_HOST, "/table/v1/driving/", coords,
                "?sources=", si, "&annotations=duration")
  if (nchar(url) > 3000) warning("URL length ", nchar(url), " may be rejected")
  for (t in seq_len(tries)) {
    res <- try(jsonlite::fromJSON(url), silent = TRUE)
    if (!inherits(res, "try-error") && !is.null(res$code) && res$code == "Ok") {
      M <- matrix(as.numeric(res$durations), nrow = nrow(src), byrow = FALSE)
      # keep only the destination columns
      return(M[, nrow(src) + seq_len(nrow(dst)), drop = FALSE])
    }
    Sys.sleep(2 * t)
  }
  matrix(NA_real_, nrow = nrow(src), ncol = nrow(dst))
}

results <- if (file.exists(CKPT)) readRDS(CKPT) else list()
t0 <- Sys.time()

for (cl in sort(unique(cen$cluster))) {
  key <- as.character(cl)
  if (!is.null(results[[key]]) && !all(is.na(results[[key]]$supply_drive_min))) next
  src <- cen %>% filter(cluster == cl)

  # candidate destinations: nearest supply counties to the cluster centroid
  clon <- mean(src$pw_lon); clat <- mean(src$pw_lat)
  dcen <- hav(clat, clon, sup$pw_lat, sup$pw_lon)
  dst  <- sup[order(dcen)[seq_len(min(N_DST, nrow(sup)))], ]

  # split oversized clusters so each request stays at N_SRC sources.
  # (Deriving the chunk size from the coordinate cap rather than N_SRC would
  # let a large cluster rebuild the over-length URL that broke the first run.)
  chunks <- split(seq_len(nrow(src)), ceiling(seq_len(nrow(src)) / N_SRC))
  out <- list()
  for (ch in chunks) {
    M <- osrm_table(src[ch, ], dst)
    # nearest supply county, by drive time, among candidates
    best <- apply(M, 1, function(r) if (all(is.na(r))) NA_real_ else min(r, na.rm = TRUE))
    bidx <- apply(M, 1, function(r) if (all(is.na(r))) NA_integer_ else which.min(r))
    out[[length(out) + 1]] <- tibble(
      fips = src$fips[ch],
      supply_drive_min = best / 60,
      supply_fips = dst$fips[bidx],
      # great-circle distance to the true nearest supply county, for validation
      gc_true_min = vapply(ch, function(i)
        min(hav(src$pw_lat[i], src$pw_lon[i], sup$pw_lat, sup$pw_lon)), numeric(1)),
      gc_cand_min = vapply(ch, function(i)
        min(hav(src$pw_lat[i], src$pw_lon[i], dst$pw_lat, dst$pw_lon)), numeric(1))
    )
    Sys.sleep(SLEEP)
  }
  results[[key]] <- bind_rows(out)
  if (cl %% 10 == 0) {
    saveRDS(results, CKPT)
    message(sprintf("  cluster %d/%d, %.1f min elapsed", cl, k,
                    as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  }
}
saveRDS(results, CKPT)

sup_dt <- bind_rows(results)

# counties that themselves have an ophthalmologist have zero travel burden
sup_dt <- sup_dt %>%
  mutate(supply_drive_min = if_else(fips %in% sup_fips, 0, supply_drive_min))

# --- validate the candidate-set assumption ----------------------------------
bad <- sup_dt %>% filter(gc_cand_min > gc_true_min + 1e-6)
message(sprintf("\nCandidate-set check: %d of %d counties whose true nearest supply county",
                nrow(bad), nrow(sup_dt)))
message("  was outside the candidate set (should be 0 or very few):")
if (nrow(bad) > 0) print(summary(bad$gc_cand_min - bad$gc_true_min))

message(sprintf("Counties with a supply drive time: %d / %d",
                sum(!is.na(sup_dt$supply_drive_min)), nrow(sup_dt)))
print(summary(sup_dt$supply_drive_min))

write_csv(sup_dt, "data_derived/county_nearest_ophthalmologist.csv")
message("Wrote data_derived/county_nearest_ophthalmologist.csv")
