# =============================================================================
# WOST MOVING WINDOW SCALE ANALYSIS - ALL YEARS, SCALES 10-5000 (step 10)
# moving_window_scale.R
# =============================================================================

library(terra)
library(fastfocal)
library(RNetCDF)
library(sf)
library(dplyr)
library(readr)

# =============================================================================
# 1. HELPER: Read NetCDF time values
# =============================================================================
get_nc_times_fixed <- function(nc_file) {
  nc <- RNetCDF::open.nc(nc_file)
  on.exit(RNetCDF::close.nc(nc))
  time_vals  <- RNetCDF::var.get.nc(nc, "time")
  time_units <- RNetCDF::att.get.nc(nc, "time", "units")
  datetime_str <- sub("days since ", "", time_units)
  datetime_str <- trimws(gsub("Z$|\\+0000$", "", trimws(datetime_str)))
  epoch <- as.POSIXct(datetime_str, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  epoch + as.difftime(time_vals, units = "days")
}

# =============================================================================
# 2. CONFIGURATION
# =============================================================================

eden_path     <- "data/WaterData"
colonies_path <- "data/shapefiles/colonies.geojson"
out_dir       <- "data/moving_window_outputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# All 500 scales from 10 to 5000 in steps of 10
scales <- seq(10, 5000, by = 10)

# Years and quarters to loop over
target_years    <- 1993:2026
target_quarters <- c("q1", "q2", "q3", "q4")

# Scales - ALSO save full raster outputs (to keep storage manageable)
# These are a representative subset. Add or remove as needed.
save_raster_scales <- c(90, 180, 360, 720, 1440, 2500, 5000)

# Skip 2026 Q3 and Q4 (data not available)
skip_combos <- list(c(2026, "q3"), c(2026, "q4"))

cat("Total scales:", length(scales), "\n")
cat("Total years:", length(target_years), "\n")
cat("Estimated total fastfocal operations:",
    length(scales) * length(target_years) * length(target_quarters), "\n")
cat("This will take a long time. Grab a coffee!\n\n")

# =============================================================================
# 3. LOAD AND ALIGN WOST COLONIES (done once, outside the loop)
# =============================================================================

cat("Loading WOST colony locations...\n")
colonies_sf   <- sf::st_read(colonies_path, quiet = TRUE)
colonies_vect <- terra::vect(colonies_sf)

# Master results list to collect all extracted values across all years
all_results <- list()

# =============================================================================
# 4. OUTER LOOP: YEARS AND QUARTERS
# =============================================================================

for (yr in target_years) {
  for (qt in target_quarters) {
    
    # Skip unavailable quarters
    is_skip <- any(sapply(skip_combos, function(x) x[1] == yr && x[2] == qt))
    if (is_skip) {
      cat("Skipping", yr, qt, "(data not available)\n")
      next
    }
    
    nc_filename <- paste0(yr, "_", qt, "_depth.nc")
    nc_path     <- file.path(gsub("\\\\", "/", normalizePath(eden_path)), nc_filename)
    
    # Skip if the file does not exist on disk
    if (!file.exists(nc_path)) {
      cat("  File not found, skipping:", nc_filename, "\n")
      next
    }
    
    cat("\n--- Processing:", yr, qt, "---\n")
    
    # =========================================================================
    # 5. LOAD AND PREPARE RASTER FOR THIS YEAR/QUARTER
    # =========================================================================
    
    r_raw <- terra::rast(nc_path)
    
    # Collapse all daily layers into a single mean depth raster
    r <- terra::mean(r_raw, na.rm = TRUE)
    names(r) <- paste0("mean_depth_cm_", yr, "_", qt)
    
    # On the very first iteration, check CRS and project colonies to match
    if (yr == target_years[1] && qt == target_quarters[1]) {
      cat("Raster CRS:\n")
      print(crs(r, proj = TRUE))
      cat("Raster resolution:", res(r), "\n")
      
  
      cat("Projecting raster to UTM Zone 17N (meters) for Florida...\n")
      r <- terra::project(r, "EPSG:32617")
      
      
      colonies_proj <- terra::project(colonies_vect, crs(r))
      cat("Colonies projected to match raster CRS.\n")
    }
    
    # =========================================================================
    # 6. MOVING WINDOW ANALYSIS ACROSS ALL 500 SCALES [4]
    # =========================================================================
    
    cat("  Running fastfocal across", length(scales), "scales...\n")
    
    # Use lapply() to apply all scales efficiently [4]
    fs.list <- lapply(scales, function(s) {
      fs <- fastfocal(
        r,
        d = s,
        w = "circle",
        fun = "mean",
        na.rm = TRUE
      )
      names(fs) <- paste0("scale_", s, "m")
      
      # Only save rasters for the key representative scales
      if (s %in% save_raster_scales) {
        out_file <- file.path(
          out_dir,
          paste0("WOST_depth_", s, "m_", yr, "_", qt, ".tif")
        )
        terra::writeRaster(fs, out_file, overwrite = TRUE)
      }
      
      return(fs)
    })
    
    names(fs.list) <- paste0("scale_", scales, "m")
    
    # =========================================================================
    # 7. EXTRACT VALUES AT WOST COLONY LOCATIONS
    # =========================================================================
    
    cat("  Extracting colony values...\n")
    
    fs_stack      <- terra::rast(fs.list)
    colony_values <- terra::extract(fs_stack, colonies_proj, bind = FALSE)
    colony_df     <- as.data.frame(colony_values) |>
      dplyr::mutate(year = yr, quarter = qt)
    
    all_results[[paste0(yr, "_", qt)]] <- colony_df
    
    # =========================================================================
    # 8. SAVE REPRESENTATIVE JPEG FOR THIS YEAR/QUARTER
    # =========================================================================
    
    # Plots only the 7 key representative scales to keep file count manageable
    jpeg(
      file.path(out_dir, paste0("moving_window_key_scales_", yr, "_", qt, ".jpg")),
      width  = 4900,
      height = 1400,
      res    = 200
    )
    par(mfrow = c(1, length(save_raster_scales)))
    
    for (s in save_raster_scales) {
      plot(
        fs.list[[paste0("scale_", s, "m")]],
        main = paste0(s, "m (", yr, " ", qt, ")")
      )
      plot(colonies_proj, add = TRUE, col = "red", pch = 16, cex = 0.3)
    }
    
    dev.off()
    cat("  JPEG saved for", yr, qt, "\n")
    
    # Clear rasters from memory after each year/quarter to avoid RAM issues
    rm(r_raw, r, fs.list, fs_stack, colony_values)
    gc()
  }
}

# =============================================================================
# 9. SAVE CONSOLIDATED RESULTS TO CSV
# =============================================================================

cat("\nBinding all results and saving master CSV...\n")

master_df <- dplyr::bind_rows(all_results)

write_csv(
  master_df,
  file.path(out_dir, "colony_depth_all_years_all_scales.csv")
)


cat("MOVING WINDOW ANALYSIS COMPLETE\n")
cat("Master CSV saved to: data/moving_window_outputs/colony_depth_all_years_all_scales.csv\n")
cat("Key rasters and JPEGs saved to:", out_dir, "\n")