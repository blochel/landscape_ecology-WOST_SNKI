# =============================================================================
# DATA SCRAPING AND SAVING SCRIPT
# =============================================================================

# Load required packages
# Install sf with: install.packages("sf") if you don't have it yet
library(edenR)
library(wader)
library(dplyr)
library(readr)
library(tidyr)
library(sf)


# =============================================================================
# 1. Download daily EDEN data
# =============================================================================
download_eden_data <- function(eden_path = "data/WaterData") {
  cat("📥 Downloading fresh EDEN water data...\n")
  
  # Ensure the directory exists
  dir.create(eden_path, recursive = TRUE, showWarnings = FALSE)
  
  # Update and fetch water data based on your pipeline's logic
  update_water(eden_path)
  water <- get_eden_covariates(eden_path = eden_path, level = "subregions") |>
    bind_rows(get_eden_covariates(eden_path = eden_path, level = "all")) |>
    bind_rows(get_eden_covariates(eden_path = eden_path, level = "wcas")) |>
    dplyr::select(year, region = Name, variable, value) |>
    as.data.frame() |>
    dplyr::select(-geometry) |>
    pivot_wider(names_from = "variable", values_from = "value") |>
    mutate(year = as.integer(year)) |>
    arrange(year, region)
  
  cat("✓ EDEN data successfully compiled.\n")
  return(water)
}

# =============================================================================
# 2. Download shapefiles of nesting location for WOST
# =============================================================================
download_wost_shapefiles <- function(save_dir = "data/shapefiles") {
  cat("📥 Downloading WOST nesting shapefiles...\n")
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  
  # TODO: Replace this URL with the actual link to your shapefile zip archive
  shapefile_url <- "https://example.com/path/to/wost_shapefiles.zip"
  dest_file <- file.path(save_dir, "wost_shapefiles.zip")
  
  tryCatch({
    download.file(shapefile_url, destfile = dest_file, mode = "wb", quiet = TRUE)
    unzip(dest_file, exdir = save_dir)
    
    # Read the shapefile into an sf object (update 'wost_nests.shp' to the actual filename inside the zip)
    # wost_sf <- sf::st_read(file.path(save_dir, "wost_nests.shp"), quiet = TRUE)
    
    cat("✓ Shapefiles downloaded and extracted to:", save_dir, "\n")
  }, error = function(e) {
    cat("⚠ Could not download shapefiles. Please check the URL.\n")
  })
}

# =============================================================================
# 3. Download nest numbers for WOST
# =============================================================================
download_wost_nest_numbers <- function(save_dir = "data/wader_data") {
  cat("📥 Downloading wader observation data for WOST nest numbers...\n")
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Uses your existing pipeline's method to fetch observation data
  download_observations(save_dir)
  
  # Extract max counts and filter strictly for Wood Stork (wost)
  wost_counts <- as_tibble(max_counts(level = "all", path = save_dir)) |>
    filter(species == "wost")
  
  cat("✓ WOST nest numbers successfully extracted.\n")
  return(wost_counts)
}

# =============================================================================
# 4. Download nest fecundity for WOST
# =============================================================================
download_wost_fecundity <- function(save_dir = "data") {
  cat("📥 Downloading WOST nest fecundity data...\n")
  
  # TODO: Replace this URL with the actual link to your fecundity CSV/Excel file
  fecundity_url <- "https://example.com/path/to/wost_fecundity.csv"
  dest_file <- file.path(save_dir, "wost_fecundity.csv")
  
  tryCatch({
    download.file(fecundity_url, destfile = dest_file, mode = "wb", quiet = TRUE)
    fecundity_data <- read_csv(dest_file, show_col_types = FALSE)
    
    cat("✓ Fecundity data downloaded successfully.\n")
    return(fecundity_data)
  }, error = function(e) {
    cat("⚠ Could not download fecundity data. Please check the URL.\n")
    return(NULL)
  })
}

# =============================================================================
# 5. Execute and Save Data
# =============================================================================
cat("========================================================\n")
cat("STARTING DATA SCRAPING PIPELINE\n")
cat("========================================================\n\n")

# 1. EDEN Data [4]
eden_water_data <- download_eden_data()
write_csv(eden_water_data, "data/processed_eden_water.csv")

# 2. Shapefiles
download_wost_shapefiles()

# 3. Nest Numbers [1]
wost_nest_counts <- download_wost_nest_numbers()
write_csv(wost_nest_counts, "data/wost_nest_counts.csv")

# 4. Nest Fecundity
wost_fecundity <- download_wost_fecundity()
# (The function already downloads it as a CSV, but you can manipulate and re-save here if needed)

cat("\n========================================================\n")
cat("✅ ALL DATA SCRAPED AND SAVED TO THE 'data' DIRECTORY\n")
cat("========================================================\n")