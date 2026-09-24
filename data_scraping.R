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
  dir.create(eden_path, recursive = TRUE, showWarnings = FALSE)
  
  # 1. Download all missing EDEN NetCDF files (1991 to 2026)
  cat("Checking for missing EDEN NetCDF files (1991-2026)...\n")
  cat("This may take some time if you are missing many historical years.\n")
  
  base_url <- "https://sflthredds.er.usgs.gov/thredds/fileServer/eden/depths/"
  
  for (y in 1991:2026) {
    for (q in 1:4) {
      if (y == 2026 && q %in% c(3, 4)) next 
      
      file_name <- paste0(y, "_q", q, "_depth.nc")
      dest_file <- file.path(eden_path, file_name)
      
      # If the file isn't in the folder yet, download it
      if (!file.exists(dest_file)) {
        cat("  Downloading missing file:", file_name, "...\n")
        url <- paste0(base_url, file_name)
        tryCatch({
          download.file(url, destfile = dest_file, mode = "wb", quiet = TRUE)
        }, error = function(e) {
          cat("  ⚠ Warning: Could not download", file_name, "\n")
        })
      }
    }
  }
  
  # 2. Run the covariate calculations
  cat("All files present. Calculating covariates...\n")
  water <- get_eden_covariates(eden_path = eden_path, level = "subregions") |>
    bind_rows(get_eden_covariates(eden_path = eden_path, level = "all")) |>
    bind_rows(get_eden_covariates(eden_path = eden_path, level = "wcas")) |>
    dplyr::select(year, region = Name, variable, value) |>
    as.data.frame() |>
    dplyr::select(-geometry) |>
    pivot_wider(names_from = "variable", values_from = "value") |>
    mutate(year = as.integer(year)) |>
    arrange(year, region)
  
  return(water)
}

# =============================================================================
# 2. Download shapefiles of nesting location for WOST
# =============================================================================
download_eden_data <- function(eden_path = "data/WaterData") {
  dir.create(eden_path, recursive = TRUE, showWarnings = FALSE)
  
  # Update and fetch water data
  update_water(eden_path)
  
  water <- get_eden_covariates(eden_path = eden_path, level = "subregions") |>
    bind_rows(get_eden_covariates(eden_path = eden_path, level = "all")) |>
    bind_rows(get_eden_covariates(eden_path = eden_path, level = "wcas")) |>
    # The date is usually stored in the dataframe returned by get_eden_covariates
    # We filter out anything after June 30, 2026 (skipping Q3 and Q4)
    filter(date <= as.Date("2026-06-30")) |> 
    dplyr::select(year, region = Name, variable, value) |>
    as.data.frame() |>
    dplyr::select(-geometry) |>
    pivot_wider(names_from = "variable", values_from = "value") |>
    mutate(year = as.integer(year)) |>
    arrange(year, region)
  
  return(water)
}
# =============================================================================
# 3. Download nest numbers for WOST
# =============================================================================
download_wost_nest_numbers <- function(save_dir = "data/wader_data") {
  cat("Downloading wader observation data for WOST nest numbers...\n")
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Uses your existing pipeline's method to fetch observation data
  download_observations(save_dir)
  
  # Extract max counts and filter strictly for Wood Stork (wost)
  wost_counts <- as_tibble(max_counts(level = "colony", path = save_dir)) |>
    filter(species == "wost")
  
  cat("WOST nest numbers successfully extracted.\n")
  return(wost_counts)
}

# =============================================================================
# 4. Download nest fecundity for WOST
# =============================================================================
download_wost_fecundity <- function(save_dir = "data") {
  cat("Downloading WOST nest fecundity data...\n")
  
  # Use the RAW GitHub URL for the CSV file (remove /blob/)
  fecundity_url <- "https://raw.githubusercontent.com/weecology/EvergladesWadingBird/main/Nesting/nest_success.csv"
  dest_file <- file.path(save_dir, "wost_fecundity.csv")
  
  tryCatch({
    download.file(fecundity_url, destfile = dest_file, mode = "wb", quiet = TRUE)
    fecundity_data <- read_csv(dest_file, show_col_types = FALSE)
    
    cat("Fecundity data downloaded successfully.\n")
    return(fecundity_data)
  }, error = function(e) {
    cat("Could not download fecundity data. Please check the URL.\n")
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
cat(" ALL DATA SCRAPED AND SAVED TO THE 'data' DIRECTORY\n")
cat("========================================================\n")