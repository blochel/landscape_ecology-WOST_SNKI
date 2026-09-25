# =============================================================================
# DATA SCRAPING AND SAVING SCRIPT
# =============================================================================
library(edenR)
library(wader)
library(dplyr)
library(readr)
library(tidyr)
library(sf)
library(RNetCDF)
library(stars)
library(units)

# =============================================================================
# 1. Download and Process daily EDEN data
# =============================================================================

# download function
download_eden_fixed <- function(eden_path = "data/WaterData", max_retries = 3) {
  dir.create(eden_path, recursive = TRUE, showWarnings = FALSE)
  metadata <- edenR::get_metadata()
  
  old_timeout <- getOption("timeout")
  options(timeout = 600)
  on.exit(options(timeout = old_timeout))
  
  base_url <- "https://sflthredds.er.usgs.gov/thredds/fileServer/eden/depths"
  
  for (i in 1:nrow(metadata)) {
    dest <- file.path(eden_path, metadata$dataset[i])
    
    if (!file.exists(dest)) {
      url     <- file.path(base_url, metadata$dataset[i])
      success <- FALSE
      attempt <- 1
      
      while (!success && attempt <= max_retries) {
        cat("Downloading", metadata$dataset[i],
            "(attempt", attempt, "of", max_retries, ")...\n")
        tryCatch({
          download.file(url, dest, mode = "wb", method = "libcurl", quiet = TRUE)
          success <- TRUE
        }, error = function(e) {
          cat("  Failed:", e$message, "\n")
          Sys.sleep(2)
        })
        attempt <- attempt + 1
      }
    }
  }
  cat("\nDownload complete!\n")
}


get_nc_times_fixed <- function(nc_file) {
  nc <- RNetCDF::open.nc(nc_file)
  on.exit(RNetCDF::close.nc(nc))
  
  time_vals  <- RNetCDF::var.get.nc(nc, "time")
  time_units <- RNetCDF::att.get.nc(nc, "time", "units")
  
  # Extract just the date-time part after "days since "
  datetime_str <- sub("days since ", "", time_units)
  
  # Normalize both formats to a single parseable string:
  datetime_str <- trimws(gsub("Z$|\\+0000$", "", trimws(datetime_str)))
  
  epoch <- as.POSIXct(datetime_str, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  
  epoch + as.difftime(time_vals, units = "days")
}

get_eden_covariates_fixed <- function(
    level           = "subregions",
    eden_path       = "data/WaterData",
    years           = edenR::available_years(eden_path),
    boundaries_path = "https://raw.githubusercontent.com/weecology/EvergladesWadingBird/refs/heads/main/SiteandMethods/",
    colony_buffers  = edenR::default_colony_buffers()) {
  
  # Fix Windows backslash issue
  eden_path <- gsub("\\\\", "/", normalizePath(eden_path))
  
  eden_data_files <- list.files(eden_path, pattern = "_depth.nc")
  boundaries      <- edenR:::get_boundaries(boundaries_path, level, colony_buffers)
  
  examp_eden_file <- stars::read_stars(paste0(eden_path, "/", eden_data_files[1]))
  boundaries_utm  <- sf::st_transform(boundaries, sf::st_crs(examp_eden_file))
  
  covariates <- c()
  
  for (year in years) {
    print(paste("Processing", year, "..."))
    
    pattern  <- paste0(year,                  "_.*_depth.nc")
    pattern2 <- paste0(as.numeric(year) - 1, "_.*_depth.nc")
    pattern3 <- paste0(as.numeric(year) - 2, "_.*_depth.nc")
    
    nc_files <- c(
      list.files(eden_path, pattern3, full.names = TRUE),
      list.files(eden_path, pattern2, full.names = TRUE),
      list.files(eden_path, pattern,  full.names = TRUE)
    )
    nc_files <- gsub("\\\\", "/", nc_files)
    
    # Use fixed time reader
    time_values <- do.call(c, lapply(nc_files, get_nc_times_fixed))
    
    year_data <- stars::read_stars(nc_files, along = "time") %>%
      stars::st_set_dimensions("time", values = time_values) %>%
      setNames("depth") %>%
      dplyr::mutate(depth = dplyr::case_when(
        depth <  units::set_units(0, cm) ~ units::set_units(0, cm),
        depth >= units::set_units(0, cm) ~ depth,
        is.na(depth)                     ~ units::set_units(NA, cm)
      ))
    
    

# Sections to edit if needed ----------------------------------------------

    breed_start <- as.POSIXct(paste0(year, "-01-01"), tz = "UTC")
    breed_end   <- as.POSIXct(paste0(year, "-06-30"), tz = "UTC")
    breed_season_data <- year_data %>%
      dplyr::filter(time >= breed_start, time <= breed_end)
    
    dry_start <- as.POSIXct(paste0(as.numeric(year) - 2, "-03-31"), tz = "UTC")
    dry_end   <- as.POSIXct(paste0(year, "-06-30"), tz = "UTC")
    dry_season_data <- year_data %>%
      dplyr::filter(time >= dry_start, time <= dry_end)
    
    pre_breed_end          <- as.POSIXct(paste0(year, "-03-01"), tz = "UTC")
    pre_breed_season_data  <- year_data %>%
      dplyr::filter(time >= breed_start, time <= pre_breed_end)
    post_breed_season_data <- year_data %>%
      dplyr::filter(time >= pre_breed_end, time <= breed_end)
    
    breed_season_depth <- breed_season_data %>%
      stars::st_apply(c(1, 2), mean) %>% setNames("breed_season_depth")
    init_depth         <- breed_season_data[, , , 1] %>% setNames("init_depth")
    recession          <- edenR:::calc_recession(breed_season_data)      %>% setNames("recession")
    pre_recession      <- edenR:::calc_recession(pre_breed_season_data)  %>% setNames("pre_recession")
    post_recession     <- edenR:::calc_recession(post_breed_season_data) %>% setNames("post_recession")
    dry_days           <- edenR:::calc_dry_days(dry_season_data)         %>% setNames("dry_days")
    reversals          <- edenR:::calc_reversals(breed_season_data)      %>% setNames("reversals")
    
    predictors <- list(init_depth, breed_season_depth, recession,
                       pre_recession, post_recession, dry_days, reversals)
    
    for (predictor in predictors) {
      year_covariates <- edenR:::extract_region_means(predictor, boundaries_utm) %>%
        dplyr::mutate(year = year)
      covariates <- rbind(covariates, year_covariates)
    }
  }
  
  return(covariates)
}

# Processing function
process_eden_data <- function(eden_path = "data/WaterData") {
  eden_path <- gsub("\\\\", "/", normalizePath(eden_path))
  cat("Calculating covariates from:", eden_path, "\n")
  
  # Only process years with available data
  target_years <- edenR::available_years(eden_path)
  target_years <- target_years[target_years >= 1993]
  
  water <- get_eden_covariates_fixed(eden_path = eden_path, level = "subregions", years = target_years) |>
    bind_rows(get_eden_covariates_fixed(eden_path = eden_path, level = "all",        years = target_years)) |>
    bind_rows(get_eden_covariates_fixed(eden_path = eden_path, level = "wcas",       years = target_years)) |>
    dplyr::select(year, region = Name, variable, value) |>
    as.data.frame() |>
    dplyr::select(-geometry) |>
    pivot_wider(names_from = "variable", values_from = "value") |>
    mutate(year = as.integer(year)) |>
    arrange(year, region)
  
  return(water)
}

# Extract daily mean water depth per region


get_daily_water_levels <- function(
    eden_path       = "data/WaterData",
    level           = "subregions",
    years           = NULL,
    boundaries_path = "https://raw.githubusercontent.com/weecology/EvergladesWadingBird/refs/heads/main/SiteandMethods/",
    colony_buffers  = edenR::default_colony_buffers()) {
  
  eden_path <- gsub("\\\\", "/", normalizePath(eden_path))
  
  if (is.null(years)) {
    years <- edenR::available_years(eden_path)
    years <- years[years >= 1993]
  }
  
  eden_data_files <- list.files(eden_path, pattern = "_depth.nc")
  boundaries      <- edenR:::get_boundaries(boundaries_path, level, colony_buffers)
  examp_file      <- stars::read_stars(paste0(eden_path, "/", eden_data_files[1]))
  boundaries_utm  <- sf::st_transform(boundaries, sf::st_crs(examp_file))
  
  all_daily <- list()
  
  for (year in years) {
    print(paste("Extracting daily levels for", year, "..."))
    
    pattern <- paste0(year, "_.*_depth.nc")
    nc_files <- list.files(eden_path, pattern, full.names = TRUE)
    nc_files <- gsub("\\\\", "/", nc_files)
    
    time_values <- do.call(c, lapply(nc_files, get_nc_times_fixed))
    
    year_data <- stars::read_stars(nc_files, along = "time") %>%
      stars::st_set_dimensions("time", values = time_values) %>%
      setNames("depth") %>%
      dplyr::mutate(depth = dplyr::case_when(
        depth <  units::set_units(0, cm) ~ units::set_units(0, cm),
        depth >= units::set_units(0, cm) ~ depth,
        is.na(depth)                     ~ units::set_units(NA, cm)
      ))
    
    # Extract mean depth per region per day
    daily <- edenR:::extract_region_means(year_data, boundaries_utm) %>%
      dplyr::rename(date = time, region = Name, depth_cm = value) %>%
      dplyr::mutate(
        date   = as.Date(date),
        year   = as.integer(year),
        region = as.character(region)
      ) %>%
      as.data.frame() %>%
      dplyr::select(-geometry)
    
    all_daily[[as.character(year)]] <- daily
  }
  
  result <- dplyr::bind_rows(all_daily) %>%
    dplyr::arrange(region, date)
  
  return(result)
}
# =============================================================================
# 2. Download shapefiles of nesting location for WOST
# =============================================================================
download_wost_shapefiles <- function(save_dir = "data/shapefiles") {
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  
  shapefile_url <- "https://raw.githubusercontent.com/weecology/EvergladesWadingBird/main/SiteandMethods/colonies/colonies.geojson"
  dest_file     <- file.path(save_dir, "colonies.geojson")
  
  tryCatch({
    download.file(shapefile_url, destfile = dest_file, mode = "wb", quiet = TRUE)
    cat("Shapefiles downloaded to:", dest_file, "\n")
    wost_sf <- sf::st_read(dest_file, quiet = TRUE)
    return(wost_sf)
  }, error = function(e) {
    cat("Could not download shapefiles. Please check the URL.\n")
    return(NULL)
  })
}

# =============================================================================
# 3. Download nest numbers for WOST
# =============================================================================
download_wost_nest_numbers <- function(save_dir = "data/wader_data") {
  cat("Downloading wader observation data for WOST nest numbers...\n")
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  
  download_observations(save_dir)
  
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
  
  fecundity_url <- "https://raw.githubusercontent.com/weecology/EvergladesWadingBird/main/Nesting/nest_success.csv"
  dest_file     <- file.path(save_dir, "wost_fecundity.csv")
  
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

# 1. EDEN Data
eden_water_data <- process_eden_data(eden_path = "data/WaterData")
write_csv(eden_water_data, "data/processed_eden_water.csv")

# 1b. Daily water levels
daily_water <- get_daily_water_levels(
  eden_path = "data/WaterData",
  level     = "subregions"   # or "all", "wcas"
)
write_csv(daily_water, "data/daily_water_levels.csv")

# 2. Shapefiles
download_wost_shapefiles()

# 3. Nest Numbers
wost_nest_counts <- download_wost_nest_numbers()
write_csv(wost_nest_counts, "data/wost_nest_counts.csv")

# 4. Nest Fecundity
wost_fecundity <- download_wost_fecundity()

cat("\n========================================================\n")
cat(" ALL DATA SCRAPED AND SAVED TO THE 'data' DIRECTORY\n")
cat("========================================================\n")