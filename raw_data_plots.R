# =============================================================================
# WOST NESTING PLOTS AND MAPS
# =============================================================================

# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)
library(sf)

# =============================================================================
# 1. DEFINE MASTER COLOR PALETTE
# =============================================================================

region_colors <- c(
  "1" = "#E41A1C",          
  "2a" = "#377EB8",         
  "2b" = "#4DAF4A",         
  "3a" = "#984EA3",         
  "3b" = "#FF7F00",         
  "coastalenp" = "#FFFF33", 
  "inlandenp" = "black",  
  "wca1" = "#F781BF",       
  "wca2" = "#999999",       
  "3an" = "#1B9E77",        
  "3as" = "lightblue",        
  "3ase" = "#7570B3"        
)

# =============================================================================
# 2. PLOT: AVERAGE WOST NESTS PER YEAR WITH ERROR BARS
# =============================================================================


wost_nests <- read_csv("data/wost_nest_counts.csv", show_col_types = FALSE)


region_yearly_summary <- wost_nests |>
  group_by(year, subregion) |>
  summarise(
    mean_nests = mean(count, na.rm = TRUE),
    sd_nests = sd(count, na.rm = TRUE),
    n = n(),
    se_nests = sd_nests / sqrt(n),
    .groups = "drop"
  )

min_yr <- min(region_yearly_summary$year, na.rm = TRUE)
max_yr <- max(region_yearly_summary$year, na.rm = TRUE)


nest_plot <- ggplot(region_yearly_summary, aes(x = year, y = mean_nests, color = subregion)) +
  geom_vline(xintercept = seq(min_yr + 0.5, max_yr - 0.5, by = 1), 
             color = "gray85", linewidth = 0.5) +
  geom_errorbar(
    aes(ymin = mean_nests - se_nests, ymax = mean_nests + se_nests), 
    position = position_dodge(width = 0.6), 
    width = 0.2, 
    alpha = 0.7
  ) +
  geom_point(position = position_dodge(width = 0.6), size = 2.5) +
  scale_color_manual(values = region_colors) +
  theme_minimal() +
  labs(
    title = "Average Wood Stork (WOST) Nests with Standard Error",
    x = "Year",
    y = "Average Nests per Colony",
    color = "Subregion"
  ) +
  scale_x_continuous(breaks = seq(min_yr, max_yr, by = 1)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

nest_plot

# =============================================================================
# 3. MAP: EDEN SUBREGIONS
# =============================================================================

# Read the EDEN subregions shapefile directly from the GitHub repository
subregions_url <- "https://raw.githubusercontent.com/weecology/EvergladesWadingBird/main/SiteandMethods/regions/subregions.geojson"
subregions_sf <- sf::st_read(subregions_url, quiet = TRUE)

region_map <- ggplot(data = subregions_sf) +
  geom_sf(aes(fill = Name), color = "black", linewidth = 0.3, alpha = 0.8) +
  scale_fill_manual(values = region_colors) +
  theme_void() +
  labs(
    title = "Everglades EDEN Subregions",
    fill = "Subregion"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10))
  )

region_map