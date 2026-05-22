################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
# date
#
# As defined in the preregistration for H1:
# 
# Treatment grid cells are those within the high seas pocket,
# and control grid cells are other high seas grid cells that fall under
# the WCPFC convention area and lie between 20°N–20°S.
#
# This script identifies the treated and control grid cells for H1.
# 
################################################################################
  
# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse,
  exactextractr,
  terra,
  sf
)

## Load data -------------------------------------------------------------------
wcpfc_data <- read_rds(file = here("data/processed/wcpfc_ps_annual.rds"))
hs_pocket <- read_sf(dsn = here("data/processed/PNA_high_seas_pockets.gpkg"))
hs <- read_sf(dsn = here("data/raw/World_High_Seas_v2_20241010_gpkg/High_Seas_v2.gpkg"))
wcpfc <- read_sf(here("data/processed/WCPFC_convention_area.gpkg"))

# Declare a reference raster. It is already limited to ± 20° of latitude
ref_rast <- rast(xmin = -180, xmax = 180,
                 ymin = -20, ymax = 20, res = 1, crs = "EPSG:4326")
# PROCESSING ###################################################################

# Step 1) Convert WCPFC data to vector and raster objects ----------------------
# vector
wcpfc_sf <- wcpfc_data |> 
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326", remove = F)
# raster (no data, only presence / absence)
wcpfc_rast <- rasterize(wcpfc_sf,
                        y = ref_rast)

# Step 2) Find treated cells: The ones that are inside the high seas pocket. ---
treated_hs_cells <- exact_extract(wcpfc_rast, hs_pocket, include_xy = T) |> 
  bind_rows(.id = ) |> 
  as.data.frame() |>  
  rename(lon = x, lat = y) |> 
  filter(coverage_fraction == 1) |>
  select(lon, lat) |> 
  mutate(treated = 1)

# Step 3) Find control cells: The ones that are entirely within the high seas
# First get a high seas area that matches the convention?
sf_use_s2(F)
wcpfc_hs_area <- hs |> 
  st_intersection(wcpfc)
sf_use_s2(T)

control_hs_cells <- exact_extract(wcpfc_rast, wcpfc_hs_area, include_xy = T) |> 
  as.data.frame() |>  
  rename(lon = x, lat = y) |> 
  filter(coverage_fraction == 1) |> 
  anti_join(treated_hs_cells, by = join_by(lon, lat)) |> # Remove cells that are in the pockets
  select(lon, lat) |> 
  mutate(treated = 0)

hs_cateogries <- bind_rows(treated_hs_cells,
                           control_hs_cells)

h1_panel <- wcpfc_sf |> 
  st_drop_geometry() |> 
  inner_join(hs_cateogries, by = join_by(lon, lat)) |> 
  mutate(post = 1 * (year > 2009),
         id = paste(lat, lon),
         group = ifelse(treated == 1, "Treatment", "Control"))

## EXPORT ######################################################################
# Export the panel 
write_rds(x = h1_panel,
          file = here("data/processed/h1_panel.rds"))
 