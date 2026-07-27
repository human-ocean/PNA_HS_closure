################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
# date
#
# As defined in the preregistration for H2:
# 
# Grid cells inside high seas pocket are ``treated'' and other high seas grid
# cells that fall under the WCPFC convention area and lie between 20°N–20°S
# will be considered ``control''
#
# This script identifies the treated and control grid cells for H2.
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
ll_wcpfc_data <- read_rds(file = here("data/processed/wcpfc_ll_annual.rds"))
hs_pocket <- read_sf(dsn = here("data/processed/PNA_high_seas_pockets.gpkg"))
hs <- read_sf(dsn = here("data/raw/World_High_Seas_v2_20241010_gpkg/High_Seas_v2.gpkg"))
wcpfc <- read_sf(here("data/processed/WCPFC_convention_area.gpkg"))

# Declare a reference raster. It is already limited to ± 20° of latitude
ref_rast <- rast(xmin = -180, xmax = 180,
                 ymin = -20, ymax = 20, res = 5, crs = "EPSG:4326")
# PROCESSING ###################################################################

# Step 1) Convert WCPFC data to vector and raster objects ----------------------
# vector
wcpfc_sf <- ll_wcpfc_data |> 
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326", remove = F)
# raster (no data, only presence / absence)
wcpfc_rast <- rasterize(wcpfc_sf,
                        y = ref_rast)

# Step 2) Find treated cells: The ones that are inside the high seas pocket. ---
treated_hs_cells <- exact_extract(wcpfc_rast, hs_pocket, include_xy = T) |> 
  bind_rows(.id = "src") |> 
  as.data.frame() |>  
  rename(lon = x, lat = y) |> 
  select(lon, lat, coverage_fraction) |> 
  mutate(treated = 1)

# Step 3) Find control cells: The ones that are entirely within the high seas
# First get a high seas area that matches the convention?
sf_use_s2(F)
wcpfc_hs_area <- hs |>
  st_intersection(wcpfc)
sf_use_s2(T)

control_hs_cells <- exact_extract(wcpfc_rast, wcpfc, include_xy = T) |> 
  as.data.frame() |>  
  rename(lon = x, lat = y) |> 
  anti_join(treated_hs_cells, by = join_by(lon, lat)) |> 
  select(lon, lat, coverage_fraction) |> 
  mutate(treated = 0)

hs_cateogries <- bind_rows(treated_hs_cells,
                           control_hs_cells)

# Visualize --------------------------------------------------------------------
p <- ggplot(hs_cateogries |> filter (lon > 0),
       aes(x = lon, y = lat, fill = coverage_fraction * treated)) +
  geom_tile(color = "black") +
  geom_sf(data = hs_pocket, inherit.aes = F, fill = "transparent", color = "gray", linewidth = 2) +
  geom_point(aes(color = factor(treated))) +
  theme_bw() +
  scale_fill_viridis_b(option = "mako") +
  scale_color_manual(values = c("gray90", "red")) +
  labs(fill = "% inside HS pocket",
       color = "Treated",
       subtitle = "Control pixels in the Western hemisphere not shown")

# ------------------------------------------------------------------------------
h2_panel <- wcpfc_sf |> 
  st_drop_geometry() |> 
  inner_join(hs_cateogries, by = join_by(lon, lat)) |> 
  mutate(post = 1 * (year > 2009),
         id = paste(lat, lon),
         group = ifelse(treated == 1, "Treatment", "Control"))

## EXPORT ######################################################################
# Export the panel 
write_rds(x = h2_panel,
          file = here("data/processed/h2_panel.rds"))
