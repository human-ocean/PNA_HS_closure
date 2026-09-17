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
hs_pocket <- read_sf(dsn = here("data/processed/PNA_high_seas_pockets.gpkg")) |> 
  filter(id == 8)

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

treated_hs_cells <- exact_extract(wcpfc_rast, hs_pocket, include_xy = T) |> 
  bind_rows(.id = ) |> 
  as.data.frame() |>  
  rename(lon = x, lat = y) |> 
  filter(coverage_fraction == 1) |>
  select(lon, lat) |> 
  mutate(fully = 1)

pts_in <- wcpfc_sf |> 
  st_filter(hs_pocket) |> 
  left_join(treated_hs_cells) |> 
  replace_na(replace = list(fully = 0))

ggplot() +
  geom_sf(data = hs_pocket) + 
  geom_tile(data = pts_in, aes(x = lon, y = lat, fill = factor(fully)), color = "black", alpha = 0.5) +
  facet_wrap(~year) +
  theme_void()

ggplot(data = pts_in, aes(x = year, y = days, group = fully, color = factor(fully))) +
  stat_summary(geom = "line", fun = sum) +
  theme_bw()

mapview::mapview(list(pts_in, hs_pocket))
