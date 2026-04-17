################################################################################
# title
################################################################################
#
# Your Name Here
# Your email here
# date
#
# Description
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
# High seas pockets
hs_pocket <- st_read(here("data/processed/PNA_high_seas_pockets.gpkg")) |> 
  st_union()
eezs <- st_read(here("data/processed/PNA_eezs.gpkg")) |> 
  st_wrap_dateline(options = "WRAPDATELINE=YES", quiet = TRUE) |> 
  st_make_valid() |> 
  st_union() |> 
  st_wrap_dateline(options = "WRAPDATELINE=YES", quiet = TRUE)

# Catch and effort data
ps_wcpfc_data <- read_rds(file = here("data/processed/wcpfc_ps_annual.rds")) |> # for purse seines
  filter(tot_mt > 0)
ll_wcpfc_data <- read_rds(file = here("data/processed/wcpfc_ll_annual.rds")) |> # for longline
  filter(tot_mt > 0)

# Declare a reference raster. It is already limited to ± 20° of latitude
ps_ref_rast <- rast(xmin = -180, xmax = 180,
                    ymin = -20, ymax = 20,
                    res = 1, crs = "EPSG:4326")
ll_ref_rast <- rast(xmin = -180, xmax = 180,
                    ymin = -20, ymax = 20,
                    res = 5, crs = "EPSG:4326")

# PROCESSING ###################################################################
## Build near and far buffers --------------------------------------------------
near <- st_buffer(hs_pocket, dist = 100 * 1854) |> 
  st_difference(hs_pocket)

far <- st_buffer(hs_pocket, dist = 200 * 1854) |> 
  st_difference(hs_pocket) |> 
  st_difference(near)

zones <- st_sf(zone = c("inside", "near", "far"),
               c(hs_pocket, near, far))

## Build sf versions of data buffers -------------------------------------------
ps_wcpfc_sf <- ps_wcpfc_data |> 
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326",
           remove = F)

ll_wcpfc_sf <- ll_wcpfc_data |> 
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326",
           remove = F)

# BUILD THE PANEL ##############################################################

# Step 1) Find points with centroid inside but where fising is still allowed ---
## Build a raster of pixels
ps_wcpfc_rast <- ps_wcpfc_sf |> 
  rasterize(y = ps_ref_rast)

ll_wcpfc_rast <- ll_wcpfc_sf |> 
  rasterize(y = ll_ref_rast)

# Find the fraction of the pixel that is inside the HS pocket
ps_fishable_cells <- exact_extract(ps_wcpfc_rast,
                                   hs_pocket,
                                   include_xy = T) |> 
  as.data.frame() |>  
  rename(lon = x, lat = y) |> 
  select(lon, lat, coverage_fraction)

ll_fishable_cells <- exact_extract(ll_wcpfc_rast,
                                   hs_pocket,
                                   include_xy = T) |> 
  as.data.frame() |>  
  rename(lon = x, lat = y) |> 
  select(lon, lat, coverage_fraction)

## Step 2) Assign treatment group to grid cells --------------------------------
ps_h3_ps_panel <- ps_wcpfc_sf |> 
  st_filter(c(hs_pocket, eezs)) |>
  st_join(zones) |> 
  st_drop_geometry() |> 
  drop_na(zone) |> 
  left_join(ps_fishable_cells, by = join_by(lon, lat)) |> 
  mutate(zone = case_when(zone == "inside" & coverage_fraction == 1 ~ "inside",
                          zone == "inside" & coverage_fraction < 1 ~ "near",
                          zone == "near" ~ "near",
                          zone == "far" ~ "far"),
         post = 1 * (year > 2009),
         id = paste(lat, lon),
         near = 1 * (zone == "near"))

h3_ll_panel <- ll_wcpfc_sf |> 
  st_join(zones) |> 
  st_drop_geometry() |> 
  drop_na(zone) |> 
  left_join(ll_fishable_cells, by = join_by(lon, lat)) |> 
  mutate(zone = case_when(zone == "inside" & coverage_fraction == 1 ~ "inside",
                          zone == "inside" & coverage_fraction < 1 ~ "near",
                          zone == "near" ~ "near",
                          zone == "far" ~ "far"),
         post = 1 * (year > 2009),
         id = paste(lat, lon),
         near = 1 * (zone == "near"))

# ANALYSIS #####################################################################

## Almost last step ------------------------------------------------------------
ggplot() +
  geom_sf(data = zones,
          aes(color = zone),
          fill = "transparent", linewidth = 1) +
  geom_point(data = ps_h3_ps_panel |> 
         distinct(lat, lon, zone),
         aes(color = zone, x = lon, y = lat))

ggplot() +
  geom_sf(data = zones,
          aes(color = zone),
          fill = "transparent", linewidth = 1) +
  geom_point(data = h3_ll_panel |> 
         distinct(lat, lon, zone),
         aes(color = zone, x = lon, y = lat))

# EXPORT #######################################################################

## The final step --------------------------------------------------------------  
write_rds(x = ps_h3_ps_panel,
          file = here("data/processed/h3_ps_panel.rds"))

write_rds(x = h3_ll_panel,
          file = here("data/processed/h3_ll_panel.rds"))
