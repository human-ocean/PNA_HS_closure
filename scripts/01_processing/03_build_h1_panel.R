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

st_erase <- function(a, b){
  st_filter(a, st_union(b), .predicate = st_disjoint)
}

## Load data -------------------------------------------------------------------
wcpfc_data <- read_rds(file = here("data/processed/wcpfc_ps_annual.rds"))
hs_pocket <- read_sf(dsn = here("data/processed/PNA_high_seas_pockets.gpkg"))
hs <- read_sf(dsn = here("data/raw/World_High_Seas_v2_20241010_gpkg/High_Seas_v2.gpkg"))

ref_rast <- rast(xmin = -180, xmax = 180,
                 ymin = -20, ymax = 20, res = 1, crs = "EPSG:4326")
# PROCESSING ###################################################################

## Make things spatial  --------------------------------------------------------
wcpfc_sf <- wcpfc_data |> 
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326", remove = F)

wcpfc_rast <- rasterize(wcpfc_sf,
                        y = ref_rast)

# Find treated cells: The ones that are inside the high seas pocket.
treated_hs_cells <- exact_extract(wcpfc_rast, hs_pocket, include_xy = T) |> 
  as.data.frame() |>  
  rename(lon = x, lat = y) |> 
  filter(coverage_fraction == 1) |> 
  select(lon, lat) |> 
  mutate(treated = 1)

# Find control cells: The ones that are entirely within the high seas
sf_use_s2(F)
hs_area <- hs |> 
  st_crop(eezs)
sf_use_s2(T)

control_hs_cells <- exact_extract(wcpfc_rast, hs_area, include_xy = T) |> 
  as.data.frame() |>  
  rename(lon = x, lat = y) |> 
  filter(coverage_fraction == 1) |> 
  anti_join(treated_hs_cells, by = join_by(lon, lat)) |> 
  select(lon, lat) |> 
  mutate(treated = 0)

hs_cateogries <- bind_rows(treated_hs_cells,
                           control_hs_cells)

inside_hs <- wcpfc_sf |> 
  inner_join(hs_cateogries, by = join_by(lon, lat))

## EXPORT ######################################################################
# Export the panel 
write_rds(x = inside_hs,
          file = here("data/processed/h1_panel.rds"))
 