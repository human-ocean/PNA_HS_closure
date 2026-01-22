################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
# Started on Nov 4, 2025
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse,
  sf,
  mapview,
  rmapshaper
)


# Load data --------------------------------------------------------------------
hs <- read_sf(here("data/raw/World_High_Seas_v2_20241010_gpkg/High_Seas_v2.gpkg"))
eez_raw <- read_sf(here("data/raw/World_EEZ_v12_20231025_gpkg/eez_v12.gpkg"))


## PROCESSING ##################################################################

# Step 1) Get EEZs in the area of interest  ------------------------------------
# Australia, China, Canada, Cook Islands, European Union, Federated States of Micronesia,
# Fiji, France, Indonesia, Japan, Kiribati, Republic of Korea, Republic of Marshall Islands,
# Nauru, New Zealand, Niue, Palau, Papua New Guinea, Philippines, Samoa,
# Solomon Islands, Tonga, Tuvalu, United States of America, Vanuatu

eezs <- eez_raw |>
  filter(ISO_TER1 %in% c("FSM", "KIR", "MHL", "NRU", "PLW", "PNG", "SLB", "TUV", "TKL")) |> 
  st_break_antimeridian(lon_0 = 150) |> 
  st_transform(crs = "EPSG:8859") |> 
  rmapshaper::ms_simplify() |> 
  select(ISO_TER1)

# Step 2) Find HS pockets ------------------------------------------------------
hs_PNA <- hs |> 
  st_break_antimeridian(lon_0 = 150) |> 
  st_transform(crs = "EPSG:8859") |> 
  rmapshaper::ms_simplify() |> 
  st_crop(eezs) |> 
  st_cast(to = "POLYGON") |> 
  mutate(id = 1:n()) |> 
  filter(id %in% c(6, 8)) %>% #id 15 is the large polygon, id 17 is the small one to the west
  mutate(area_km2 = st_area(.),
         area_km2 = units::set_units(area_km2, "km2")) |> 
  select(id, area_km2, source) |> 
  st_transform(crs = "EPSG:4326")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
write_sf(obj = hs_PNA,
         dsn = here("data/processed/PNA_high_seas_pockets.gpkg"),
         delete_dsn = T)

write_sf(obj = eezs |> 
           st_transform(crs = "EPSG:4326"),
         dsn = here("data/processed/PNA_eezs.gpkg"),
         delete_dsn = T)
