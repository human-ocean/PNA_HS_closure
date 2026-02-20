################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
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
  sf,
  terra,
  tidyterra
)

## Load data -------------------------------------------------------------------
wcpfc_data <- read_rds(file = here("data/processed/wcpfc_ps_annual.rds"))
hs_pocket <- read_sf(dsn = here("data/processed/PNA_high_seas_pockets.gpkg")) |> 
  st_transform(crs = "EPSG:8859")
hs <- read_sf(dsn = here("data/raw/World_High_Seas_v2_20241010_gpkg/High_Seas_v2.gpkg"))
eezs <- read_sf(dsn = here("data/processed/PNA_eezs.gpkg"))

ref_rast <- rast(xmin = 138, xmax = 177,
                 ymin = -13, ymax = 6, res = 1, crs = "EPSG:4326")

inside_hs <- read_rds(file = here("data/processed/h1_panel.rds")) |> 
  filter(treated == 1)

days <- inside_hs |> 
  filter(between(year, 2005, 2014)) |> 
  rasterize(ref_rast, field = "days", by = "year")

sets <- inside_hs |> 
  filter(between(year, 2005, 2014)) |> 
  rasterize(ref_rast, field = "num_sets", by = "year")

# VISUALIZE ####################################################################

days_map <- ggplot() +
  geom_spatraster(data = log(days)) +
  geom_sf(data = hs_pocket, linewidth = 1, fill = "transparent", color = "black") +
  facet_wrap(~lyr, ncol = 5) +
  scale_fill_viridis_c(option = "mako", na.value = "transparent") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title.position = "top") +
  labs(fill = "Fishing effort (log-days)")

sets_map <- ggplot() +
  geom_spatraster(data = log(sets)) +
  geom_sf(data = hs_pocket, linewidth = 1, fill = "transparent", color = "black") +
  facet_wrap(~lyr, ncol = 5) +
  scale_fill_viridis_c(option = "mako", na.value = "transparent") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title.position = "top") +
  labs(fill = "Fishing effort (log-days)")

# EXPORT #######################################################################

## The final step --------------------------------------------------------------
ggsave(plot = days_map,
       filename = here("content/img/fig_map_day_HS_effort.png"),
       width = 8,
       height = 3.5) 

ggsave(plot = sets_map,
       filename = here("content/img/fig_maps_sets_HS_effort.png"),
       width = 8,
       height = 3.5) 

