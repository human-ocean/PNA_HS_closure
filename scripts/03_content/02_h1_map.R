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

############
ts_days <- ggplot(data = inside_hs,
       aes(x = year, y = days)) +
  geom_vline(xintercept = 2009.5, linetype = "dashed") +
  stat_summary(geom = "line", fun = "mean",
               linewidth = 1,
               color = "black") +
  stat_summary(geom = "linerange", fun.data = "mean_cl_normal",
               linewidth = 0.5) +
  stat_summary(geom = "linerange", fun.data = "mean_se",
               linewidth = 1.5,
               color = "steelblue3") +
  stat_summary(geom = "point", fun = "mean",
               size = 3,
               shape = 21,
               fill = "steelblue3",
               color = "black") +
  theme_linedraw() +
  guides(fill = FALSE,
         shape = guide_legend(
           override.aes = list(shape = c(16, 15))
         )) +
  labs(x = "Year",
       y = "Fishing effort (days) ± SE and 95% CI")

ts_sets <- ggplot(data = inside_hs,
                  aes(x = year, y = num_sets)) +
  geom_vline(xintercept = 2009.5, linetype = "dashed") +
  stat_summary(geom = "line", fun = "mean",
               linewidth = 1,
               color = "black") +
  stat_summary(geom = "linerange", fun.data = "mean_cl_normal",
               linewidth = 0.5) +
  stat_summary(geom = "linerange", fun.data = "mean_se",
               linewidth = 1.5,
               color = "steelblue3") +
  stat_summary(geom = "point", fun = "mean",
               size = 3,
               shape = 21,
               fill = "steelblue3",
               color = "black") +
  theme_linedraw() +
  guides(fill = FALSE,
         shape = guide_legend(
           override.aes = list(shape = c(16, 15))
         )) +
  labs(x = "Year",
       y = "Fishing effort (sets) ± SE and 95% CI")


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

ggsave(plot = ts_days,
       filename = here("content/img/fig_ts_days_HS_effort.png"),
       width = 8,
       height = 3.5) 

ggsave(plot = ts_sets,
       filename = here("content/img/fig_ts_sets_HS_effort.png"),
       width = 8,
       height = 3.5) 

