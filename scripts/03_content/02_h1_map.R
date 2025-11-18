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
  sf
)

## Load data -------------------------------------------------------------------
wcpfc_data <- read_rds(file = here("data/processed/wcpfc_ps_annual.rds"))
hs_pocket <- read_sf(dsn = here("data/processed/PNA_high_seas_pockets.gpkg"))
hs <- read_sf(dsn = here("data/raw/World_High_Seas_v2_20241010_gpkg/High_Seas_v2.gpkg"))
eezs <- read_sf(dsn = here("data/processed/PNA_and_neighbor_eezs.gpkg"))

inside_hs <- read_rds(file = here("data/processed/h1_panel.rds")) |> 
  mutate(post = 1 * (year > 2009),
         id = paste(lat, lon))

# VISUALIZE ####################################################################

days_map <- ggplot() +
  geom_sf(data = hs_pocket, linewidth = 1, fill = "transparent", color = "black") +
  geom_sf(data = inside_hs, aes(color = log(days)), size = 2) +
  facet_wrap(~year, ncol = 5) +
  scale_color_viridis_c(option = "mako") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Effort (days)")

sets_map <- ggplot() +
  geom_sf(data = hs_pocket, linewidth = 1, fill = "transparent", color = "black") +
  geom_sf(data = inside_hs, aes(color = log(num_sets)), size = 2) +
  facet_wrap(~year, ncol = 5) +
  scale_color_viridis_c(option = "mako") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Effort (sets)")

maps <- cowplot::plot_grid(days_map, sets_map)


days_ts <- ggplot(inside_hs,
                  aes(x = year, y = days)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2009.75, linetype = "dashed") +
  stat_summary(geom = "line", fun = "mean") +
  stat_summary(geom = "pointrange", fun.data = "mean_cl_normal") +
  stat_summary(geom = "pointrange", fun.data = "mean_se", linewidth = 1.5) +
  theme_bw() +
  labs(x = "Year",
       y = "Mean effort (days) ± S.E. & 95%CI")

sets_ts <- ggplot(inside_hs,
                  aes(x = year, y = num_sets)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2009.75, linetype = "dashed") +
  stat_summary(geom = "line", fun = "mean") +
  stat_summary(geom = "pointrange", fun.data = "mean_cl_normal") +
  stat_summary(geom = "pointrange", fun.data = "mean_se", linewidth = 1.5) +
  theme_bw() +
  labs(x = "Year",
       y = "Mean effort (sets) ± S.E. & 95%CI")

ts <- cowplot::plot_grid(days_ts, sets_ts, ncol = 1)

# EXPORT #######################################################################

## The final step --------------------------------------------------------------
ggsave(plot = maps,
       filename = here("content/img/fig_maps_HS_effort.png"),
       width = 16,
       height = 9) 

ggsave(plot = ts,
       filename = here("content/img/fig_ts_HS_effort.png"),
       width = 8,
       height = 5) 