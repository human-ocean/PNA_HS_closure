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

## Load data -------------------------------------------------------------------
wcpfc_data <- read_rds(file = here("data/processed/wcpfc_ps_annual.rds"))
hs_pocket <- read_sf(dsn = here("data/processed/PNA_high_seas_pockets.gpkg"))

ref_rast <- rast(xmin = 150, xmax = 180,
                 ymin = -20, ymax = 20, res = 1, crs = "EPSG:4326")
# PROCESSING ###################################################################

## Some step -------------------------------------------------------------------
wcpfc_sf <- wcpfc_data |> 
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326", remove = F)

wcpfc_rast <- rasterize(wcpfc_sf,
                        y = ref_rast)

cells_inside <- exact_extract(wcpfc_rast, hs_pocket, include_xy = T) |> 
  as.data.frame() |>  
  rename(lon = x, lat = y)

cells_completely_inside <- cells_inside |> 
  filter(coverage_fraction == 1) |> 
  select(lon, lat)

inside_hs <- wcpfc_sf |> 
  inner_join(cells_completely_inside, by = join_by(lon, lat))

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

## Visualize to explain method
p <- ggplot() +
  geom_tile(data = cells_inside,
            aes(x = lon, y = lat, fill = coverage_fraction == 1),
            color = "black") +
  geom_point(data = cells_inside,
             aes(x = lon, y = lat),
             color = "black") +
  geom_sf(data = hs_pocket,
          fill = "transparent",
          color = "black",
          linewidth = 1) +
  labs(fill = "Cell completely inside",
       x = "Lon",
       y = "Lat") +
  scale_fill_manual(values = c("gray90", "gray50")) +
  theme_bw()

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

ggsave(plot = p,
       filename = here("content/img/fig_inside_HS_gridcells.png"),
       width = 6,
       height = 4) 
 