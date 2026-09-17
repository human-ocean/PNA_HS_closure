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
  rnaturalearth,
  sf,
  tidyverse
)

## Load data -------------------------------------------------------------------
PNA_eezs <- read_sf(dsn = here("data/processed/PNA_eezs.gpkg")) |> 
  st_transform(crs = "EPSG:8859")

eezs <- read_sf(dsn = here("data/raw/World_EEZ_v12_20231025_gpkg/eez_v12.gpkg")) |> 
  st_break_antimeridian(lon_0 = 150) |>
  st_transform(crs = "EPSG:8859") |> 
  st_make_valid() |> 
  st_crop(st_buffer(PNA_eezs, 1000000))

wcpfc <- read_sf(dsn = here("data/processed/WCPFC_convention_area.gpkg"))
hs_pocket <- read_sf(dsn = here("data/processed/PNA_high_seas_pockets.gpkg"))

coast <- ne_countries() |> 
  st_break_antimeridian(lon_0 = 150) |>
  st_transform(crs = "EPSG:8859") |> 
  st_crop(st_buffer(PNA_eezs, 1000000))

mounts <- read_sf(here("data/raw/YessonEtAl2019-Seamounts-V2/YessonEtAl2019-SeamountBases-V2.shp"))

h1 <- read_rds(file = here("data/processed/h1_panel.rds"))
h2 <- read_rds(file = here("data/processed/h2_panel.rds"))
h3_ps <- read_rds(file = here("data/processed/h3_ps_panel.rds"))
h3_ll <- read_rds(file = here("data/processed/h3_ll_panel.rds"))

## PROCESSING ##################################################################
shallow_mounts <- mounts |> 
  st_centroid() |>
  st_transform(st_crs(PNA_eezs)) |> 
  st_crop(PNA_eezs) |> 
  mutate(depth = abs(Depth),
         shallow = depth <= 150) |> 
  arrange(depth) |> 
  filter(shallow)

h1_cells <- h1 |> 
  select(lon, lat, group) |> 
  distinct() |> 
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326") |> 
  st_transform(crs = "EPSG:8859")

h2_cells <- h2 |> 
  select(lon, lat, group) |> 
  distinct() |> 
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326") |> 
  st_transform(crs = "EPSG:8859")

h3_ps_cells <- h3_ps |> 
  filter(!zone == "inside") |> 
  select(lon, lat, near) |> 
  distinct() |> 
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326") |> 
  st_transform(crs = "EPSG:8859")

h3_ll_cells <- h3_ll |> 
  filter(!zone == "inside") |> 
  select(lon, lat, near) |> 
  distinct() |> 
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326") |> 
  st_transform(crs = "EPSG:8859")



# VISUALIZE ####################################################################

## Another step ----------------------------------------------------------------
p <- ggplot() + 
  geom_sf(data = eezs,
          aes(fill = "Non-PNA EEZ"),
          color = "black") +
  geom_sf(data = PNA_eezs,
          aes(fill = "PNA EEZ"),
          color = "black") +
  geom_sf(data = coast,
          fill = "black",
          color = "black",
          linewidth = 0) +
  geom_sf(data = hs_pocket,
          aes(fill = "High Seas Pockets"),
          color = "black") +
  scale_fill_manual(values = c("Non-PNA EEZ" = "gray90",
                               "PNA EEZ" = "gray50",
                               "High Seas Pockets" = "cadetblue")) +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.99, 0.99),
        legend.justification.inside = c(1, 1),
        legend.background = element_rect(color = "black")) +
  labs(fill = "Legend") +
  scale_x_continuous(expand = c(0, 0), breaks = c(120, 135, 150, 165, 180, -165, -150, -135)) +
  scale_y_continuous(expand = c(0, 0))

p_mounts <- p +
  geom_sf(data = shallow_mounts,
          aes(fill = "Shallow seamounts"),
          pch = 21,
          color = "black") +
  scale_fill_manual(values = c("Non-PNA EEZ" = "gray90",
                               "PNA EEZ" = "gray50",
                               "High Seas Pockets" = "cadetblue",
                               "Shallow seamounts" = "steelblue"))


h_plot_base <- ggplot() +
  geom_sf(data = coast,
          fill = "black",
          color = "black",
          linewidth = 0) +
  geom_sf(data = eezs,
          aes(fill = "Non-PNA EEZ"),
          color = "black") +
  geom_sf(data = PNA_eezs,
          aes(fill = "PNA EEZ"),
          alpha = 0.5,
          color = "black") +
  geom_sf(data = hs_pocket,
          alpha = 0.5,
          aes(fill = "High Seas Pockets"),
          color = "black") +
  scale_fill_manual(values = c("Non-PNA EEZ" = "gray90",
                               "PNA EEZ" = "gray50",
                               "High Seas Pockets" = "cadetblue")) +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.99, 0.99),
        legend.justification.inside = c(1, 1),
        legend.background = element_rect(color = "black"),
        legend.direction = "horizontal") +
  guides(fill = "none") +
  scale_x_continuous(expand = c(0, 0), breaks = c(120, 135, 150, 165, 180, -165, -150, -135)) +
  scale_y_continuous(expand = c(0, 0))

h1_map <- h_plot_base +
  geom_sf(data = h1_cells, aes(color = group)) +
  scale_color_brewer(palette = "Set1", name = "Treatment group")

h2_map <- h_plot_base +
  geom_sf(data = h2_cells, aes(color = group)) +
  scale_color_brewer(palette = "Set1", name = "Treatment group")

h3_ps_map <- h_plot_base +
  geom_sf(data = h3_ps_cells, aes(color = ifelse(near == 1, "near", "far"))) +
  scale_color_brewer(palette = "Set1", name = "Treatment group")

h3_ll_map <- h_plot_base +
  geom_sf(data = h3_ll_cells, aes(color = ifelse(near == 1, "near", "far"))) +
  scale_color_brewer(palette = "Set1", name = "Treatment group")

# EXPORT #######################################################################

## The final step --------------------------------------------------------------
ggsave(plot = p,
       filename = here("content/img/fig_HS_pocket_map.png"),
       width = 6,
       height = 4)

ggsave(plot = p_mounts,
       filename = here("content/img/fig_seamount_density.png"),
       width = 6,
       height = 4)

ggsave(plot = h1_map,
       filename = here("content/img/fig_h1_map.png"),
       width = 6,
       height = 4)
ggsave(plot = h2_map,
       filename = here("content/img/fig_h2_map.png"),
       width = 6,
       height = 4)
ggsave(plot = h3_ps_map,
       filename = here("content/img/fig_h3_ps_map.png"),
       width = 6,
       height = 4)
ggsave(plot = h3_ll_map,
       filename = here("content/img/fig_h3_ll_map.png"),
       width = 6,
       height = 4)

