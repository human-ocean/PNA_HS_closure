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
  st_crop(st_buffer(eezs, 1000000))

wcpfc <- read_sf(dsn = here("data/processed/WCPFC_convention_area.gpkg"))
hs_pocket <- read_sf(dsn = here("data/processed/PNA_high_seas_pockets.gpkg"))

coast <- rnaturalearth::ne_countries() |> 
  st_break_antimeridian(lon_0 = 150) |>
  st_transform(crs = "EPSG:8859") |> 
  st_crop(st_buffer(eezs, 1000000))

# VISUALIZE ####################################################################

## Another step ----------------------------------------------------------------
p <- ggplot() + 
  geom_sf(data = coast,
          fill = "black") +
  geom_sf(data = eezs,
          aes(fill = "Non-PNA EEZ"),
          color = "black") +
  geom_sf(data = PNA_eezs,
          aes(fill = "PNA EEZ"),
          color = "black") +
  geom_sf(data = hs_pocket, aes(fill = "High Seas Pockets"), color = "black") +
  scale_fill_manual(values = c("Non-PNA EEZ" = "gray90",
                               "PNA EEZ" = "gray50",
                               "High Seas Pockets" = "red")) +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.99, 0.99),
        legend.justification.inside = c(1, 1),
        legend.background = element_rect(color = "black")) +
  labs(fill = "Legend") +
  scale_x_continuous(expand = c(0, 0), breaks = c(120, 135, 150, 165, 180, -165, -150, -135)) +
  scale_y_continuous(expand = c(0, 0))


# EXPORT #######################################################################

## The final step --------------------------------------------------------------
ggsave(plot = p,
       filename = here("content/img/fig_HS_pocket_map.png"),
       width = 6,
       height = 4)
