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
eezs <- read_sf(dsn = here("data/processed/PNA_and_neighbor_eezs.gpkg"))
hs_pocket <- read_sf(dsn = here("data/processed/PNA_high_seas_pockets.gpkg"))

coast <- rnaturalearth::ne_countries() |> 
  filter(iso_a3 %in% unique(eezs$ISO_TER1)) |> 
  st_break_antimeridian(lon_0 = 150) |> 
  st_transform(crs = "EPSG:8859")

# VISUALIZE ####################################################################

## Another step ----------------------------------------------------------------
p <- ggplot() + 
  geom_sf(data = coast, fill = "black") +
  geom_sf(data = eezs, aes(fill = PNA), color = "black") +
  geom_sf(data = hs_pocket, aes(fill = "High Seas Pockets"), color = "black") +
  scale_fill_manual(values = c("High Seas Pockets" = "red",
                               "Party to Nauru Agreement" = "gray50",
                               "Non-Party" = "gray90")) +
  theme_bw() +
  labs(fill = "Legend")


# EXPORT #######################################################################

## The final step --------------------------------------------------------------
ggsave(plot = p,
       filename = here("content/img/fig_HS_pocket_map.png"),
       width = 6,
       height = 3)
