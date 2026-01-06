################################################################################
# Build WCPFC Convention area
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
# Jan 6, 2026
#
# This script builds the WCPFC convention area polygon based on the following 
# description:
# The Convention Area is defined in article 3 of the Convention and comprises
# all waters of the Pacific Ocean bounded to the south and to the east by a line
# drawn from the south coast of Australia due south along the 141° meridian of
# east longitude to its intersection with the 55° parallel of south latitude;
# thence due east along the 55° parallel of south latitude to its intersection
# with the 150° meridian of east longitude; thence due south along the 150°
# meridian of east longitude to its intersection with the 60° parallel of south
# latitude; thence due east along the 60° parallel of south latitude to its
# intersection with the 130° meridian of west longitude; thence due north
# along the 130° meridian of west longitude to its intersection with the 4°
# parallel of south latitude; thence due west along the 4° parallel of south
# latitude to its intersection with the 150° meridian of west longitude; thence
# due north along the 150° meridian of west longitude.
#
# The description comes from: http://wcpfc.int/about/convention-and-map
# Last checked on Jan 6, 2026
#
# The above does not specify a northern limit. But the below states it's 60°N:
# https://www.agriculture.gov.au/agriculture-land/fisheries/international/wcfpc
# 
################################################################################
  
# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
pacman::p_load(
  here,
  sf,
  tidyverse
)

# Disable spherical geometries
sf_use_s2(FALSE)

# PROCESSING ###################################################################

# Define coordinartes using a 0–360 longitude system, but crop it at -20 and + 20
poly_raw <- st_sfc(
  st_polygon(
    list(
      matrix(
        c(
          141, -55,
          150, -55,
          150, -60,
          230, -60,   # 230 = 130°W
          230,  -4,
          210,  -4,   # 210 = 150°W
          210, 60,
          141, 60,
          141, -55
        ),
        ncol = 2,
        byrow = TRUE
      )
    )
  ),
  crs = 4326)

# Wrap at the antimeridian
poly_wrapped <- st_wrap_dateline(
  poly_raw,
  options = c("WRAPDATELINE=YES"),
  quiet = TRUE
)

# Make valid and cast, then build sf object
convention_area <- poly_wrapped |> 
  st_cast("MULTIPOLYGON") |>
  st_make_valid() |> 
  st_sf()

# EXPORT #######################################################################
write_sf(obj = convention_area,
         dsn = here("data/processed/WCPFC_convention_area.gpkg"),
         delete_dsn = T)

  