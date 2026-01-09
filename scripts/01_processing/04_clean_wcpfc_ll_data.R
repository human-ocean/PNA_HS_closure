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

# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse
)

## Load data -------------------------------------------------------------------
wcpfc_ll <- read_csv(here("data/raw/WCPFC_L_PUBLIC_BY_YR_MON_3/WCPFC_L_PUBLIC_BY_YR_MON.CSV"))

# PROCESSING ###################################################################

## Some step -------------------------------------------------------------------
ll_tuna_clean <- wcpfc_ll %>%
  rename(year = yy,
         lat = lat5,
         lon = lon5,
         alb_mt = alb_c,
         yft_mt = yft_c,
         bet_mt = bet_c) %>%
  select(year, lat, lon, hhooks, contains("_mt")) %>%
  mutate(tot_mt = alb_mt + yft_mt + bet_mt) %>%
  filter(tot_mt > 0) %>%
  mutate(
    cpue_alb = alb_mt / hhooks,
    cpue_yft = yft_mt / hhooks,
    cpue_bet = bet_mt / hhooks,
    cpue_tot = tot_mt / hhooks
  ) %>%
  mutate(
    lat_mult = ifelse(str_detect(lat, "N"), 1, -1),
    lon_mult = ifelse(str_detect(lon, "E"), 1, -1)) %>%
  mutate(
    # WCPFC reports "the latitude of the south-west corner"
    lat = lat_mult * as.numeric(str_remove_all(lat, "[:alpha:]")) + 2.5,
    lon = lon_mult * as.numeric(str_remove_all(lon, "[:alpha:]")) + 2.5) %>%
  select(-lat_mult, -lon_mult) |> 
  filter(between(year, 2000, 2020)) |> 
  complete(year, nesting(lon, lat), fill = list(hhooks = 0,
                                                alb_mt = 0,
                                                yft_mt = 0,
                                                bet_mt = 0))


# EXPORT #######################################################################

## The final step --------------------------------------------------------------
write_rds(x = ll_tuna_clean,
          file = here("data/processed/wcpfc_ll_annual.rds"))





