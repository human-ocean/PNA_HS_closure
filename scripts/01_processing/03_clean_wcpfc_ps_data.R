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
wcpfc <- read_csv(here("data/raw/WCPFC_S_PUBLIC_BY_1x1_MM_5/WCPFC_S_PUBLIC_BY_1x1_MM.CSV"))

# PROCESSING ###################################################################

## Some step -------------------------------------------------------------------
ps_tuna_clean <- wcpfc %>%
  rename(year = yy,
         lat = lat_short,
         lon = lon_short) %>%
  mutate(
    num_sets = sets_una + sets_log + sets_dfad + sets_afad + sets_oth,
    skj_mt = skj_c_una + skj_c_log + skj_c_dfad + skj_c_afad + skj_c_oth,
    yft_mt = yft_c_una + yft_c_log + yft_c_dfad + yft_c_afad + yft_c_oth,
    bet_mt = bet_c_una + bet_c_log + bet_c_dfad + bet_c_afad + bet_c_oth,
  ) %>%
  select(year, lat, lon, days, num_sets, contains("_mt")) %>%
  mutate(tot_mt = skj_mt + yft_mt + bet_mt) %>%
  filter(tot_mt > 0) %>%
  mutate(
    cpue_skj = skj_mt / num_sets,
    cpue_yft = yft_mt / num_sets,
    cpue_bet = bet_mt / num_sets,
    cpue_tot = tot_mt / num_sets
  ) %>% 
  mutate(
    lat_mult = ifelse(str_detect(lat, "N"), 1, -1),
    lon_mult = ifelse(str_detect(lon, "E"), 1, -1)) %>%
  mutate(
    # WCPFC reports "the latitude of the south-west corner"
    lat = lat_mult * as.numeric(str_remove_all(lat, "[:alpha:]")) + 0.5,
    lon = lon_mult * as.numeric(str_remove_all(lon, "[:alpha:]")) + 0.5) %>%
  select(-lat_mult, -lon_mult) |> 
  filter(between(year, 2000, 2020)) |> 
  complete(year, nesting(lon, lat), fill = list(days = 0,
                                                num_sets = 0,
                                                skj_mt = 0,
                                                yft_mt = 0,
                                                bet_mt = 0))


# EXPORT #######################################################################

## The final step --------------------------------------------------------------
write_rds(x = ps_tuna_clean,
         file = here("data/processed/wcpfc_ps_annual.rds"))





