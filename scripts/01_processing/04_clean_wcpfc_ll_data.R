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
ll_tuna_clean <- wcpfc_ll |>
  rename(year = yy,
         lat = lat5,
         lon = lon5,
         alb_mt = alb_c,
         yft_mt = yft_c,
         bet_mt = bet_c) |>
  filter(between(year, 2000, 2020)) |> 
  # Fix coordinates
  mutate(
    lat_mult = ifelse(str_detect(lat, "N"), 1, -1),
    lon_mult = ifelse(str_detect(lon, "E"), 1, -1),
    # WCPFC reports "the latitude of the south-west corner"
    lat = lat_mult * as.numeric(str_remove_all(lat, "[:alpha:]")) + 2.5,
    lon = lon_mult * as.numeric(str_remove_all(lon, "[:alpha:]")) + 2.5) |>
  mutate(tot_mt = alb_mt + yft_mt + bet_mt,
         tot_n = alb_n + yft_n + bet_n) |>
  filter(tot_mt > 0 & tot_n > 0) |>
  select(year, lat, lon, hhooks, contains("_mt"), contains("_n"),
         -c("mls_n", "blm_n", "bum_n", "swo_n", "oth_n")) |>
  complete(year, nesting(lon, lat), fill = list(hhooks = 0,
                                                alb_mt = 0,
                                                yft_mt = 0,
                                                bet_mt = 0,
                                                alb_n = 0,
                                                yft_n = 0,
                                                bet_n = 0)) |>
  mutate(
    cpue_alb_mt = alb_mt / hhooks,
    cpue_yft_mt = yft_mt / hhooks,
    cpue_bet_mt = bet_mt / hhooks,
    cpue_tot_mt = tot_mt / hhooks,
    # CPUE in N / hhooks
    cpue_alb_n = alb_n / hhooks,
    cpue_yft_n = yft_n / hhooks,
    cpue_bet_n = bet_n / hhooks,
    cpue_tot_n = tot_n / hhooks) 


# EXPORT #######################################################################

## The final step --------------------------------------------------------------
write_rds(x = ll_tuna_clean,
          file = here("data/processed/wcpfc_ll_annual.rds"))





