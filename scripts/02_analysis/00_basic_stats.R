################################################################################
# title
################################################################################
#
# Your Name Here
# Your email here
# date
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
ll_data <- read_rds(here("data/processed/h2_panel.rds"))
ll_full <- read_rds(here("data/processed/wcpfc_ll_annual.rds"))
ps_data <- read_rds(here("data/processed/h1_panel.rds"))
ps_full <- read_rds(here("data/processed/wcpfc_ps_annual.rds"))

# PROCESSING ###################################################################

# Notes:
# - The WCPFC longline fleet caught ~65,000 tons of BET in 2009
# - Inside the HS pocket, longliners catch ~ 50 tons per pixel per year. Pixels are 5*5 (308025 km2), so around 0.00016 tons / km2 / year
# - Inside HS pockets, longliners caught around 7,000 tons per year.
# 
# - BET mortality was reduced by ~30 tons per pixel per year. That is roughly 0.002 tons / km2 / year
# - BET mortality reduced by ~3,000 tons per year
# - Simultaneously, SKJ mortality was reduced by 400 tons per pixel per year, or 0.03 tons / km2 / year
# - Overall, about 60,000 mt of skipjack tuna are no longer caught by the purse seine fleet in the high seas
# - The purse seine fleet caught about 1,000,000 tons of skj in 2009 as a whole. Catch from the hs pockets was about 6%.

## Longline plots --------------------------------------------------------------
ll_full |> 
  group_by(year) |> 
  summarize(bet_mt = sum(bet_n, na.rm = T)) |> 
  ggplot(aes(x = year, y = bet_mt)) + 
  geom_line() +
  geom_point(x = 2009, aes(y = bet_mt[year == 2009]))

ll_data |> 
  filter(group == "Treatment") |> 
  group_by(year) |> 
  summarize(mt = mean(bet_mt, na.rm = T),
            .groups = "drop") |> 
  ggplot(aes(x = year, y = mt)) + 
  geom_line() +
  labs(title = "Mean BET catch (MT) by longline fleet")

ll_data |> 
  filter(group == "Treatment") |>
  group_by(year) |> 
  summarize(mt = sum(bet_mt, na.rm = T),
            .groups = "drop") |> 
  ggplot(aes(x = year, y = mt)) + 
  geom_line() +
  labs(title = "Total BET catch (MT) by longline fleet")


## Purse seine plots -----------------------------------------------------------
ps_data |> 
  filter(group == "Treatment") |>
  group_by(year) |> 
  summarize(bet = mean(bet_mt, na.rm = T),
            .groups = "drop") |> 
  ggplot(aes(x = year, y = bet)) + 
  geom_line() +
  labs(title = "Mean BET catch (MT) by purse seine fleet")

ps_data |> 
  filter(group == "Treatment") |>
  group_by(year, group) |> 
  summarize(bet = sum(bet_mt, na.rm = T),
            .groups = "drop") |> 
  ggplot(aes(x = year, y = bet)) + 
  geom_line() +
  labs(title = "Total BET catch (MT) by purse seine fleet")

ps_full |> 
  group_by(year) |> 
  summarize(bet_mt = sum(bet_mt, na.rm = T)) |> 
  ggplot(aes(x = year, y = bet_mt)) + 
  geom_line() +
  geom_point(x = 2009, aes(y = bet_mt[year == 2009]))

ps_data |> 
  group_by(year, group) |> 
  summarize(skj = mean(skj_mt, na.rm = T),
            .groups = "drop") |> 
  ggplot(aes(x = year, y = skj, color = group)) + 
  geom_line() +
  labs(title = "Mean SKJ catch (MT) by purse seine fleet")

ps_data |> 
  group_by(year, group) |> 
  summarize(skj = sum(skj_mt, na.rm = T),
            .groups = "drop") |> 
  ggplot(aes(x = year, y = skj, color = group)) + 
  geom_line() +
  labs(title = "Total SKJ catch (MT) by purse seine fleet")

ps_full |> 
  group_by(year) |> 
  summarize(skj_mt = sum(skj_mt, na.rm = T)) |> 
  ggplot(aes(x = year, y = skj_mt)) + 
  geom_line() +
  geom_point(x = 2009, aes(y = skj_mt[year == 2009]))
