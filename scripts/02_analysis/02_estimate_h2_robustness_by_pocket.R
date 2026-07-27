################################################################################
# H3 Robustness: Spillover effects split by high seas pocket
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jxv893@miami.edu
# 2026-05-20
#
# Robustness check for Hypothesis 2 (rebound effects for Bigeye). Re-estimates
# the main H2 specification for bigeye tuna caught by the longline seine fleet,
# splitting the sample by high seas pocket: HSP1 (lon <= 152.5 degrees) and
# HSP2 (lon > 152.5 degrees). Uses fixest's fsplit argument to produce
# pocket-specific estimates alongside the full-sample model in a single call.
# Outputs a regression table and event-study figures, both exported to the
# content/ directory.
#
################################################################################
  
# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
pacman::p_load(
  here,
  fixest,
  tidyverse,
  modelsummary,
  ggfixest,
  cowplot,
  magick
)

source(here("scripts/00_config.R"))

## Load data -------------------------------------------------------------------
data <- read_rds(file = here("data/processed/h2_panel.rds")) |> 
  filter(thooks > 0) |> 
  mutate(pocket = ifelse(lon <= 152.5, "HSP1", "HSP2"))

# ESTIMATION ###################################################################

## Set fixest defaults ---------------------------------------------------------
setFixest_fml(
  # Outcomes
  ..levels = ~c(cpue_tot_n, cpue_tot_mt),
  ..logs = ~c(log(cpue_tot_n), log(cpue_tot_mt)),
  # Base specification
  ..dyn_twfe = ~i(year, treated, 2009),
  ..post_twfe = ~post:treated,
  # Fixed effects
  ..fe = ~id + year)

setFixest_dict(dict = c("post" = "Post"))

outcomes <- c("fish / 1000 hooks", "mt / 1000 hooks")

## Prepare Bigeye data ---------------------------------------------------------
# Bigeye is the primary commercial target of the longline fleet and
# the preregistered main outcome for H2
bet_data <- data |>
  filter(if_any(contains("bet"), ~ . > 0)) |>
  select(id, lon, lat, year, post, treated, pocket, thooks,
         cpue_tot_n = cpue_bet_n,
         cpue_tot_mt = cpue_bet_mt)
## Post-treatment models (split by pocket) -------------------------------------
bet_levels_post_hsp1 <- feols(..levels ~ ..post_twfe | ..fe,
                         weights = ~thooks,
                         subset = ~(treated == 0 | (treated == 1 & pocket == "HSP1")),
                         data = bet_data,
                         se = "conley")

bet_levels_post_hsp2 <- feols(..levels ~ ..post_twfe | ..fe,
                              weights = ~thooks,
                              subset = ~(treated == 0 | (treated == 1 & pocket == "HSP2")),
                              data = bet_data,
                              se = "conley")

## Event-study models (split by pocket) ----------------------------------------
bet_levels_es_hsp1 <- feols(..levels ~ ..dyn_twfe | ..fe,
                       weights = ~thooks,
                       subset = ~(treated == 0 | (treated == 1 & pocket == "HSP2")),
                       data = bet_data,
                       se = "conley")
bet_levels_es_hsp2 <- feols(..levels ~ ..dyn_twfe | ..fe,
                       weights = ~thooks,
                       subset = ~(treated == 0 | (treated == 1 & pocket == "HSP2")),
                       data = bet_data,
                       se = "conley")
# BUILD CONTENTS ###############################################################

## Regression tables ------------------------------------------------------------
# Build a table where I can compare full sample (left) vs Pocket 2 only (right) 
# for both metrics (top and bottom).

## Event-study plots -----------------------------------------------------------


# EXPORT #######################################################################

## The final step --------------------------------------------------------------
  