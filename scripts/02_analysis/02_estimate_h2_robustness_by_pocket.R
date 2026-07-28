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
bet_levels_post <- feols(..levels ~ ..post_twfe | ..fe,
                         weights = ~thooks,
                         data = bet_data,
                         se = "conley") |> 
  set_names(outcomes)

bet_levels_post_hsp1 <- feols(..levels ~ ..post_twfe | ..fe,
                              weights = ~thooks,
                              subset = ~(treated == 0 | (treated == 1 & pocket == "HSP1")),
                              data = bet_data,
                              se = "conley") |> 
  set_names(outcomes)

bet_levels_post_hsp2 <- feols(..levels ~ ..post_twfe | ..fe,
                              weights = ~thooks,
                              subset = ~(treated == 0 | (treated == 1 & pocket == "HSP2")),
                              data = bet_data,
                              se = "conley") |> 
  set_names(outcomes)

# BUILD CONTENTS ###############################################################

## Regression tables ------------------------------------------------------------
coef <- c("post" = "Post",
          "post:treated" = "Post x Treated")

se_dist <- str_extract(attr(bet_levels_post[[1]]$se, "type"), "[:digit:]+km")

notes <- paste(note_obs, note_fe,
               paste0("Numbers in parentheses are Conley standard errors with a ", se_dist, " radius."))

modelsummary(
  list("A) CPUE (fish / 1000 hooks)" = c(bet_levels_post[1],
                                         bet_levels_post_hsp1[1],
                                         bet_levels_post_hsp2[1]) |> 
         set_names("sample: Full sample", "sample: HSP1", "sample: HSP2"),
       "B) CPUE (mt / 1000 hooks)" = c(bet_levels_post[2],
                                       bet_levels_post_hsp1[2],
                                       bet_levels_post_hsp2[2]) |> 
         set_names("sample: Full sample", "sample: HSP1", "sample: HSP2")),
  title = "\\label{tab:h2_rob_pocket_bet_ll}Coefficient estimates for change in bigeye tuna CPUE in
  the high seas pockets after the closure, relative to changes in bigeye tuna CPUE
  observed for other tropical (20°S - 20°N) high seas areas in the WCPFC convention area,
  estimated separately for HSP1 (western pocket, lon $\\leq$ 152.5$^{\\circ}$)
  and HSP2 (eastern pocket, lon $>$ 152.5$^{\\circ}$). The first column shows our main
  text estimates as in \\autoref{tab:h2}.",
  shape = "rbind",
  stars = tab_stars,
  gof_omit = gof_omit,
  coef_map = coef,
  notes = notes,
  escape = F,
  output = here("content/tab/h2_rob_pocket_bet_ll.tex"))
make_small(here("content", "tab", "h2_rob_pocket_bet_ll.tex"))
wrap_notes(here("content", "tab", "h2_rob_pocket_bet_ll.tex"))
