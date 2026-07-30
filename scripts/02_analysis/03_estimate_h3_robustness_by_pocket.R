################################################################################
# H3 Robustness: Spillover effects split by high seas pocket
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jxv893@miami.edu
# 2026-05-20
#
# Robustness check for Hypothesis 3 (spillover effects). Re-estimates the main
# H3 spillover specification for Skipjack tuna caught by the purse seine fleet,
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

# Modelsummary keeps changing the way they build columns
options(modelsummary_factory_latex = "kableExtra")

source(here("scripts/00_config.R"))

## Load data -------------------------------------------------------------------
ps_data <- read_rds(file = here("data/processed/h3_ps_panel.rds")) |>
  filter(days > 0,
         zone %in% c("near", "far")) |>
  # Assign each grid cell to a pocket based on longitude split used in PNA management
  mutate(pocket = ifelse(lon <= 152.5, "HSP1", "HSP2"))

# ESTIMATION ###################################################################

## Set fixest defaults ---------------------------------------------------------
setFixest_fml(
  # Base specification: pre/post interaction with near-zone treatment indicator
  ..dyn_twfe = ~i(year, near, 2009),
  ..post_twfe = ~post:near,
  # Fixed effects: grid cell and year absorb unit-specific and time trends
  ..fe = ~id + year)

setFixest_dict(dict = c("post" = "Post"))

## Prepare Skipjack data -------------------------------------------------------
# Skipjack is the primary commercial target of the purse seine fleet and
# the preregistered main outcome for H3
skj_data <- ps_data |>
  filter(if_any(contains("skj"), ~ . > 0)) |>
  select(id, lon, lat, year, post, near, pocket, num_sets, days,
         cpue_tot_days = cpue_skj_days,
         cpue_tot_sets = cpue_skj_sets)

## Post-treatment models (split by pocket) -------------------------------------
# fsplit = ~pocket fits the same TWFE model separately for each pocket level
# (Full sample, HSP1, HSP2) in a single call, returning a fixest_multi object
skj_days_post <- feols(cpue_tot_days ~ ..post_twfe | ..fe,
                       weights = ~days,
                       fsplit = ~pocket,
                       data = skj_data, se = "conley")

skj_sets_post <- feols(cpue_tot_sets ~ ..post_twfe | ..fe,
                       weights = ~num_sets,
                       fsplit = ~pocket,
                       data = skj_data, se = "conley")

## Event-study models (split by pocket) ----------------------------------------
skj_days_es <- feols(cpue_tot_days ~ ..dyn_twfe | ..fe,
                     weights = ~days,
                     fsplit = ~pocket,
                     data = skj_data, se = "conley")

skj_sets_es <- feols(cpue_tot_sets ~ ..dyn_twfe | ..fe,
                     weights = ~num_sets,
                     fsplit = ~pocket,
                     data = skj_data, se = "conley")

# VISUALIZE ####################################################################

## Regression table ------------------------------------------------------------
coef <- c("post" = "Post",
          "post:near" = "Post x Near")
se_dist <- str_extract(attr(skj_days_post[[1]]$se, "type"), "[:digit:]+km")

notes <- paste(note_obs, note_fe,
               paste0("Numbers in parentheses are Conley standard errors with a ",
                      se_dist, " radius."))

modelsummary(
  list("A) CPUE (mt/day)" = skj_days_post,
       "B) CPUE (mt/set)" = skj_sets_post),
  title = "\\label{tab:h3_rob_pocket_skj_ps}Coefficient estimates for change in
    skipjack tuna CPUE caught by the purse seine fleet in areas within 100
    nautical miles of each high seas pocket after the closure.
    Models are estimated separately for HSP1 (western pocket, lon $\\leq$ 152.5$^{\\circ}$)
    and HSP2 (eastern pocket, lon $>$ 152.5$^{\\circ}$); See \\autoref{fig:map}.
    The first column shows our main text estimates as in \\autoref{tab:h3}.",
  shape = "rbind",
  stars = tab_stars,
  gof_omit = gof_omit,
  coef_map = coef,
  notes = notes,
  escape = F,
  output = here("content", "tab", "h3_rob_pocket_skj_ps.tex")
)
make_small(here("content", "tab", "h3_rob_pocket_skj_ps.tex"))
wrap_notes(here("content", "tab", "h3_rob_pocket_skj_ps.tex"))

# EXPORT #######################################################################

## Event-study figures ---------------------------------------------------------
# Each panel shows dynamic treatment effects by year, faceted by pocket
skj_es_1 <- ggiplot(skj_days_es,
                    multi_style = "facet",
                    geom_style = "ribbon",
                    col = rep(skj_color, 3)) +
  theme_linedraw() +
  theme(legend.position = "none") +
  labs(title = NULL,
       x = "Year",
       y = "Estimate \u00b1 95% CI (mt/day)")

skj_es_2 <- ggiplot(skj_sets_es,
                    multi_style = "facet",
                    geom_style = "ribbon",
                    col = rep(skj_color, 3),
                    pt.pch = 17) +
  theme_linedraw() +
  theme(legend.position = "none") +
  labs(title = NULL,
       x = "Year",
       y = "Estimate \u00b1 95% CI (mt/set)")

figure <- plot_grid(skj_es_1, skj_es_2,
                    labels = "AUTO",
                    ncol = 1)

ggsave(plot = figure,
       filename = here("content", "img", "h3_rob_pocket_skj_ps_es.png"),
       width = 10, height = 6)
