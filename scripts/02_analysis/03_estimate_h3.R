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
  fixest,
  tidyverse,
  modelsummary,
  ggfixest,
  cowplot
)

source(here("scripts/00_config.R"))

## Load data -------------------------------------------------------------------
ps_data <- read_rds(file = here("data/processed/h3_ps_panel.rds")) |> 
  filter(days > 0)

ll_data <- read_rds(file = here("data/processed/h3_ll_panel.rds")) |> 
  filter(hhooks > 0)

# ESTIMATION ###################################################################

## Set fixest defaults ---------------------------------------------------------
setFixest_fml(
  # Outcomes
  # longline
  ..ll_levels = ~c(cpue_tot_n, cpue_tot_mt),
  ..ll_logs = ~c(log(cpue_tot_n), log(cpue_tot_mt)),
  # Base specification
  ..dyn_twfe = ~i(year, near, 2009),
  ..post_twfe = ~post:near,
  # Fixed effects
  ..fe = ~id + year)

setFixest_dict(dict = c("post" = "Post"))

outcomes_ps_levels <- c("CPUE (mt/day)", "CPUE (mt/set)")
outcomes_ps_logs <- c("Effort [log(mt/day)]", "Effort [log(mt/set)]")

outcomes_ll_levels <- c("CPUE (fish/hundred hooks)", "CPUE (mt/hundred hooks)")
outcomes_ll_logs <- c("Effort [log(fish/hundred hooks)]", "Effort [log(mt/hundred hooks)]")

# PROCESSING ###################################################################

## Total CPUE ------------------------------------------------------------------
# Purse seines
# Post regressions
all_ps_levels_post <- list(
  feols(cpue_tot_days ~ ..post_twfe | ..fe, weights = ~days,     data = ps_data, se = "conley"),
  feols(cpue_tot_sets ~ ..post_twfe | ..fe, weights = ~num_sets, data = ps_data, se = "conley")
) |> set_names(outcomes_ps_levels)

all_ps_logs_post <- list(
  feols(log(cpue_tot_days) ~ ..post_twfe | ..fe, weights = ~days,     data = ps_data, se = "conley"),
  feols(log(cpue_tot_sets) ~ ..post_twfe | ..fe, weights = ~num_sets, data = ps_data, se = "conley")
) |> set_names(outcomes_ps_logs)

# Event studies
all_ps_levels_es <- list(
  feols(cpue_tot_days ~ ..dyn_twfe | ..fe, weights = ~days,     data = ps_data, se = "conley"),
  feols(cpue_tot_sets ~ ..dyn_twfe | ..fe, weights = ~num_sets, data = ps_data, se = "conley")
) |> set_names(outcomes_ps_levels)

all_ps_logs_es <- list(
  feols(log(cpue_tot_days) ~ ..dyn_twfe | ..fe, weights = ~days,     data = ps_data, se = "conley"),
  feols(log(cpue_tot_sets) ~ ..dyn_twfe | ..fe, weights = ~num_sets, data = ps_data, se = "conley")
) |> set_names(outcomes_ps_logs)

# Longline
all_ll_levels_post <- feols(..ll_levels ~ ..post_twfe | ..fe,
                            weights = ~hhooks,
                            data = ll_data,
                            se = "conley") |> 
  set_names(outcomes_ll_levels)
all_ll_logs_post <- feols(..ll_logs ~ ..post_twfe | ..fe,
                            weights = ~hhooks,
                            data = ll_data,
                            se = "conley") |>
  set_names(outcomes_ll_logs)


## Species-level regressions ---------------------------------------------------
fit_spp <- function(spp, spec = "post", outcome = "levels", gear = "ps", data){
  # browser()
  # Filter the data inside
  if (gear == "ps") {
    inside_data <- data |> 
      filter(if_any(contains(spp), ~ . > 0)) |> 
      select(id, lon, lat, year, post, near, num_sets, days, contains(spp))
  } else if (gear == "ll") {
    inside_data <- data |> 
      filter(if_any(contains(spp), ~ . > 0)) |> 
      select(id, lon, lat, year, post, near, hhooks, contains(spp))
  }
  
  names <- colnames(inside_data)
  updated_names <- str_replace_all(names, spp, "tot")
  
  names(inside_data) <- updated_names
  
  if (gear == "ps") {
    if (spec == "post") {
      if (outcome == "levels") {
        model <- list(
          feols(cpue_tot_days ~ ..post_twfe | ..fe, weights = ~days,     data = inside_data, se = "conley"),
          feols(cpue_tot_sets ~ ..post_twfe | ..fe, weights = ~num_sets, data = inside_data, se = "conley")
        ) |> set_names(outcomes_ps_levels)
      } else if (outcome == "logs") {
        model <- list(
          feols(log(cpue_tot_days) ~ ..post_twfe | ..fe, weights = ~days,     data = inside_data, se = "conley"),
          feols(log(cpue_tot_sets) ~ ..post_twfe | ..fe, weights = ~num_sets, data = inside_data, se = "conley")
        ) |> set_names(outcomes_ps_logs)
      }
    } else if (spec == "es") {
      if (outcome == "levels") {
        model <- list(
          feols(cpue_tot_days ~ ..dyn_twfe | ..fe, weights = ~days,     data = inside_data, se = "conley"),
          feols(cpue_tot_sets ~ ..dyn_twfe | ..fe, weights = ~num_sets, data = inside_data, se = "conley")
        ) |> set_names(outcomes_ps_levels)
      } else if (outcome == "logs") {
        model <- list(
          feols(log(cpue_tot_days) ~ ..dyn_twfe | ..fe, weights = ~days,     data = inside_data, se = "conley"),
          feols(log(cpue_tot_sets) ~ ..dyn_twfe | ..fe, weights = ~num_sets, data = inside_data, se = "conley")
        ) |> set_names(outcomes_ps_logs)
      }
    }
    } else if (gear == "ll") {
      ##### LONGLINE MODEL FITTING
      if (spec == "post") {
        if (outcome == "levels") {
          model <- feols(..ll_levels ~ ..post_twfe | ..fe,
                         weights = ~hhooks,
                         data = inside_data,
                         se = "conley") |> 
            set_names(outcomes_ll_levels)
        } else if (outcome == "logs") {
          model <- feols(..ll_logs ~ ..post_twfe | ..fe,
                         weights = ~hhooks,
                         data = inside_data,
                         se = "conley") |> 
            set_names(outcomes_ll_logs)
        }
      } else if (spec == "es") {
        if (outcome == "levels") {
          model <- feols(..ll_levels ~ ..dyn_twfe | ..fe,
                         weights = ~hhooks,
                         data = inside_data,
                         se = "conley") |> 
            set_names(outcomes_ll_levels)
        } else if (outcome == "logs") {
          model <- feols(..ll_logs ~ ..dyn_twfe | ..fe,
                         weights = ~hhooks,
                         data = inside_data,
                         se = "conley") |> 
            set_names(outcomes_ll_logs)
        }
      }
    }
  return(model)
}

## For purse seine -------------------------------------------------------------
# 1) Bigeye Tuna
# Pre/post models
bet_ps_levels_post <- fit_spp(spp = "bet", outcome = "levels", data = ps_data)
bet_ps_logs_post <- fit_spp(spp = "bet", outcome = "logs", data = ps_data)

# Event-study models
bet_ps_levels_es <- fit_spp(spp = "bet", spec = "es", outcome = "levels", data = ps_data)
bet_ps_logs_es <- fit_spp(spp = "bet", spec = "es", outcome = "logs", data = ps_data)

# 2) Skijpack models
# Pre/post models
skj_ps_levels_post <- fit_spp(spp = "skj", outcome = "levels", data = ps_data)
skj_ps_logs_post <- fit_spp(spp = "skj", outcome = "logs", data = ps_data)

# Event-study models
skj_ps_levels_es <- fit_spp(spp = "skj", spec = "es", outcome = "levels", data = ps_data)
skj_ps_logs_es <- fit_spp(spp = "skj", spec = "es", outcome = "logs", data = ps_data)

# 3) For Yellowfin Tuna
# Pre/post models
yft_ps_levels_post <- fit_spp(spp = "yft", outcome = "levels", data = ps_data)
yft_ps_logs_post <- fit_spp(spp = "yft", outcome = "logs", data = ps_data)

# Event-study models
yft_ps_levels_es <- fit_spp(spp = "yft", spec = "es", outcome = "levels", data = ps_data)
yft_ps_logs_es <- fit_spp(spp = "yft", spec = "es", outcome = "logs", data = ps_data)


## For longline ----------------------------------------------------------------
# 1) For Albacore
# Pre/post models
alb_ll_levels_post <- fit_spp(spp = "alb", outcome = "levels", gear = "ll", data = ll_data)
alb_ll_logs_post <- fit_spp(spp = "alb", outcome = "logs", gear = "ll", data = ll_data)

# Event-study models
alb_ll_levels_es <- fit_spp(spp = "alb", spec = "es", outcome = "levels", gear = "ll", data = ll_data)
alb_ll_logs_es <- fit_spp(spp = "alb", spec = "es", outcome = "logs", gear = "ll", data = ll_data)


# 2) Bigeye Tuna
# Pre/post models
bet_ll_levels_post <- fit_spp(spp = "bet", outcome = "levels", gear = "ll", data = ll_data)
bet_ll_logs_post <- fit_spp(spp = "bet", outcome = "logs", gear = "ll", data = ll_data)

# Event-study models
bet_ll_levels_es <- fit_spp(spp = "bet", spec = "es", outcome = "levels", gear = "ll", data = ll_data)
bet_ll_logs_es <- fit_spp(spp = "bet", spec = "es", outcome = "logs", gear = "ll", data = ll_data)

# 3) For Yellowfin Tuna
# Pre/post models
yft_ll_levels_post <- fit_spp(spp = "yft", outcome = "levels", gear = "ll", data = ll_data)
yft_ll_logs_post <- fit_spp(spp = "yft", outcome = "logs", gear = "ll", data = ll_data)

# Event-study models
yft_ll_levels_es <- fit_spp(spp = "yft", spec = "es", outcome = "levels", gear = "ll", data = ll_data)
yft_ll_logs_es <- fit_spp(spp = "yft", spec = "es", outcome = "logs", gear = "ll", data = ll_data)


# VISUALIZE ####################################################################

## Tables for the main text ----------------------------------------------------
coef <- c("post" = "Post",
          "post:near" = "Post x Near")

se_dist <- str_extract(attr(skj_ps_levels_post[[1]]$se, "type"), "[:digit:]+km")

# Mean outcomes
skj_data <- ps_data |>
  filter(if_any(contains("skj"), ~ . > 0)) |>
    select(id, lon, lat, year, post, near, num_sets, days, contains("skj"))
mean_days <- mean(skj_data$cpue_skj_days[skj_data$post == 0 & skj_data$near == 1])
mean_sets <- mean(skj_data$cpue_skj_sets[skj_data$post == 0 & skj_data$near == 1])
rows <- tribble(~term, ~fish, ~mt,
                '$\\bar{Y}_{pre}$', mean_days, mean_sets)

attr(rows, 'position') <- c(3, 1)

notes <- c(note_obs, note_fe,
  paste0("Numbers in parentheses are Conley standard errors with a ", se_dist, " radius."))
notes_main <- c(notes, note_ybar)

# Needs caption
# Needs mean of Y in pre-treatment period
modelsummary(skj_ps_levels_post,
             title = "\\label{tab:h3}Coefficient estimates for change in Skipjack tuna CPUE in
             areas within 100 nautical miles of the high seas pocket after the closure, relative to changes in Bigeye tuna CPUE
             observed for areas between 100 and 200 nautical miles and inside PNA nation's Exclusive Economic Zones.",
             stars = tab_stars,
             gof_omit = gof_omit,
             coef_map = coef,
             add_rows = rows,
             notes = notes_main,
             escape = F,
             output = here("content/tab/h3_reg.tex"))

## Main text figures -----------------------------------------------------------
ts_days <- ggplot(data = skj_data,
               mapping = aes(x = year, y = cpue_skj_days)) +
  geom_vline(xintercept = 2009.5,
             linetype = "dashed",
             linewidth = lw) +
  stat_summary(geom = "line", fun = "mean",
               linetype = "dashed",
               color = skj_color) +
  stat_summary(geom = "linerange", 
               fun.data = "mean_cl_normal",
               linewidth = 0.5,
               color = skj_color) +
  stat_summary(geom = "point", fun = "mean",
               size = pt_size,
               color = skj_color) +
  theme_linedraw() +
  theme(legend.position = "none") +
  guides(fill = "none") +
  labs(x = "Year",
       y = "CPUE (mt/day)")

ts_sets <- ggplot(data = skj_data,
               mapping = aes(x = year, y = cpue_skj_sets)) +
  geom_vline(xintercept = 2009.5,
             linetype = "dashed",
             linewidth = lw) +
  stat_summary(geom = "line", fun = "mean",
               linetype = "dashed",
               color = skj_color) +
  stat_summary(geom = "linerange", 
               fun.data = "mean_cl_normal",
               linewidth = 0.5,
               color = skj_color) +
  stat_summary(geom = "point", fun = "mean",
               size = pt_size,
               pch = 17,
               color = skj_color) +
  theme_linedraw() +
  theme(legend.position = "none") +
  guides(fill = "none") +
  labs(x = "Year",
       y = "CPUE (mt/set)")



es_days <- ggiplot(skj_ps_levels_es[[1]],
                geom_style = "ribbon",
                col = skj_color) +
  labs(title = NULL,
       x = "Year",
       y = "Estimate ± 95% CI (mt/day)") +
  theme_linedraw()

es_sets <- ggiplot(skj_ps_levels_es[[2]],
                 geom_style = "ribbon",
                 col = skj_color,
                 pt.pch = 17) +
  labs(title = NULL,
       x = "Year",
       y = "Estimate ± 95% CI (mt/set)") +
  theme_linedraw()

figure <- plot_grid(ts_days, ts_sets,
                    es_days, es_sets,
                    labels = "AUTO")

ggsave(plot = figure,
       filename = here("content/img/h3_main_figure.png"),
       width = 9, height = 6)

# EXPORT #######################################################################


## The final step --------------------------------------------------------------  