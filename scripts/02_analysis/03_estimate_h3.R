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
  cowplot,
  magick
)

source(here("scripts/00_config.R"))

## Load data -------------------------------------------------------------------
ps_data <- read_rds(file = here("data/processed/h3_ps_panel.rds")) |> 
  filter(days > 0,
         zone %in% c("near", "far"))

ll_data <- read_rds(file = here("data/processed/h3_ll_panel.rds")) |> 
  filter(thooks > 0,
         zone %in% c("near", "far"))

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

outcomes_ps <- c("CPUE (mt/day)", "CPUE (mt/set)")
outcomes_ps_log <- c("log-CPUE (mt/day)", "log-CPUE (mt/set)")

outcomes_ll <- c("CPUE (fish/1000 hooks)", "CPUE (mt/1000 hooks)")
outcomes_ll_log <- c("log-CPUE (fish/1000 hooks)", "log-CPUE (mt/1000 hooks)")

# PROCESSING ###################################################################

## Total CPUE ------------------------------------------------------------------
# Purse seines
# Post regressions
all_ps_levels_post <- list(
  feols(cpue_tot_days ~ ..post_twfe | ..fe, weights = ~days,     data = ps_data, se = "conley"),
  feols(cpue_tot_sets ~ ..post_twfe | ..fe, weights = ~num_sets, data = ps_data, se = "conley")) |> 
  set_names(outcomes_ps)

all_ps_logs_post <- list(
  feols(log(cpue_tot_days) ~ ..post_twfe | ..fe, weights = ~days,     data = ps_data, se = "conley"),
  feols(log(cpue_tot_sets) ~ ..post_twfe | ..fe, weights = ~num_sets, data = ps_data, se = "conley")) |> 
  set_names(outcomes_ps_log)

# Event studies
all_ps_levels_es <- list(
  feols(cpue_tot_days ~ ..dyn_twfe | ..fe, weights = ~days,     data = ps_data, se = "conley"),
  feols(cpue_tot_sets ~ ..dyn_twfe | ..fe, weights = ~num_sets, data = ps_data, se = "conley")) |> 
  set_names(outcomes_ps)

all_ps_logs_es <- list(
  feols(log(cpue_tot_days) ~ ..dyn_twfe | ..fe, weights = ~days,     data = ps_data, se = "conley"),
  feols(log(cpue_tot_sets) ~ ..dyn_twfe | ..fe, weights = ~num_sets, data = ps_data, se = "conley")) |> 
  set_names(outcomes_ps_log)

# Longline
all_ll_levels_post <- feols(..ll_levels ~ ..post_twfe | ..fe,
                            weights = ~thooks,
                            data = ll_data,
                            se = "conley") |> 
  set_names(outcomes_ll)
all_ll_logs_post <- feols(..ll_logs ~ ..post_twfe | ..fe,
                            weights = ~thooks,
                            data = ll_data,
                            se = "conley") |>
  set_names(outcomes_ll)

# Event studies
all_ll_levels_es <- feols(..ll_levels ~ ..dyn_twfe | ..fe,
                          weights = ~thooks,
                          data = ll_data,
                          se = "conley") |>
  set_names(outcomes_ll)
all_ll_logs_es <- feols(..ll_logs ~ ..dyn_twfe | ..fe,
                        weights = ~thooks,
                        data = ll_data,
                        se = "conley") |>
  set_names(outcomes_ll_log)


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
      select(id, lon, lat, year, post, near, thooks, contains(spp))
  }
  
  names <- colnames(inside_data)
  updated_names <- str_replace_all(names, spp, "tot")
  
  names(inside_data) <- updated_names
  
  if (gear == "ps") {
    if (spec == "post") {
      if (outcome == "levels") {
        model <- list(feols(cpue_tot_days ~ ..post_twfe | ..fe, weights = ~days,     data = inside_data, se = "conley"),
                      feols(cpue_tot_sets ~ ..post_twfe | ..fe, weights = ~num_sets, data = inside_data, se = "conley")) |> 
          set_names(outcomes_ps)
      } else if (outcome == "logs") {
        model <- list(feols(log(cpue_tot_days) ~ ..post_twfe | ..fe, weights = ~days,     data = inside_data, se = "conley"),
                      feols(log(cpue_tot_sets) ~ ..post_twfe | ..fe, weights = ~num_sets, data = inside_data, se = "conley")) |> 
          set_names(outcomes_ps)
      }
    } else if (spec == "es") {
      if (outcome == "levels") {
        model <- list(feols(cpue_tot_days ~ ..dyn_twfe | ..fe, weights = ~days,     data = inside_data, se = "conley"),
                      feols(cpue_tot_sets ~ ..dyn_twfe | ..fe, weights = ~num_sets, data = inside_data, se = "conley")) |> 
          set_names(outcomes_ps)
      } else if (outcome == "logs") {
        model <- list(feols(log(cpue_tot_days) ~ ..dyn_twfe | ..fe, weights = ~days,     data = inside_data, se = "conley"),
                      feols(log(cpue_tot_sets) ~ ..dyn_twfe | ..fe, weights = ~num_sets, data = inside_data, se = "conley")) |> 
          set_names(outcomes_ps_log)
      }
    }
    } else if (gear == "ll") {
      ##### LONGLINE MODEL FITTING
      if (spec == "post") {
        if (outcome == "levels") {
          model <- feols(..ll_levels ~ ..post_twfe | ..fe,
                         weights = ~thooks,
                         data = inside_data,
                         se = "conley") |> 
            set_names(outcomes_ll)
        } else if (outcome == "logs") {
          model <- feols(..ll_logs ~ ..post_twfe | ..fe,
                         weights = ~thooks,
                         data = inside_data,
                         se = "conley") |> 
            set_names(outcomes_ll)
        }
      } else if (spec == "es") {
        if (outcome == "levels") {
          model <- feols(..ll_levels ~ ..dyn_twfe | ..fe,
                         weights = ~thooks,
                         data = inside_data,
                         se = "conley") |> 
            set_names(outcomes_ll)
        } else if (outcome == "logs") {
          model <- feols(..ll_logs ~ ..dyn_twfe | ..fe,
                         weights = ~thooks,
                         data = inside_data,
                         se = "conley") |> 
            set_names(outcomes_ll_log)
        }
      }
    }
  return(model)
}

## For purse seine -------------------------------------------------------------
# 1) bigeye Tuna
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

# 3) For yellowfin Tuna
# Pre/post models
yft_ps_levels_post <- fit_spp(spp = "yft", outcome = "levels", data = ps_data)
yft_ps_logs_post <- fit_spp(spp = "yft", outcome = "logs", data = ps_data)
# Event-study models
yft_ps_levels_es <- fit_spp(spp = "yft", spec = "es", outcome = "levels", data = ps_data)
yft_ps_logs_es <- fit_spp(spp = "yft", spec = "es", outcome = "logs", data = ps_data)


## For longline ----------------------------------------------------------------
# 1) For albacore
# Pre/post models
alb_ll_levels_post <- fit_spp(spp = "alb", outcome = "levels", gear = "ll", data = ll_data)
alb_ll_logs_post <- fit_spp(spp = "alb", outcome = "logs", gear = "ll", data = ll_data)
# Event-study models
alb_ll_levels_es <- fit_spp(spp = "alb", spec = "es", outcome = "levels", gear = "ll", data = ll_data)
alb_ll_logs_es <- fit_spp(spp = "alb", spec = "es", outcome = "logs", gear = "ll", data = ll_data)

# 2) bigeye Tuna
# Pre/post models
bet_ll_levels_post <- fit_spp(spp = "bet", outcome = "levels", gear = "ll", data = ll_data)
bet_ll_logs_post <- fit_spp(spp = "bet", outcome = "logs", gear = "ll", data = ll_data)
# Event-study models
bet_ll_levels_es <- fit_spp(spp = "bet", spec = "es", outcome = "levels", gear = "ll", data = ll_data)
bet_ll_logs_es <- fit_spp(spp = "bet", spec = "es", outcome = "logs", gear = "ll", data = ll_data)

# 3) For yellowfin Tuna
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

se_dist <- str_extract(attr(skj_ps_levels_post[[1]]$se, "vcov_type"), "[:digit:]+km")

# Mean outcomes
skj_data <- ps_data |>
  filter(if_any(contains("skj"), ~ . > 0)) |>
    select(id, lon, lat, year, post, near, num_sets, days, contains("skj"))
mean_days <- mean(skj_data$cpue_skj_days[skj_data$post == 0 & skj_data$near == 1])
mean_sets <- mean(skj_data$cpue_skj_sets[skj_data$post == 0 & skj_data$near == 1])
rows <- tribble(~term, ~fish, ~mt,
                '$\\bar{Y}_{pre}$', mean_days, mean_sets)

attr(rows, 'position') <- c(3, 1)

notes <- paste(note_obs, note_fe,
  paste0("Numbers in parentheses are Conley standard errors with a ", se_dist, " radius."))
notes_main <- paste(notes, note_ybar)

# Main-text table
save_table(skj_ps_levels_post,
           small = FALSE,
           title = "\\label{tab:h3}Coefficient estimates for change in skipjack tuna CPUE in the purse seine fleet operating
             in areas within 100 nautical miles of the high seas pockets after the closure, relative to changes in CPUE
             observed for areas between 100 and 200 nautical miles and inside PNA nation's Exclusive Economic Zones.",
           coef_map = coef,
           add_rows = rows,
           notes = notes_main,
           path = here("content/tab/h3_reg.tex"))

## Supplementary tables --------------------------------------------------------
# Purse seine
save_table(list("A) Levels" = all_ps_levels_post,
                "B) Log-transformed" = all_ps_logs_post),
           title = "\\label{tab:h3_reg_all_ps}Coefficient estimates for change in all tuna CPUE in the purse seine fleet in
             areas within 100 nautical miles of the high seas pockets after the closure, relative to changes in CPUE
             observed for areas between 100 and 200 nautical miles and inside PNA nation's Exclusive Economic Zones.
             Panel A presents results in levels. Panel B presents results in which the dependent variable is log-transformed.",
           shape = "rbind",
           coef_map = coef,
           notes = notes,
           path = here("content", "tab", "h3_reg_all_ps.tex"))

save_table(list("A) Levels" = bet_ps_levels_post,
                "B) Log-transformed" = bet_ps_logs_post),
           title = "\\label{tab:h3_reg_bet_ps}Coefficient estimates for change in bigeye tuna CPUE in the purse seine fleet in
             areas within 100 nautical miles of the high seas pockets after the closure, relative to changes in CPUE
             observed for areas between 100 and 200 nautical miles and inside PNA nation's Exclusive Economic Zones.
             Panel A presents results in levels. Panel B presents results in which the dependent variable is log-transformed.",
           shape = "rbind",
           coef_map = coef,
           notes = notes,
           path = here("content", "tab", "h3_reg_bet_ps.tex"))

save_table(list("A) Levels" = skj_ps_levels_post,
                "B) Log-transformed" = skj_ps_logs_post),
           title = "\\label{tab:h3_reg_skj_ps}Coefficient estimates for change in skipjack tuna CPUE in the purse seine fleet in
             areas within 100 nautical miles of the high seas pockets after the closure, relative to changes in CPUE
             observed for areas between 100 and 200 nautical miles and inside PNA nation's Exclusive Economic Zones.
             Panel A presents results in levels. Panel B presents results in which the dependent variable is log-transformed.",
           shape = "rbind",
           coef_map = coef,
           notes = notes,
           path = here("content", "tab", "h3_reg_skj_ps.tex"))

save_table(list("A) Levels" = yft_ps_levels_post,
                "B) Log-transformed" = yft_ps_logs_post),
           title = "\\label{tab:h3_reg_yft_ps}Coefficient estimates for change in yellowfin tuna CPUE in the purse seine fleet in
             areas within 100 nautical miles of the high seas pockets after the closure, relative to changes in CPUE
             observed for areas between 100 and 200 nautical miles and inside PNA nation's Exclusive Economic Zones.
             Panel A presents results in levels. Panel B presents results in which the dependent variable is log-transformed.",
           shape = "rbind",
           coef_map = coef,
           notes = notes,
           path = here("content", "tab", "h3_reg_yft_ps.tex"))

# Longline
save_table(list("A) Levels" = all_ll_levels_post,
                "B) Log-transformed" = all_ll_logs_post),
           title = "\\label{tab:h3_reg_all_ll}Coefficient estimates for change in all tuna CPUE in the longline fleet in
             areas within 100 nautical miles of the high seas pockets after the closure, relative to changes in CPUE
             observed for areas between 100 and 200 nautical miles.
             Panel A presents results in levels. Panel B presents results in which the dependent variable is log-transformed.",
           shape = "rbind",
           coef_map = coef,
           notes = notes,
           path = here("content", "tab", "h3_reg_all_ll.tex"))

save_table(list("A) Levels" = alb_ll_levels_post,
                "B) Log-transformed" = alb_ll_logs_post),
           title = "\\label{tab:h3_reg_alb_ll}Coefficient estimates for change in albacore tuna CPUE in the longline fleet in
             areas within 100 nautical miles of the high seas pockets after the closure, relative to changes in CPUE
             observed for areas between 100 and 200 nautical miles.
             Panel A presents results in levels. Panel B presents results in which the dependent variable is log-transformed.",
           shape = "rbind",
           coef_map = coef,
           notes = notes,
           path = here("content", "tab", "h3_reg_alb_ll.tex"))

save_table(list("A) Levels" = bet_ll_levels_post,
                "B) Log-transformed" = bet_ll_logs_post),
           title = "\\label{tab:h3_reg_bet_ll}Coefficient estimates for change in bigeye tuna CPUE in the longline fleet in
             areas within 100 nautical miles of the high seas pockets after the closure, relative to changes in CPUE
             observed for areas between 100 and 200 nautical miles.
             Panel A presents results in levels. Panel B presents results in which the dependent variable is log-transformed.",
           shape = "rbind",
           coef_map = coef,
           notes = notes,
           path = here("content", "tab", "h3_reg_bet_ll.tex"))

save_table(list("A) Levels" = yft_ll_levels_post,
                "B) Log-transformed" = yft_ll_logs_post),
           title = "\\label{tab:h3_reg_yft_ll}Coefficient estimates for change in yellowfin tuna CPUE in the longline fleet in
             areas within 100 nautical miles of the high seas pockets after the closure, relative to changes in CPUE
             observed for areas between 100 and 200 nautical miles.
             Panel A presents results in levels. Panel B presents results in which the dependent variable is log-transformed.",
           shape = "rbind",
           coef_map = coef,
           notes = notes,
           path = here("content", "tab", "h3_reg_yft_ll.tex"))

## Summary stats ---------------------------------------------------------------
write_summary <- function(x, ts = F, append = T) {
  # browser()
  timestamp <- if(ts) {paste("---", Sys.Date(), "---\n")} else {""}
  
  cat(paste0(timestamp, x, "\n"),
      file = here("content", "summaries", "h3_summaries.tex"),
      append = append)
  cat("\n\n",
      file = here("content", "summaries", "h3_summaries.tex"), append = T)
}

write_summary("Notes for H3", ts = T, append = F)

ps_data |> 
  group_by(near) |> 
  summarize(n = n_distinct(id)) |> 
  kableExtra::kbl(format = "simple",
                  caption = "N per treatment group for purse seine") |>
  write_summary()

ll_data |> 
  group_by(near) |> 
  summarize(n = n_distinct(id)) |> 
  kableExtra::kbl(format = "simple",
                  caption = "N per treatment group for longline") |>
  write_summary()

# FIGURES ######################################################################
## Main text figures -----------------------------------------------------------
pos <- position_dodge(width = 0.5)

skj_data <- skj_data |>
  mutate(group = if_else(near == 1, "Treatment", "Control"))

ts_days <- ggplot(skj_data,
               aes(x = year, y = cpue_skj_days, group = near, linetype = group)) +
  geom_vline(xintercept = 2009.5,
             linetype = "dashed",
             linewidth = lw) +
  stat_summary(geom = "line",
               fun = "mean",
               color = skj_color,
               position = pos) +
  stat_summary(aes(group = near),
               geom = "linerange", 
               linetype = "solid",
               fun.data = "mean_cl_normal",
               linewidth = 0.5,
               color = skj_color,
               position = pos) +
  stat_summary(aes(group = near),
               geom = "point",
               fun = "mean",
               size = pt_size,
               color = skj_color,
               position = pos) +
  scale_linetype_manual(values = c("Control" = "dashed",
                                   "Treatment" = "solid")) +
  theme_linedraw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0, 1),
        legend.justification.inside = c(0, 1),
        legend.background = element_blank()) +
  guides(fill = "none") +
  labs(x = "Year",
       y = "CPUE ± 95% CI (mt/day)",
       linetype = "Treatment")

ts_sets <- ggplot(skj_data,
               aes(x = year, y = cpue_skj_sets, group = near, linetype = group)) +
  geom_vline(xintercept = 2009.5,
             linetype = "dashed",
             linewidth = lw) +
  stat_summary(geom = "line",
               fun = "mean",
               color = skj_color,
               position = pos) +
  stat_summary(aes(group = near),
               geom = "linerange", 
               linetype = "solid",
               fun.data = "mean_cl_normal",
               linewidth = 0.5,
               color = skj_color,
               position = pos) +
  stat_summary(aes(group = near),
               geom = "point",
               fun = "mean",
               size = pt_size,
               pch = 17,
               color = skj_color,
               position = pos) +
  scale_linetype_manual(values = c("Control" = "dashed",
                                   "Treatment" = "solid")) +
  theme_linedraw() +
  theme(legend.position = "none") +
  guides(fill = "none") +
  labs(x = "Year",
       y = "CPUE ± 95% CI (mt/set)")

skj_raster <- as.raster(
  image_read_svg(here::here("data/raw/fish_pics/SKJ.svg"), width = 500)
)

ts_sets_build <- ggplot_build(ts_sets)
ts_sets_xrange <- ts_sets_build$layout$panel_params[[1]]$x.range
ts_sets_yrange <- ts_sets_build$layout$panel_params[[1]]$y.range
img_w <- 7.5
img_aspect <- nrow(skj_raster) / ncol(skj_raster)
panel_ratio <- 1.56
img_h <- img_w * img_aspect * (diff(ts_sets_yrange) / diff(ts_sets_xrange)) * panel_ratio

ts_sets <- ts_sets +
  annotation_raster(skj_raster,
                    xmin = ts_sets_xrange[2] - img_w,
                    xmax = ts_sets_xrange[2],
                    ymin = ts_sets_yrange[2] - img_h,
                    ymax = ts_sets_yrange[2])

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

## Supplementary event-study figures -------------------------------------------
my_labeller <- labeller(lhs = function(x){str_replace_all(x, ".", "")})

# Purse seine
all_ps_es <- ggiplot(c(all_ps_levels_es, all_ps_logs_es),
                     geom_style = "ribbon",
                     multi_style = "facet",
                     col = rep(ps_color, 4),
                     pt.pch = 1,
                     facet_args = list(scales = "free_y",
                                       ncol = 2,
                                       labeller = my_labeller)) +
  theme_linedraw() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(color = "black")) +
  labs(title = "All species (purse seine)",
       x = "Year")

bet_ps_es <- ggiplot(c(bet_ps_levels_es, bet_ps_logs_es),
                     geom_style = "ribbon",
                     multi_style = "facet",
                     col = rep(bet_color, 4),
                     pt.pch = 1,
                     facet_args = list(scales = "free_y",
                                       ncol = 2,
                                       labeller = my_labeller)) +
  theme_linedraw() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(color = "black")) +
  labs(title = "Bigeye (purse seine)",
       x = "Year")

skj_ps_es <- ggiplot(c(skj_ps_levels_es, skj_ps_logs_es),
                     geom_style = "ribbon",
                     multi_style = "facet",
                     col = rep(skj_color, 4),
                     pt.pch = 1,
                     facet_args = list(scales = "free_y",
                                       ncol = 2,
                                       labeller = my_labeller)) +
  theme_linedraw() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(color = "black")) +
  labs(title = "Skipjack (purse seine)",
       x = "Year")

yft_ps_es <- ggiplot(c(yft_ps_levels_es, yft_ps_logs_es),
                     geom_style = "ribbon",
                     multi_style = "facet",
                     col = rep(yft_color, 4),
                     pt.pch = 1,
                     facet_args = list(scales = "free_y",
                                       ncol = 2,
                                       labeller = my_labeller)) +
  theme_linedraw() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(color = "black")) +
  labs(title = "Yellowfin (purse seine)",
       x = "Year")

# Longline
all_ll_es <- ggiplot(c(all_ll_levels_es, all_ll_logs_es),
                     geom_style = "ribbon",
                     multi_style = "facet",
                     col = rep(ll_color, 4),
                     pt.pch = 1,
                     facet_args = list(scales = "free_y",
                                       ncol = 2,
                                       labeller = my_labeller)) +
  theme_linedraw() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(color = "black")) +
  labs(title = "All species (longline)",
       x = "Year")

alb_ll_es <- ggiplot(c(alb_ll_levels_es, alb_ll_logs_es),
                     geom_style = "ribbon",
                     multi_style = "facet",
                     col = rep(alb_color, 4),
                     pt.pch = 1,
                     facet_args = list(scales = "free_y",
                                       ncol = 2,
                                       labeller = my_labeller)) +
  theme_linedraw() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(color = "black")) +
  labs(title = "Albacore (longline)",
       x = "Year")

bet_ll_es <- ggiplot(c(bet_ll_levels_es, bet_ll_logs_es),
                     geom_style = "ribbon",
                     multi_style = "facet",
                     col = rep(bet_color, 4),
                     pt.pch = 1,
                     facet_args = list(scales = "free_y",
                                       ncol = 2,
                                       labeller = my_labeller)) +
  theme_linedraw() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(color = "black")) +
  labs(title = "Bigeye (longline)",
       x = "Year")

yft_ll_es <- ggiplot(c(yft_ll_levels_es, yft_ll_logs_es),
                     geom_style = "ribbon",
                     multi_style = "facet",
                     col = rep(yft_color, 4),
                     pt.pch = 1,
                     facet_args = list(scales = "free_y",
                                       ncol = 2,
                                       labeller = my_labeller)) +
  theme_linedraw() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(color = "black")) +
  labs(title = "Yellowfin (longline)",
       x = "Year")

## Save event-study figures ----------------------------------------------------
plots <- list(all_ps_es, bet_ps_es, skj_ps_es, yft_ps_es,
              all_ll_es, alb_ll_es, bet_ll_es, yft_ll_es)
names <- c("all_ps", "bet_ps", "skj_ps", "yft_ps",
           "all_ll", "alb_ll", "bet_ll", "yft_ll")

walk2(.x = plots, .y = names, .f = es_save, prefix = "h3")