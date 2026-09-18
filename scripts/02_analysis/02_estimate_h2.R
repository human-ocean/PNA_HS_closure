################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
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
data <- read_rds(file = here("data/processed/h2_panel.rds")) |> 
  filter(thooks > 0)

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
outcomes_logs <- c("log-fish / 1000 hooks", "log-mt / 1000 hooks")

## Estimate --------------------------------------------------------------------
# 1) For total CPUE
# Pre/post models
all_levels_post <- feols(..levels ~ ..post_twfe | ..fe,
                         weights = ~thooks,
                         data = data,
                         se = "conley") |> 
  set_names(outcomes)
all_logs_post <- feols(..logs ~ ..post_twfe | ..fe,
                       weights = ~thooks,
                       data = data,
                       se = "conley") |> 
  set_names(outcomes)

# Event-study models
all_levels_es <- feols(..levels ~ ..dyn_twfe | ..fe,
                       weights = ~thooks,
                       data = data,
                       se = "conley") |> 
  set_names(outcomes)
all_logs_es <- feols(..logs ~ ..dyn_twfe | ..fe,
                     weights = ~thooks,
                     data = data,
                     se = "conley") |> 
  set_names(outcomes_logs)

# Now for each species ---------------------------------------------------------
# Do it one species at a time. This requires that, for each species, we remove observations
# where a particular column is 0
fit_spp <- function(spp, spec = "post", outcome = "levels", data){
  # Filter the data inside
  inside_data <- data |> 
    filter(if_any(contains(spp), ~ . > 0)) |> 
    select(id, lon, lat, year, post, treated, thooks, contains(spp))
  
  names <- colnames(inside_data)
  updated_names <- str_replace_all(names, spp, "tot")
  
  names(inside_data) <- updated_names
  
  if (spec == "post") {
    if (outcome == "levels") {
      model <- feols(..levels ~ ..post_twfe | ..fe,
                     weights = ~thooks,
                     data = inside_data,
                     se = "conley")
    } else if (outcome == "logs") {
      model <- feols(..logs ~ ..post_twfe | ..fe,
                     weights = ~thooks,
                     data = inside_data,
                     se = "conley")
    }
  } else if (spec == "es") {
    if (outcome == "levels") {
      model <- feols(..levels ~ ..dyn_twfe | ..fe,
                     weights = ~thooks,
                     data = inside_data,
                     se = "conley")
    } else if (outcome == "logs") {
      model <- feols(..logs ~ ..dyn_twfe | ..fe,
                     weights = ~thooks,
                     data = inside_data,
                     se = "conley")
    }
  }
  
  model <- model |> 
    set_names(if (spec == "es" && outcome == "logs") outcomes_logs else outcomes)
  
  return(model)
}

# 1) For Albacore Tuna
# Pre/post models
alb_levels_post <- fit_spp(spp = "alb", outcome = "levels", data = data)
alb_logs_post <- fit_spp(spp = "alb", outcome = "logs", data = data)
# Event-study models
alb_levels_es <- fit_spp(spp = "alb", spec = "es", outcome = "levels", data = data)
alb_logs_es <- fit_spp(spp = "alb", spec = "es", outcome = "logs", data = data)

# 2) For Bigeye Tuna
# Pre/post models
bet_levels_post <- fit_spp(spp = "bet", outcome = "levels", data = data)
bet_logs_post <- fit_spp(spp = "bet", outcome = "logs", data = data)

# Event-study models
bet_levels_es <- fit_spp(spp = "bet", spec = "es", outcome = "levels", data = data)
bet_logs_es <- fit_spp(spp = "bet", spec = "es", outcome = "logs", data = data)

# 3) For Yellowfin Tuna
# Pre/post models
yft_levels_post <- fit_spp(spp = "yft", outcome = "levels", data = data)
yft_logs_post <- fit_spp(spp = "yft", outcome = "logs", data = data)

# Event-study models
yft_levels_es <- fit_spp(spp = "yft", spec = "es", outcome = "levels", data = data)
yft_logs_es <- fit_spp(spp = "yft", spec = "es", outcome = "logs", data = data)


# BUILD CONTENTS ###############################################################
## Tables for the main text ----------------------------------------------------
coef <- c("post" = "Post",
          "post:treated" = "Post x Treated")

se_dist <- str_extract(attr(bet_levels_post[[1]]$se, "vcov_type"), "[:digit:]+km")

# Mean outcomes
mean_n <- mean(data$cpue_bet_n[data$post == 0 & data$treated == 1])
mean_mt <- mean(data$cpue_bet_mt[data$post == 0 & data$treated == 1])
rows <- tribble(~term, ~fish, ~mt,
                '$\\bar{Y}_{pre}$', mean_n, mean_mt)

attr(rows, 'position') <- c(3, 1)

notes <- paste(note_obs, note_fe,
               paste0("Numbers in parentheses are Conley standard errors with a ", se_dist, " radius."))
notes_main <- paste(notes, note_ybar)

# Needs caption
# Needs mean of Y in pre-treatment period
save_table(bet_levels_post,
           small = FALSE,
           title = "\\label{tab:h2}Coefficient estimates for change in bigeye
             tuna CPUE in the longline fleet in the high seas pockets after
             the closure, relative to changes in CPUE observed for other tropical
             (20°S - 20°N) high seas areas in the WCPFC convention area.",
           coef_map = coef,
           add_rows = rows,
           notes = notes_main,
           path = here("content/tab/h2_reg.tex"))

## Supplementary tables
## Build regression tables -----------------------------------------------------
# Set defaults
save_table(list("A) Levels" = all_levels_post,
                "B) Log-transformed" = all_logs_post),
           title = "\\label{tab:h2_reg_all}Coefficient estimates for change in all
             tuna CPUE in the longline fleet in the high seas pockets after
             the closure, relative to changes in CPUE observed for other tropical
             (20°S - 20°N) high seas areas in the WCPFC convention area.
             Panel A presents results in levels. Panel B presents results in
             which the dependent variable is log-transformed.",
           shape = "rbind",
           coef_map = coef,
           notes = notes,
           path = here("content", "tab", "h2_reg_all.tex"))

save_table(list("A) Levels" = alb_levels_post,
                "B) Log-transformed" = alb_logs_post),
           title = "\\label{tab:h2_reg_alb}Coefficient estimates for change in albacore
             tuna CPUE in the longline fleet in the high seas pockets after
             the closure, relative to changes in CPUE observed for other tropical
             (20°S - 20°N) high seas areas in the WCPFC convention area.
             Panel A presents results in levels. Panel B presents results in
             which the dependent variable is log-transformed.",
           shape = "rbind",
           coef_map = coef,
           notes = notes,
           path = here("content", "tab", "h2_reg_alb.tex"))

save_table(list("A) Levels" = bet_levels_post,
                "B) Log-transformed" = bet_logs_post),
           title = "\\label{tab:h2_reg_bet}Coefficient estimates for change in bigeye
             tuna CPUE in the longline fleet in the high seas pockets after
             the closure, relative to changes in CPUE observed for other tropical
             (20°S - 20°N) high seas areas in the WCPFC convention area.
             Panel A presents results in levels (same as in \\autoref{tab:h2}).
             Panel B presents results in
             which the dependent variable is log-transformed.",
           shape = "rbind",
           coef_map = coef,
           notes = notes,
           path = here("content", "tab", "h2_reg_bet.tex"))

save_table(list("A) Levels" = yft_levels_post,
                "B) Log-transformed" = yft_logs_post),
           title = "\\label{tab:h2_reg_yft}Coefficient estimates for change in yellowfin
             tuna CPUE in the longline fleet in the high seas pockets after
             the closure, relative to changes in CPUE observed for other tropical
             (20°S - 20°N) high seas areas in the WCPFC convention area.
             Panel A presents results in levels. Panel B presents results in
             which the dependent variable is log-transformed.",
           shape = "rbind",
           coef_map = coef,
           notes = notes,
           path = here("content", "tab", "h2_reg_yft.tex"))

## Summary stats ---------------------------------------------------------------
write_summary <- function(x, ts = F, append = T) {
  # browser()
  timestamp <- if(ts) {paste("---", Sys.Date(), "---\n")} else {""}
  
  cat(paste0(timestamp, x, "\n"),
      file = here("content", "summaries", "h2_summaries.tex"),
      append = append)
  cat("\n\n",
      file = here("content", "summaries", "h2_summaries.tex"), append = T)
}

write_summary("Notes for H2", ts = T, append = F)

data |> 
  group_by(treated) |> 
  summarize(n = n_distinct(id)) |> 
  kableExtra::kbl(format = "simple",
                  caption = "N per treatment group") |>
  write_summary()

## Plots for main text ---------------------------------------------------------
# This will be a 4-panel figure. Each column is CPUE in different units.
# Top row is raw CPUE time series and bottom row are event-studies
pos <- position_dodge(width = 0.5)

# Time-series of catch (n)
ts_n <- ggplot(data,
               aes(x = year, y = cpue_bet_n, group = treated, linetype = group)) +
  geom_vline(xintercept = 2009.5,
             linetype = "dashed",
             linewidth = lw) +
  stat_summary(geom = "line",
               fun = "mean",
               color = bet_color,
               position = pos) +
  stat_summary(aes(group = treated),
               geom = "linerange", 
               linetype = "solid",
               fun.data = "mean_cl_normal",
               linewidth = 0.5,
               color = bet_color,
               position = pos) +
  stat_summary(aes(group = treated),
               geom = "point",
               fun = "mean",
               size = pt_size,
               color = bet_color,
               position = pos) +
  scale_linetype_manual(values = c("Control" = "dashed",
                                   "Treatment" = "solid")) +
  theme_linedraw() +
  theme(legend.position = "inside",
        legend.position.inside = c(1, 1),
        legend.justification.inside = c(1, 1),
        legend.background = element_blank()) +
  guides(fill = "none") +
  labs(x = "Year",
       y = "CPUE ± 95% CI\n(fish / thousand hooks)",
       linetype = "Treatment")

# Time-series of catch (mt)
ts_mt <- ggplot(data = data,
                aes(x = year, y = cpue_bet_mt, group = treated, linetype = group)) +
  geom_vline(xintercept = 2009.5,
             linetype = "dashed",
             linewidth = lw) +
  stat_summary(geom = "line",
               fun = "mean",
               color = bet_color,
               position = pos) +
  stat_summary(aes(group = treated),
               geom = "linerange", 
               linetype = "solid",
               fun.data = "mean_cl_normal",
               linewidth = 0.5,
               color = bet_color,
               position = pos) +
  stat_summary(aes(group = treated),
               geom = "point",
               fun = "mean",
               size = pt_size,
               color = bet_color,
               position = pos) +
  scale_linetype_manual(values = c("Control" = "dashed",
                                   "Treatment" = "solid")) +
  theme_linedraw() +
  theme(legend.position = "none") +
  guides(fill = "none") +
  labs(x = "Year",
       y = "CPUE ± 95% CI\n(mt / thousand hooks)")

# Add a tuna on top of the mt plot
bet_raster <- as.raster(
  image_read_svg(here::here("data/raw/fish_pics/BET.svg"), width = 500)
)

ts_mt_build <- ggplot_build(ts_mt)
ts_mt_xrange <- ts_mt_build$layout$panel_params[[1]]$x.range
ts_mt_yrange <- ts_mt_build$layout$panel_params[[1]]$y.range
img_w <- 7.5
img_aspect <- nrow(bet_raster) / ncol(bet_raster)
panel_ratio <- 1.56
img_h <- img_w * img_aspect * (diff(ts_mt_yrange) / diff(ts_mt_xrange)) * panel_ratio

ts_mt <- ts_mt +
  annotation_raster(bet_raster,
                    xmin = ts_mt_xrange[2] - img_w,
                    xmax = ts_mt_xrange[2],
                    ymin = ts_mt_yrange[2] - img_h,
                    ymax = ts_mt_yrange[2])

# Now build the event studies
# First for n
es_n <- ggiplot(bet_levels_es[[1]],
                geom_style = "ribbon",
                col = bet_color) +
  labs(title = NULL,
       x = "Year",
       y = "Estimate ± 95% CI\n(fish / thousand hooks)") +
  theme_linedraw()

#Now for metric tons
es_mt <- ggiplot(bet_levels_es[[2]],
                 geom_style = "ribbon",
                 col = bet_color,
                 pt.pch = 17) +
  labs(title = NULL,
       x = "Year",
       y = "Estimate ± 95% CI\n(mt / thousand hooks)") +
  theme_linedraw()

# Put it together
figure <- plot_grid(ts_n, ts_mt,
                    es_n, es_mt,
                    labels = "AUTO")


## Supplementary figures -------------------------------------------------------

# First, a time series of effort (thousand hooks)
ts_hooks <- ggplot(data,
               aes(x = year, y = thooks, group = treated, linetype = group)) +
  geom_vline(xintercept = 2009.5,
             linetype = "dashed",
             linewidth = lw) +
  stat_summary(geom = "line",
               fun = "mean",
               color = bet_color,
               position = pos) +
  stat_summary(aes(group = treated),
               geom = "linerange", 
               linetype = "solid",
               fun.data = "mean_cl_normal",
               linewidth = 0.5,
               color = bet_color,
               position = pos) +
  stat_summary(aes(group = treated),
               geom = "point",
               fun = "mean",
               size = pt_size,
               color = bet_color,
               position = pos) +
  scale_linetype_manual(values = c("Control" = "dashed",
                                   "Treatment" = "solid")) +
  theme_linedraw() +
  theme(legend.position = "inside",
        legend.position.inside = c(1, 1),
        legend.justification.inside = c(1, 1),
        legend.background = element_blank()) +
  guides(fill = "none") +
  labs(x = "Year",
       y = "Effort (housand hooks)",
       linetype = "Treatment")


# Get coefficient estimates
coef <- list("all_levels" = all_levels_post,
             "all_logs" = all_logs_post,
             "alb_levels" = alb_levels_post,
             "alb_logs" = alb_logs_post,
             "bet_levels" = bet_levels_post,
             "bet_logs" = bet_logs_post,
             "yft_levels" = yft_levels_post,
             "yft_logs" = yft_logs_post) |> 
  map_dfr(ggfixest:::coefplot_data, .id = "src") |> 
  mutate(spp = str_extract(src, "all|alb|bet|yft"),
         outcome = str_extract(src, "levels|logs"))

## Now build plots -------------------------------------------------------------
# Coefficient plots
coefplot_levels <- coef |> 
  filter(outcome == "levels") |> 
  ggplot(aes(x = spp, y = estimate, color = spp)) + 
  geom_hline(yintercept = 0) +
  geom_linerange(aes(ymin = ci_low,
                     ymax = ci_high),
                 linewidth = lw) +
  geom_point(size = pt_size) +
  scale_color_manual(values = all_spp) +
  facet_wrap(~id, scales = "free") +
  coord_flip() +
  theme_linedraw()

coefplot_logs <- coef |> 
  filter(outcome == "logs") |> 
  ggplot(aes(x = spp, y = estimate, color = spp)) + 
  geom_hline(yintercept = 0) +
  geom_linerange(aes(ymin = ci_low,
                     ymax = ci_high),
                 linewidth = lw) +
  geom_point(size = pt_size) +
  scale_color_manual(values = all_spp) +
  facet_wrap(~id, scales = "free") +
  coord_flip() +
  theme_linedraw()


# Event-study plots
my_labeller <- labeller(lhs = function(x){str_replace_all(x, ".", "")})

# For all species combined
all_es <- ggiplot(c(all_levels_es, all_logs_es),
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
  labs(title = "All species",
       x = "Year")

# For albacore  
alb_es <- ggiplot(c(alb_levels_es, alb_logs_es),
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
  labs(title = "Albacore",
       x = "Year")

# For bigeye
bet_es <- ggiplot(c(bet_levels_es, bet_logs_es),
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
  labs(title = "Bigeye",
       x = "Year")

# For yellowfin
yft_es <- ggiplot(c(yft_levels_es, yft_logs_es),
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
  labs(title = "Yellowfin",
       x = "Year")

# EXPORT #######################################################################

## Export figures --------------------------------------------------------------
ggsave(plot = figure,
       filename = here("content/img/h2_main_figure.png"),
       width = 9, height = 6)

ggsave(plot = coefplot_levels,
       filename = here("content", "img", "h2_coefplot_levels.png"),
       width = 6,
       height = 4)
ggsave(plot = coefplot_logs,
       filename = here("content", "img", "h2_coefplot_logs.png"),
       width = 6,
       height = 4)

ggsave(plot = ts_hooks,
       filename = here("content/img/h2_effort_ts.png"),
       width = 6, height = 4)


plots <- list(all_es,
              alb_es,
              bet_es,
              yft_es)

walk2(.x = plots,
      .y = c("all", "alb", "bet", "yft"),
      .f = es_save,
      prefix = "h2")
