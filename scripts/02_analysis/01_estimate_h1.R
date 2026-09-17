################################################################################
# H1 Test: Decrease in fishing effort
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
# July 24, 2026
#
# Description: Tests whether fishing effort within the high seas was eliminated
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
data <- read_rds(file = here("data/processed/h1_panel.rds"))

# ESTIMATION ###################################################################

## Set fixest defaults ---------------------------------------------------------
setFixest_fml(
  # Outcomes
  ..levels = ~c(days, num_sets),
  ..ihs = ~c(asinh(days), asinh(num_sets)),
  # Left hand side
  ..dyn = ~i(year, 2009),
  ..dyn_twfe = ~i(year, treated, 2009),
  ..post = ~post,
  ..post_twfe = ~post:treated,
  # Fixed effects
  ..fe = ~id,
  ..twfe = ~id + year)

setFixest_dict(dict = c(post = "Post"))

# These vectors are used to rename model objects. We only use the ihs version
# on dynamic (event-study) models because that's where we need it for the 
# facets.
outcomes <- c("Effort (days)", "Effort (sets)")
outcomes_ihs <- c("ihs-Effort (days)", "ihs-Effort (sets)")


## Estimate --------------------------------------------------------------------
# Self
## Pre/post regressions
post_lev <- feols(..levels ~ ..post | ..fe,
                  data = data,
                  subset = ~treated ==1,
                  se = "conley") |> 
  set_names(outcomes)

post_ihs <- feols(..ihs ~ ..post | ..fe,
                 data = data,
                 subset = ~treated ==1,
                 se = "conley") |> 
  set_names(outcomes)

## Event studies
dyn_lev <- feols(..levels ~ ..dyn | ..fe,
                 data = data,
                 subset = ~treated ==1,
                 se = "conley") |> 
  set_names(outcomes)

dyn_ihs <- feols(..ihs ~ ..dyn | ..fe,
                 data = data,
                 subset = ~treated ==1,
                 se = "conley") |> 
  set_names(outcomes_ihs)

# TWFE -------------------------------------------------------------------------
## Pre/post regressions
post_lev_twfe <- feols(..levels ~ ..post_twfe | ..twfe,
                  data = data,
                  se = "conley") |> 
  set_names(outcomes)

post_ihs_twfe <- feols(..ihs ~ ..post_twfe | ..twfe,
                  data = data,
                  se = "conley") |> 
  set_names(outcomes)

## Event studies
dyn_lev_twfe <- feols(..levels ~ ..dyn_twfe | ..twfe,
                      data = data,
                      se = "conley") |> 
  set_names(outcomes)

dyn_ihs_twfe <- feols(..ihs ~ ..dyn_twfe | ..twfe,
                      data = data,
                      se = "conley") |> 
  set_names(outcomes_ihs)

# VISUALIZE ####################################################################

## Build event studies ---------------------------------------------------------
my_labeller <- labeller(lhs = function(x){str_replace_all(x, ".", "")})

# Self spec: combine levels and IHS outcomes into one faceted plot
p1 <- ggiplot(c(dyn_lev, dyn_ihs),
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
  labs(x = "Year",
       title = "")

# TWFE spec: combine levels and IHS outcomes into one faceted plot
p2 <- ggiplot(c(dyn_lev_twfe, dyn_ihs_twfe),
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
  labs(x = "Year",
       title = "")

## Tables ----------------------------------------------------------------------
coef <- c("post" = "Post",
          "post:treated" = "Post x Treated")

se_dist <- str_extract(attr(post_lev_twfe[[1]]$se, "vcov_type"), "[:digit:]+km")

# Mean outcomes
mean_days <- mean(data$days[data$post == 0 & data$treated == 1])
mean_sets <- mean(data$num_sets[data$post == 0 & data$treated == 1])

rows <- tribble(~term, ~days, ~sets,
                '$\\bar{Y}_{pre}$', mean_days, mean_sets)

attr(rows, 'position') <- c(3, 1)

notes <- paste(note_obs, note_fe,
  paste0("Numbers in parentheses are Conley standard errors using a ", se_dist, " radius."))
notes_main <- paste(notes, note_ybar)

# Needs caption
# Needs mean of Y in pre-treatment period
save_table(post_lev_twfe,
           small = FALSE,
           title = "\\label{tab:h1}Coefficient estimates for change in fishing effort inside
             the high seas pockets after the closure, relative to changes in fishing effort
             observed for other comparable high seas areas in the WCPFC convention area.",
           coef_map = coef,
           add_rows = rows,
           notes = notes_main,
           path = here("content/tab/h1_reg.tex"))

save_table(models = list("A) Levels" = post_lev,
                         "B) Inverse-hyperbolic sine transformation" = post_ihs),
           title = "\\label{tab:h1_self}Coefficient estimates for change in fishing
             effort inside the high seas pockets after the closure.
             Panel A presents results in levels. Panel B presents results in
             which the dependent variable is transformed using the inverse
             hyperbolic sine (IHS) transformation.",
           shape = "rbind",
           coef_map = coef,
           notes = paste(note_obs, 
                         paste0("Numbers in parentheses are Conley standard errors using a ", se_dist, " radius.")),
           path = here("content/tab/h1_reg_self.tex"))

save_table(models = list("A) Levels" = post_lev_twfe,
                         "B) Inverse-hyperbolic sine transformation" = post_ihs_twfe),
           title = "\\label{tab:h1_twfe}Coefficient estimates for change in fishing
             effort inside the high seas pockets after the closure, relative to changes in
             fishing effort observed in other comparable high seas areas of the WCPFC
             convention area. Panel A presents results in levels (identical to the
             main-text estimates in \\autoref{tab:h1}). Panel B presents results in
             which the dependent variable is transformed using the inverse
             hyperbolic sine (IHS) transformation.",
           shape = "rbind",
           coef_map = coef,
           notes = notes,
           path = here("content/tab/h1_reg_twfe.tex"))

## Summary stats ---------------------------------------------------------------
write_summary <- function(x, ts = F, append = T) {
  # browser()
  timestamp <- if(ts) {paste("---", Sys.Date(), "---\n")} else {""}
  
  cat(paste0(timestamp, x, "\n"),
      file = here("content", "summaries", "h1_summaries.tex"),
      append = append)
  cat("\n\n",
      file = here("content", "summaries", "h1_summaries.tex"), append = T)
}

write_summary("Notes for H1", ts = T, append = F)

data |> 
  filter(treated == 1,
         post == 0) |> 
  group_by(year) |> 
  summarize(days = sum(days),
            num_sets = sum(num_sets),
            .groups = "drop") |> 
  select(-year) |> 
  summarize_all(c(mean = mean, sd = sd)) |>
  kableExtra::kbl(format = "simple",
                  caption = "Mean annual effort inside HS pockets before closure") |>
  write_summary()

data |> 
  group_by(treated) |> 
  summarize(n = n_distinct(id)) |> 
  kableExtra::kbl(format = "simple",
                  caption = "N per treatment group") |>
  write_summary()

## Figures ---------------------------------------------------------------------
# Build figure for paper. Panel figure with the following:
# TS of effort in days and sets for A and B. Then event study in each metric,
# for C and D.

inside_hs <- read_rds(file = here("data/processed/h1_panel.rds")) |> 
  filter(treated == 1)

ts_days <- ggplot(data = inside_hs,
                  aes(x = year, y = days)) +
  geom_vline(xintercept = 2009.5,
             linetype = "dashed",
             linewidth = lw) +
  stat_summary(geom = "line", fun = "sum",
               linetype = "dashed",
               color = ps_color) +
  stat_summary(geom = "point", fun = "sum",
               size = pt_size,
               color = ps_color) +
  theme_linedraw() +
  guides(fill = "none",
         shape = guide_legend(
           override.aes = list(shape = c(16, 15)))) +
  labs(x = "Year",
       y = "Fishing effort (days)")

ts_sets <- ggplot(data = inside_hs,
                  aes(x = year, y = num_sets)) +
  geom_vline(xintercept = 2009.5,
             linetype = "dashed",
             linewidth = lw) +
  stat_summary(geom = "line", fun = "sum",
               linetype = "dashed",
               color = ps_color) +
  stat_summary(geom = "point", fun = "sum",
               shape = 17,
               size = pt_size,
               color = ps_color) +
  theme_linedraw() +
  guides(fill = "none",
         shape = guide_legend(
           override.aes = list(shape = c(16, 15)))) +
  labs(x = "Year",
       y = "Fishing effort (sets)")

ps_raster <- as.raster(
  image_read_svg(here::here("data/raw/fish_pics/Purse seine.svg"), width = 500)
)

# Compute image placement in data coordinates (top-right of panel)
ts_sets_build <- ggplot_build(ts_sets)
ts_sets_xrange <- ts_sets_build$layout$panel_params[[1]]$x.range
ts_sets_yrange <- ts_sets_build$layout$panel_params[[1]]$y.range
img_w <- 7.5
img_aspect <- nrow(ps_raster) / ncol(ps_raster)
# Scale height to preserve aspect ratio, accounting for non-square panel (w/h ≈ 1.56)
panel_ratio <- 1.56
img_h <- img_w * img_aspect * (diff(ts_sets_yrange) / diff(ts_sets_xrange)) * panel_ratio

ts_sets <- ts_sets +
  annotation_raster(ps_raster,
                    xmin = ts_sets_xrange[2] - img_w,
                    xmax = ts_sets_xrange[2],
                    ymin = ts_sets_yrange[2] - img_h,
                    ymax = ts_sets_yrange[2])

es_days <- ggiplot(dyn_lev_twfe[[1]],
                   geom_style = "ribbon",
                   col = ps_color) +
  labs(title = NULL,
       x = "Year",
       y = "Estimate ± 95% CI (days)") +
  theme_linedraw()

es_sets <- ggiplot(dyn_lev_twfe[[2]],
                   geom_style = "ribbon",
                   col = ps_color,
                   pt.pch = 17) +
  labs(title = NULL,
       x = "Year",
       y = "Estimate ± 95% CI (sets)") +
  theme_linedraw()

figure <- plot_grid(ts_days, ts_sets,
                    es_days, es_sets,
                    labels = "AUTO")

# EXPORT #######################################################################

## The final step --------------------------------------------------------------
ggsave(plot = figure,
       filename = here("content/img/h1_main_figure.png"),
       width = 9, height = 6)

plots <- list(p1,
              p2)

walk2(.x = plots,
      .y = c("self", "twfe"),
      .f = es_save,
      prefix = "h1")
