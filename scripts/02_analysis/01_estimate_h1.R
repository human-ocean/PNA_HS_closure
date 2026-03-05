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
  cowplot
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

outcomes_levels <- c("Effort (days)", "Effort (sets)")
outcomes_ihs <- c("Effort [asinh(days)]", "Effort [asinh(sets)]")


## Estimate --------------------------------------------------------------------
# Self
## Pre/post regressions
post_lev <- feols(..levels ~ ..post | ..fe,
                  data = data,
                  subset = ~treated ==1,
                  se = "conley") |> 
  set_names(outcomes_levels)

post_ihs <- feols(..ihs ~ ..post | ..fe,
                 data = data,
                 subset = ~treated ==1,
                 se = "conley") |> 
  set_names(outcomes_ihs)

## Event studies
dyn_lev <- feols(..levels ~ ..dyn | ..fe,
                 data = data,
                 subset = ~treated ==1,
                 se = "conley") |> 
  set_names(outcomes_levels)

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
  set_names(outcomes_levels)

post_ihs_twfe <- feols(..ihs ~ ..post_twfe | ..twfe,
                  data = data,
                  se = "conley") |> 
  set_names(outcomes_ihs)

## Event studies
dyn_lev_twfe <- feols(..levels ~ ..dyn_twfe | ..twfe,
                      data = data,
                      se = "conley") |> 
  set_names(outcomes_levels)

dyn_ihs_twfe <- feols(..ihs ~ ..dyn_twfe | ..twfe,
                      data = data,
                      se = "conley") |> 
  set_names(outcomes_ihs)

# VISUALIZE ####################################################################

## Another step ----------------------------------------------------------------
p1 <- ggiplot(dyn_lev,
             multi_style = "facet", 
             facet_args = list(scales = "free_y", ncol = 1)) +
  scale_color_manual(values = c(ps_color, ps_color)) +
  theme(legend.position = "none") +
  labs(x = "Year")

p2 <- ggiplot(dyn_ihs,
             multi_style = "facet", 
             facet_args = list(scales = "free_y", ncol = 1)) +
  scale_color_manual(values = c(ps_color, ps_color)) +
  theme(legend.position = "none") +
  labs(x = "Year")

p3 <- ggiplot(dyn_lev_twfe,
              multi_style = "facet", 
              facet_args = list(scales = "free_y", ncol = 1)) +
  scale_color_manual(values = c(ps_color, ps_color)) +
  theme(legend.position = "none") +
  labs(x = "Year")

p4 <- ggiplot(dyn_ihs_twfe,
              multi_style = "facet", 
              facet_args = list(scales = "free_y", ncol = 1)) +
  scale_color_manual(values = c(ps_color, ps_color)) +
  theme(legend.position = "none") +
  labs(x = "Year")

## Tables ----------------------------------------------------------------------
coef <- c("post" = "Post",
          "post:treated" = "Post x Treated")

se_dist <- str_extract(attr(post_lev_twfe[[1]]$se, "type"), "[:digit:]+km")

# Mean outcomes
mean_days <- mean(data$days[data$post == 0 & data$treated == 1])
mean_sets <- mean(data$num_sets[data$post == 0 & data$treated == 1])

rows <- tribble(~term, ~days, ~sets,
                '$\\bar{Y}_{pre}$', mean_days, mean_sets)

attr(rows, 'position') <- c(3, 1)

notes <- c(note_obs, note_fe,
  paste0("Numbers in parentheses are Conley standard errors with a ", se_dist, " radius."))
notes_main <- c(notes, note_ybar)

# Needs caption
# Needs mean of Y in pre-treatment period
modelsummary(post_lev_twfe,
             title = "\\label{tab:h1}Coefficient estimates for change in fishing effort inside
             the high seas pocket after the closure, relative to changes in fishing effort
             observed for other high seas areas in the WCPFC convention area.",
             stars = tab_stars,
             gof_omit = gof_omit,
             coef_map = coef,
             add_rows = rows,
             notes = notes_main,
             escape = F,
             output = here("content/tab/h1_reg.tex"))

modelsummary(models = list("A) Levels" = post_lev,
                           "B) Inverse-hyperbolic sine transformation" = post_ihs),
             title = "\\label{tab:h1_self}Coefficient estimates for change in fishing effort inside 
             the high seas pocket after the closure.",
             shape = "rbind",
             stars = tab_stars,
             gof_omit = gof_omit,
             coef_map = coef,
             notes = notes,
             escape = F,
             output = here("content/tab/h1_reg_self.tex"))

modelsummary(models = list("A) Levels" = post_lev_twfe,
                           "B) Inverse-hyperbolic sine transformation" = post_ihs_twfe),
             title = "\\label{tab:h1_twfe}Coefficient estimates for change in fishing effort inside
             the high seas pocket after the closure, relative to changes in fishing effort
             observed for other high seas areas in the WCPFC convention area.",
             shape = "rbind",
             stars = tab_stars,
             gof_omit = gof_omit,
             coef_map = coef,
             notes = notes,
             escape = F,
             output = here("content/tab/h1_reg_twfe.tex"))

## Summary stats ---------------------------------------------------------------
write_summary <- function(x, ts = F, append = T) {
  # browser()
  timestamp <- if(ts) {paste("---", Sys.time(), "---\n")} else {""}
  
  cat(paste0(timestamp, x, "\n"),
      file = here("content", "summaries", "h1_summaries.tex"),
      append = append)
}

write_summary("Mean annual effort inside HS pockets\n", ts = T, append = F)

data |> 
  filter(treated == 1,
         post == 0) |> 
  group_by(year) |> 
  summarize(days = sum(days),
            num_sets = sum(num_sets),
            .groups = "drop") |> 
  select(-year) |> 
  summarize_all(c(mean = mean, sd = sd)) |>
  kableExtra::kbl(format = "simple") |>
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
