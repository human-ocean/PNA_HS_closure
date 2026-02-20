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
  ggfixest,
  cowplot
)

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

setFixest_dict(dict = c("post" = "Post"))


## Estimate --------------------------------------------------------------------
# Self
dyn_lev <- feols(..levels ~ ..dyn | ..fe,
                 data = data,
                 subset = ~treated ==1,
                 se = "conley")

dyn_ihs <- feols(..ihs ~ ..dyn | ..fe,
                 data = data,
                 subset = ~treated ==1,
                 se = "conley")

post_lev <- feols(..levels ~ ..post | ..fe,
                  data = data,
                  subset = ~treated ==1,
                  se = "conley")
post_ihs <- feols(..ihs ~ ..post | ..fe,
                 data = data,
                 subset = ~treated ==1,
                 se = "conley")

# TWFE
dyn_lev_twfe <- feols(..levels ~ ..dyn_twfe | ..twfe,
                      data = data,
                      se = "conley")
dyn_ihs_twfe <- feols(..ihs ~ ..dyn_twfe | ..twfe,
                      data = data,
                      se = "conley")


post_lev_twfe <- feols(..levels ~ ..post_twfe | ..twfe,
                  data = data,
                  se = "conley")
post_ihs_twfe <- feols(..ihs ~ ..post_twfe | ..twfe,
                  data = data,
                  se = "conley")

all_es <- map_dfr(list("No counterfactual" = dyn_lev,
                       "Counterfactual" = dyn_lev_twfe),
                  iplot_data,
                  .id = "model_type") |> 
  mutate(lhs = ifelse(lhs == "days", "Effort (days)", "Effort (sets)"))

# VISUALIZE ####################################################################

## Another step ----------------------------------------------------------------
p1 <- ggiplot(dyn_lev,
             multi_style = "facet", 
             facet_args = list(scales = "free_y", ncol = 1)) +
  scale_color_manual(values = c("steelblue3", "steelblue4")) +
  theme(legend.position = "none") +
  labs(x = "Year")

p2 <- ggiplot(dyn_ihs,
             multi_style = "facet", 
             facet_args = list(scales = "free_y", ncol = 1)) +
  scale_color_manual(values = c("steelblue3", "steelblue4")) +
  theme(legend.position = "none") +
  labs(x = "Year")

p3 <- ggiplot(dyn_lev_twfe,
              multi_style = "facet", 
              facet_args = list(scales = "free_y", ncol = 1)) +
  scale_color_manual(values = c("steelblue3", "steelblue4")) +
  theme(legend.position = "none") +
  labs(x = "Year")

p4 <- ggiplot(dyn_ihs_twfe,
              multi_style = "facet", 
              facet_args = list(scales = "free_y", ncol = 1)) +
  scale_color_manual(values = c("steelblue3", "steelblue4")) +
  theme(legend.position = "none") +
  labs(x = "Year")

## Tables
gof_omit <- "With|IC|RMSE|FE|SE"
stars <- c("*" = 0.1, "**" = 0.05, "***" = 0.01)

modelsummary::modelsummary(post_lev_twfe,
                           stars = panelsummary:::econ_stars(),
                           gof_omit = "With|IC|RMSE|FE",
                           coef_map = c("post" = "Post",
                                        "post:treated" = "Post x Treated"),
                           output = "content/tab/reg.tex")

modelsummary::modelsummary(list("A) Self" = post_lev,
                                "B) Cont" = post_lev_twfe),
                           shape = "rbind",
                           stars = T,
                           gof_omit = "With|IC|RMSE|FE",
                           coef_map = c("post" = "Post",
                                        "post:treated" = "Post x Treated"),
                           output = "content/tab/reg.tex")

modelsummary::modelsummary(list("A) Self" = post_ihs,
                                "B) Cont" = post_ihs_twfe),
                           shape = "rbind",
                           stars = T,
                           gof_omit = "With|IC|RMSE|FE",
                           coef_map = c("post" = "Post",
                                        "post:treated" = "Post x Treated"),
                           output = "content/tab/reg_ihs.tex")

# Build figure for paper. Panel figure with the following:
# TS of effort in days and sets for A and B. Then event study in each metric,
# for C and D.

inside_hs <- read_rds(file = here("data/processed/h1_panel.rds")) |> 
  filter(treated == 1)

lw <- 0.3
size <- 2

ts_days <- ggplot(data = inside_hs,
                  aes(x = year, y = days)) +
  geom_vline(xintercept = 2009.5,
             linetype = "dashed",
             linewidth = lw) +
  geom_hline(yintercept = 0,
             linewidth = lw) +
  stat_summary(geom = "line", fun = "sum",
               linetype = "dashed",
               color = "steelblue") +
  stat_summary(geom = "point", fun = "sum",
               size = size,
               color = "steelblue") +
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
  geom_hline(yintercept = 0,
             linewidth = lw) +
  stat_summary(geom = "line", fun = "sum",
               linetype = "dashed",
               color = "cadetblue") +
  stat_summary(geom = "point", fun = "sum",
               size = size,
               color = "cadetblue") +
  theme_linedraw() +
  guides(fill = "none",
         shape = guide_legend(
           override.aes = list(shape = c(16, 15)))) +
  labs(x = "Year",
       y = "Fishing effort (sets)")

es_days <- ggiplot(dyn_lev_twfe[[1]],
                   geom_style = "ribbon",
                   col = "steelblue") +
  labs(title = NULL,
       x = "Year",
       y = "Estimate ± 95% CI (days)") +
  theme_linedraw()

es_sets <- ggiplot(dyn_lev_twfe[[2]],
                   geom_style = "ribbon",
                   col = "cadetblue") +
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
