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
  ggfixest
)

## Load data -------------------------------------------------------------------
data <- read_rds(file = here("data/processed/h1_panel.rds")) |> 
  mutate(post = 1 * (year > 2009),
         id = paste(lat, lon))

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
dyn_ihs_twfe <- feols(..levels ~ ..dyn_twfe | ..twfe,
                      data = data,
                      se = "conley")


post_lev_twfe <- feols(..levels ~ ..post_twfe | ..twfe,
                  data = data,
                  se = "conley")
post_ihs_twfe <- feols(..ihs ~ ..post_twfe | ..twfe,
                  data = data,
                  se = "conley")

# VISUALIZE ####################################################################

## Another step ----------------------------------------------------------------
p1 <- ggiplot(dyn_lev,
             multi_style = "facet", 
             facet_args = list(scales = "free_y")) +
  theme_minimal() +
  scale_color_manual(values = c("steelblue", "cadetblue")) +
  theme(legend.position = "none") +
  labs(x = "Year")

p2 <- ggiplot(dyn_ihs,
             multi_style = "facet", 
             facet_args = list(scales = "free_y")) +
  theme_minimal() +
  scale_color_manual(values = c("steelblue", "cadetblue")) +
  theme(legend.position = "none") +
  labs(x = "Year")

p3 <- ggiplot(dyn_lev_twfe,
              multi_style = "facet", 
              facet_args = list(scales = "free_y")) +
  theme_minimal() +
  scale_color_manual(values = c("steelblue", "cadetblue")) +
  theme(legend.position = "none") +
  labs(x = "Year")

p4 <- ggiplot(dyn_ihs_twfe,
              multi_style = "facet", 
              facet_args = list(scales = "free_y")) +
  theme_minimal() +
  scale_color_manual(values = c("steelblue", "cadetblue")) +
  theme(legend.position = "none") +
  labs(x = "Year")

modelsummary::modelsummary(list("A) Self" = post_lev,
                                "B) Cont" = post_lev_twfe),
                           shape = "rbind",
                           stars = T,
                           gof_omit = "With|IC|RMSE",
                           coef_map = c("post" = "Post",
                                        "post:treated" = "Post x Treated"),
                           output = "content/tab/reg.tex")

modelsummary::modelsummary(list("A) Self" = post_ihs,
                                "B) Cont" = post_ihs_twfe),
                           shape = "rbind",
                           stars = T,
                           gof_omit = "With|IC|RMSE",
                           coef_map = c("post" = "Post",
                                        "post:treated" = "Post x Treated"),
                           output = "content/tab/reg_ihs.tex")


ggsave(plot = p1,
       filename = here("content/img/Effort_plot.png"),
       width = 10, height = 2.5)

ggsave(plot = p2,
       filename = here("content/img/Effort_plot_ihs.png"),
       width = 10, height = 2.5)

# EXPORT #######################################################################

## The final step --------------------------------------------------------------
  