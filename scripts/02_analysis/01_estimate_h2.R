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
data <- read_rds(file = here("data/processed/h2_panel.rds"))

# ESTIMATION ###################################################################

## Set fixest defaults ---------------------------------------------------------
setFixest_fml(
  # Outcomes
  ..levels = ~c(cpue_tot, cpue_alb, cpue_bet, cpue_yft),
  ..log = ~c(log(cpue_tot), log(cpue_alb), log(cpue_bet), log(cpue_yft)),
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
# TWFE
dyn_lev_twfe <- feols(..levels ~ ..dyn_twfe | ..twfe,
                      data = data,
                      se = "conley")
dyn_log_twfe <- feols(..log ~ ..dyn_twfe | ..twfe,
                      data = data,
                      se = "conley")


post_lev_twfe <- feols(..levels ~ ..post_twfe | ..twfe,
                       data = data,
                       se = "conley")
post_log_twfe <- feols(..log ~ ..post_twfe | ..twfe,
                       data = data,
                       se = "conley")

# VISUALIZE ####################################################################

## Another step ----------------------------------------------------------------
p1 <- ggiplot(dyn_lev_twfe,
              multi_style = "facet", 
              facet_args = list(scales = "free_y")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Year")

p2 <- ggiplot(dyn_log_twfe,
              multi_style = "facet", 
              facet_args = list(scales = "free_y")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Year")

modelsummary::modelsummary(list("A) Levels" = post_lev_twfe,
                                "B) Log-transformed" = post_log_twfe),
                           shape = "rbind",
                           stars = T,
                           gof_omit = "With|IC|RMSE|FE",
                           coef_map = c("post" = "Post",
                                        "post:treated" = "Post x Treated"),
                           output = "content/tab/h2_reg.tex")

ggsave(plot = p1,
       filename = here("content/img/h3_plot_levels.png"),
       width = 10, height = 2.5)

ggsave(plot = p2,
       filename = here("content/img/h3_plot_logs.png"),
       width = 10, height = 2.5)

# EXPORT #######################################################################

## The final step --------------------------------------------------------------
