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
data <- read_rds(file = here("data/processed/h2_panel.rds")) |> 
  filter(hhooks > 0)

# ESTIMATION ###################################################################

## Set fixest defaults ---------------------------------------------------------
setFixest_fml(
  # Outcomes
  ..levels_mt = ~c(cpue_tot_mt, cpue_alb_mt, cpue_bet_mt, cpue_yft_mt),
  ..levels_n = ~c(cpue_tot_n, cpue_alb_n, cpue_bet_n, cpue_yft_n),
  ..log_mt = ~c(log(cpue_tot_mt), log(cpue_alb_mt), log(cpue_bet_mt), log(cpue_yft_mt)),
  ..log_n = ~c(log(cpue_tot_n), log(cpue_alb_n), log(cpue_bet_n), log(cpue_yft_n)),
  # Left hand side
  ..dyn = ~i(year, 2009),
  ..dyn_twfe = ~i(year, treated, 2009),
  ..post = ~post,
  ..post_twfe = ~post:treated,
  # Fixed effects
  ..fe = ~id,
  ..twfe = ~id + year)

setFixest_dict(dict = c("post" = "Post"))

spp <- c("All", "Albacore", "Bigeye", "Yellowfin")

## Estimate --------------------------------------------------------------------
# TWFE
dyn_lev_twfe_mt <- feols(..levels_mt ~ ..dyn_twfe | ..twfe,
                      weights = ~hhooks,
                      data = data,
                      se = "conley") |> 
  set_names(spp)
dyn_lev_twfe_n <- feols(..levels_n ~ ..dyn_twfe | ..twfe,
                      weights = ~hhooks,
                      data = data,
                      se = "conley") |> 
  set_names(spp)

dyn_log_twfe_mt <- feols(..log_mt ~ ..dyn_twfe | ..twfe,
                      weights = ~hhooks,
                      data = data,
                      se = "conley") |> 
  set_names(spp)
dyn_log_twfe_n <- feols(..log_n ~ ..dyn_twfe | ..twfe,
                      weights = ~hhooks,
                      data = data,
                      se = "conley") |> 
  set_names(spp)


post_lev_twfe_mt <- feols(..levels_mt ~ ..post_twfe | ..twfe,
                       weights = ~hhooks,
                       data = data,
                       se = "conley") |> 
  set_names(spp)
post_lev_twfe_n <- feols(..levels_n ~ ..post_twfe | ..twfe,
                       weights = ~hhooks,
                       data = data,
                       se = "conley") |> 
  set_names(spp)

post_log_twfe_mt <- feols(..log_mt ~ ..post_twfe | ..twfe,
                       weights = ~hhooks,
                       data = data,
                       se = "conley") |> 
  set_names(spp)
post_log_twfe_n <- feols(..log_n ~ ..post_twfe | ..twfe,
                       weights = ~hhooks,
                       data = data,
                       se = "conley") |> 
  set_names(spp)

# VISUALIZE ####################################################################

## Another step ----------------------------------------------------------------
p1 <- ggiplot(dyn_lev_twfe_mt,
              multi_style = "facet", 
              facet_args = list(scales = "free_y")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Year")
p2 <- ggiplot(dyn_lev_twfe_n,
              multi_style = "facet", 
              facet_args = list(scales = "free_y")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Year")

p3 <- ggiplot(dyn_log_twfe_mt,
              multi_style = "facet", 
              facet_args = list(scales = "free_y")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Year")
p4 <- ggiplot(dyn_log_twfe_n,
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
       filename = here("content/img/h2_plot_levels.png"),
       width = 10, height = 5)

ggsave(plot = p2,
       filename = here("content/img/h2_plot_logs.png"),
       width = 10, height = 5)

# EXPORT #######################################################################

## The final step --------------------------------------------------------------
# Do it one species at a time

bet_model_mt <- feols(c(cpue_bet_mt, log(cpue_bet_mt)) ~ ..dyn_twfe | ..twfe,
                      weights = ~hhooks,
                      data = data |> 
                        filter(cpue_bet_mt > 0),
                      se = "conley")
bet_model_n <- feols(c(cpue_bet_n, log(cpue_bet_n)) ~ ..dyn_twfe | ..twfe,
                     weights = ~hhooks,
                     data = data |> 
                       filter(cpue_bet_n > 0),
                     se = "conley")

bet_model_did_mt <- feols(c(cpue_bet_mt, log(cpue_bet_mt)) ~ ..post_twfe | ..twfe,
                          weights = ~hhooks,
                          data = data |> 
                            filter(cpue_bet_mt > 0),
                          se = "conley")
bet_model_did_n <- feols(c(cpue_bet_n, log(cpue_bet_n)) ~ ..post_twfe | ..twfe,
                         weights = ~hhooks,
                         data = data |> 
                           filter(cpue_bet_n > 0),
                         se = "conley")

bet_mt <- ggiplot(bet_model_mt,
                  multi_style = "facet",
                  facet_args = list(scales = "free_y")) +
  theme(legend.position = "none") +
  labs(x = "Year",
       title = "Effect on Bigeye CPUE (fish / hundred hooks)")

bet_n <- ggiplot(bet_model_n,
                 multi_style = "facet",
                 facet_args = list(scales = "free_y")) +
  theme(legend.position = "none") +
  labs(x = "Year",
       title = "Effect on Bigeye CPUE (fish / hundred hooks)")

all_model <- feols(cpue_tot_mt ~ ..dyn_twfe | ..twfe,
                   weights = ~hhooks,
                   data = data,
                   se = "conley")

all_model_did_mt <- feols(c(cpue_tot_mt, log(cpue_tot_mt)) ~ ..post_twfe | ..twfe,
                          weights = ~hhooks,
                          data = data |> 
                            filter(cpue_tot_mt > 0),
                          se = "conley")
all_model_did_n <- feols(cpue_tot_n ~ ..post_twfe | ..twfe,
                          weights = ~hhooks,
                          data = data |> 
                            filter(cpue_tot_n > 0),
                          se = "conley")

all <- ggiplot(all_model, col = "#c13832") +
  theme(legend.position = "none") +
  labs(x = "Year",
       title = "Effect on all tuna CPUE (fish / hundred hooks)")

bet_and_all <- cowplot::plot_grid(bet, all, ncol = 1)

modelsummary::msummary(list("BET" = bet_model_did_mt,
                            "All" = all_model_did_mt),
                       shape = "rbind",
                       # output = "markdown",
                       stars = T,
                       gof_omit = "With|IC|RMSE|FE",
                       coef_map = c("post" = "Post",
                                    "post:treated" = "Post x Treated"))
                       

ggsave(plot = bet,
        filename = here("content/img/h2_bet_plot_levels.png"),
        width = 10, height = 5)

ggsave(plot = bet_and_all,
       filename = here("content/img/h2_bet_and_all_plot_levels.png"),
       width = 5, height = 5)
