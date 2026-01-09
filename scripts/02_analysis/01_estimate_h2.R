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
  filter(bet_mt > 0)

# ESTIMATION ###################################################################

## Set fixest defaults ---------------------------------------------------------
setFixest_fml(
  # Outcomes
  ..levels = ~c(cpue_tot, cpue_alb, cpue_bet, cpue_yft),
  ..ihs = ~c(log(cpue_tot), log(cpue_alb), log(cpue_bet), log(cpue_yft)),
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

# Build a single visualization
p <- ggplot(all_es, aes(x = x, y = y, shape = model_type, fill = lhs)) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 2009, linetype = "dashed") +
  geom_linerange(aes(ymin = ci_low, ymax = ci_high),
                 position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 3,
             color = "black") +
  facet_wrap(~lhs, scales = "free_y", ncol = 1) +
  theme_minimal() +
  scale_shape_manual(values = c(21, 22)) +
  scale_fill_manual(values = c("steelblue", "cadetblue")) +
  guides(fill = FALSE,
         shape = guide_legend(
           override.aes = list(shape = c(16, 15))
         )) +
  labs(x = "Year",
       y = "Estimate ± 95% CI",
       shape = "Model type")


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

# ggsave(plot = p,
#        filename = here("content/img/h1_event_study.png"),
#        width = 8, height = 5)
# 
# ggsave(plot = p1,
#        filename = here("content/img/Effort_plot.png"),
#        width = 10, height = 2.5)
# 
# ggsave(plot = p2,
#        filename = here("content/img/Effort_plot_ihs.png"),
#        width = 10, height = 2.5)
# 
# ggsave(plot = p3,
#        filename = here("content/img/Effort_plot_twfe.png"),
#        width = 10, height = 2.5)
# 
# ggsave(plot = p4,
#        filename = here("content/img/Effort_plot_ihs_twfe.png"),
#        width = 10, height = 2.5)

# EXPORT #######################################################################

## The final step --------------------------------------------------------------
