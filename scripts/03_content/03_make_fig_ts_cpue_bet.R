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
  cowplot
)

## Load data -------------------------------------------------------------------
data <- read_rds(file = here("data/processed/h2_panel.rds"))

# VISUALIZE ####################################################################

## Another step ----------------------------------------------------------------
bet_cpue_ts_1 <- data |> 
  filter(cpue_bet_mt > 0,
         treated == 1) |> 
  ggplot(aes(x = year, y = cpue_bet_mt)) +
  geom_vline(xintercept = 2009.5, linetype = "dashed") +
  geom_smooth(method = "lm", aes(group = post), color = "black", linetype = "dashed") +
  stat_summary(geom = "line", fun = "mean",
               linewidth = 1,
               color = "black") +
  stat_summary(geom = "linerange", 
               fun.data = "mean_cl_normal",
               linewidth = 0.5) +
  stat_summary(geom = "linerange",
               fun.data = "mean_se",
               linewidth = 1.5,
               color = "#d28e00") +
  stat_summary(geom = "point", fun = "mean",
               size = 3,
               shape = 21,
               fill = "#d28e00",
               color = "black") +
  theme_linedraw() +
  theme(legend.position = "none") +
  guides(fill = "none") +
  labs(x = "Year",
       y = "CPUE (mt / hundred hooks) ± SE and 95% CI")

bet_cpue_ts_2 <- data |> 
  filter(cpue_bet_n > 0,
         treated == 1) |> 
  ggplot(aes(x = year, y = cpue_bet_n)) +
  geom_vline(xintercept = 2009.5, linetype = "dashed") +
  geom_smooth(method = "lm", aes(group = post), color = "black", linetype = "dashed") +
  stat_summary(geom = "line", fun = "mean",
               linewidth = 1,
               color = "black") +
  stat_summary(geom = "linerange", 
               fun.data = "mean_cl_normal",
               linewidth = 0.5) +
  stat_summary(geom = "linerange",
               fun.data = "mean_se",
               linewidth = 1.5,
               color = "#d28e00") +
  stat_summary(geom = "point", fun = "mean",
               size = 3,
               shape = 21,
               fill = "#d28e00",
               color = "black") +
  theme_linedraw() +
  theme(legend.position = "none") +
  guides(fill = "none") +
  labs(x = "Year",
       y = "CPUE (fish / hundred hooks) ± SE and 95% CI")

bet_cpue_ts <- plot_grid(bet_cpue_ts_1, bet_cpue_ts_2,
                         ncol = 1)

ggsave(plot = bet_cpue_ts,
       filename = here("content/img/fig_ts_cpue_bet.png"),
       width = 8,
       height = 10)
