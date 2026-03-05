################################################################################
# Project-wide configuration
################################################################################
#
# Centralized colors, labels, modelsummary defaults, and graphics parameters.
# Sourced by each analysis script via source(here("scripts/00_config.R")).
# Also sourced by .Rprofile for interactive convenience.
#
################################################################################

# Color schemes ----------------------------------------------------------------
# For gear
ps_color <- "#005C99"
ll_color <- "#E36C4A"

all_gears <- c(ps = ps_color,
               ll = ll_color)

# For species
alb_color <- "#7C6DB0"
bet_color <- "#B33A3A"
skj_color <- "#5A5A5A"
yft_color <- "#D9A441"

all_spp <- c(alb = alb_color,
             bet = bet_color,
             skj = skj_color,
             yft = yft_color,
             all = "black")

# Labels -----------------------------------------------------------------------
spp_labs <- c("alb" = "Albacore",
              "bet" = "Bigeye",
              "skj" = "Skipjack",
              "yft" = "Yellowfin")

# modelsummary defaults --------------------------------------------------------
gof_omit <- "With|IC|RMSE|FE|Std"
tab_stars <- c("*" = 0.1, "**" = 0.05, "***" = 0.01)

# Graphics defaults ------------------------------------------------------------
lw <- 0.3
pt_size <- 2

# Table note building blocks ---------------------------------------------------
note_obs <- "The unit of observation is a grid cell by year."
note_fe  <- "All model specifications include fixed effects by year and grid cell."
note_ybar <- "$\\\\bar{Y}_{pre}$ indicates the mean of each outcome variable in the pre-closure period."
