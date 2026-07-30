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
  tidyverse,
  cowplot
)

## Load data -------------------------------------------------------------------
data <- read_rds(file = here("data/processed/h1_panel.rds"))

# PROCESSING ###################################################################

## Annual catch isnide pockets, by species -------------------------------------
total_inside <- data |> 
  filter(treated == 1,
         post == 0) |> 
  group_by(year) |> 
  summarize(tot_mt = sum(tot_mt, na.rm = T),
            bet = sum(bet_mt, na.rm = T),
            skj = sum(skj_mt, na.rm = T),
            yft = sum(yft_mt, na.rm = T),
            .groups = "drop") |> 
  pivot_longer(cols = c(bet:yft),
               names_to = "spp",
               values_to = "mt")

# VISUALIZE ####################################################################

## Get some numbers for the text
means <- total_inside |> 
  select(spp, mt) |> 
  group_by(spp) |> 
  summarize_all(.funs = c(mean = mean, sd = sd))

## Another step ----------------------------------------------------------------
mean <- ggplot(data = total_inside,
       mapping = aes(x = spp, y = mt, fill = spp)) +
  stat_summary(geom = "col", fun = "mean") +
  stat_summary(geom = "linerange", fun.data = "mean_cl_normal") +
  scale_x_discrete(labels = spp_labs) +
  scale_fill_manual(values = all_spp) +
  theme_linedraw() +
  theme(legend.position = "none") +
  labs(x = "Species",
       y = "Mean catch (mt)")

ts <- ggplot(data = total_inside,
             mapping = aes(x = year, y = mt, color = spp)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2000, 2009, by = 2)) +
  scale_color_manual(values = all_spp,
                     labels = spp_labs) +
  theme_linedraw() +
  theme(legend.position = "inside",
        legend.justification.inside = c(0, 1),
        legend.position.inside = c(0.01, 0.99)) +
  labs(x = "Year",
       y = "Total catch (mt)",
       color = "Species")

plot <- plot_grid(ts, mean, rel_widths = c(2, 1),
                  labels = "AUTO")

# EXPORT #######################################################################

## The final step --------------------------------------------------------------  
ggsave(plot = plot,
       filename = here("content", "img", "fig_ps_catch_inside.png"),
       width = 9,
       height = 3)
