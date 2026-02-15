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
  ggfixest
)

## Load data -------------------------------------------------------------------
data <- read_rds(file = here("data/processed/h2_panel.rds")) |> 
  filter(hhooks > 0)

# ESTIMATION ###################################################################

## Set fixest defaults ---------------------------------------------------------
setFixest_fml(
  # Outcomes
  ..levels = ~c(cpue_tot_n, cpue_tot_mt),
  ..logs = ~c(log(cpue_tot_n), log(cpue_tot_mt)),
  # Base specification
  ..dyn_twfe = ~i(year, treated, 2009),
  ..post_twfe = ~post:treated,
  # Fixed effects
  ..fe = ~id + year)

setFixest_dict(dict = c("post" = "Post"))

outcomes <- c("fish / 100 hooks", "mt / 100 hooks")
spp <- c("Albacore", "Bigeye", "Yellowfin")

## Estimate --------------------------------------------------------------------
# 1) For total CPUE
# Pre/post models
all_levels_post <- feols(..levels ~ ..post_twfe | ..fe,
                         weights = ~hhooks,
                         data = data,
                         se = "conley") |> 
  set_names(outcomes)
all_logs_post <- feols(..logs ~ ..post_twfe | ..fe,
                       weights = ~hhooks,
                       data = data,
                       se = "conley") |> 
  set_names(outcomes)

# Event-study models
all_levels_es <- feols(..levels ~ ..dyn_twfe | ..fe,
                     weights = ~hhooks,
                     data = data,
                     se = "conley") |> 
  set_names(outcomes)
all_logs_es <- feols(..logs ~ ..dyn_twfe | ..fe,
                     weights = ~hhooks,
                     data = data,
                     se = "conley") |> 
  set_names(outcomes)

# Now for each species ---------------------------------------------------------
# Do it one species at a time. This requires that, for each species, we remove observations
# whare a particular column is 0
fit_spp <- function(spp, spec = "post", outcome = "levels", data){
  # Filter the data inside
  inside_data <- data |> 
    filter(if_any(contains(spp), ~ . > 0)) |> 
    select(id, lon, lat, year, post, treated, hhooks, contains(spp))
  
  names <- colnames(inside_data)
  updated_names <- str_replace_all(names, spp, "tot")
  
  names(inside_data) <- updated_names
  
  if (spec == "post") {
    if (outcome == "levels") {
      model <- feols(..levels ~ ..post_twfe | ..fe,
                     weights = ~hhooks,
                     data = inside_data,
                     se = "conley")
    } else if (outcome == "logs") {
      model <- feols(..logs ~ ..post_twfe | ..fe,
                     weights = ~hhooks,
                     data = inside_data,
                     se = "conley")
    }
  } else if (spec == "es") {
    if (outcome == "levels") {
      model <- feols(..levels ~ ..dyn_twfe | ..fe,
                     weights = ~hhooks,
                     data = inside_data,
                     se = "conley")
    } else if (outcome == "logs") {
      model <- feols(..logs ~ ..dyn_twfe | ..fe,
                     weights = ~hhooks,
                     data = inside_data,
                     se = "conley")
    }
  }
  
  model <- model |> 
  set_names(outcomes)
  
  return(model)
}

# 1) For Albacore Tuna
# Pre/post models
alb_levels_post <- fit_spp(spp = "alb", outcome = "levels", data = data)
alb_logs_post <- fit_spp(spp = "alb", outcome = "logs", data = data)
# Event-study models
alb_levels_es <- fit_spp(spp = "alb", spec = "es", outcome = "levels", data = data)
alb_logs_es <- fit_spp(spp = "alb", spec = "es", outcome = "logs", data = data)

# 2) For Bigeye Tuna
# Pre/post models
bet_levels_post <- fit_spp(spp = "bet", outcome = "levels", data = data)
bet_logs_post <- fit_spp(spp = "bet", outcome = "logs", data = data)

# Event-study models
bet_levels_es <- fit_spp(spp = "bet", spec = "es", outcome = "levels", data = data)
bet_logs_es <- fit_spp(spp = "bet", spec = "es", outcome = "logs", data = data)

# 3) For Yellowfin Tuna
# Pre/post models
yft_levels_post <- fit_spp(spp = "yft", outcome = "levels", data = data)
yft_logs_post <- fit_spp(spp = "yft", outcome = "logs", data = data)

# Event-study models
yft_levels_es <- fit_spp(spp = "yft", spec = "es", outcome = "levels", data = data)
yft_logs_es <- fit_spp(spp = "yft", spec = "es", outcome = "logs", data = data)


# VISUALIZE ####################################################################

## Build regression tables -----------------------------------------------------
# Set defaults
gof_omit <- "With|IC|RMSE|FE|SE"
stars <- c("*" = 0.1, "**" = 0.05, "***" = 0.01)

msummary(list("A) Levels" = all_levels_post,
              "B) Logs" = all_logs_post),
         shape = "rbind",
         stars = stars,
         gof_omit = gof_omit)

msummary(list("A) Levels" = alb_levels_post,
              "B) Logs" = alb_logs_post),
         shape = "rbind",
         stars = stars,
         gof_omit = gof_omit)

msummary(list("A) Levels" = bet_levels_post,
              "B) Logs" = bet_logs_post),
         shape = "rbind",
         stars = stars,
         gof_omit = gof_omit)

msummary(list("A) Levels" = yft_levels_post,
              "B) Logs" = yft_logs_post),
         shape = "rbind",
         stars = stars,
         gof_omit = gof_omit)

coef <- list("all_levels" = all_levels_post,
             "all_logs" = all_logs_post,
             "alb_levels" = alb_levels_post,
             "alb_logs" = alb_logs_post,
             "bet_levels" = bet_levels_post,
             "bet_logs" = bet_logs_post,
             "yft_levels" = yft_levels_post,
             "yft_logs" = yft_logs_post) |> 
  map_dfr(ggfixest:::coefplot_data, .id = "src") |> 
  mutate(spp = str_extract(src, "all|alb|bet|yft"),
         outcome = str_extract(src, "levels|logs"))

## Now build plots -------------------------------------------------------------
# Coefficient plots
coefplot_levels <- coef |> 
  filter(outcome == "levels") |> 
  ggplot(aes(x = spp, y = estimate)) + 
  geom_hline(yintercept = 0) +
  geom_linerange(aes(ymin = ci_low,
                     ymax = ci_high)) +
  geom_point() +
  facet_wrap(~id, scales = "free") +
  coord_flip() +
  theme_linedraw()

coefplot_logs <- coef |> 
  filter(outcome == "logs") |> 
  ggplot(aes(x = spp, y = estimate)) + 
  geom_hline(yintercept = 0) +
  geom_linerange(aes(ymin = ci_low,
                     ymax = ci_high)) +
  geom_point() +
  facet_wrap(~id, scales = "free") +
  coord_flip() +
  theme_linedraw()


# Event-study plots
# For all species combined
all_es <- ggiplot(list(all_levels_es, all_logs_es),
                  geom_style = "ribbon",
                  multi_style = "facet", 
                  facet_args = list(scales = "free_y")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "All species",
       x = "Year")

# For albacore  
alb_es <- ggiplot(list(alb_levels_es, alb_logs_es),
                  geom_style = "ribbon",
                  multi_style = "facet", 
              facet_args = list(scales = "free_y")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Albacore",
       x = "Year")

# For bigeye
bet_es <- ggiplot(list(bet_levels_es, bet_logs_es),
                  geom_style = "ribbon",
                  multi_style = "facet", 
                  facet_args = list(scales = "free_y")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Bigeye",
       x = "Year")

# For yellowfin
yft_es <- ggiplot(list(yft_levels_es, yft_logs_es),
                  geom_style = "ribbon",
                  multi_style = "facet", 
                  facet_args = list(scales = "free_y")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Yellowfin",
       x = "Year")

# EXPORT #######################################################################

## The final step --------------------------------------------------------------
es_save <- function(plot, spp){
  
  ggsave(plot = plot,
         filename = here("content/img/", paste0("h2_", spp, "_es.png")),
         width = 14, height = 8)
}


plots <- list(all_es,
              alb_es,
              bet_es,
              yft_es)

walk2(.x = plots,
      .y = c("all", "alb", "bet", "yft"),
      .f = es_save)
