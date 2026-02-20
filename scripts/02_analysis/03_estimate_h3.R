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
  modelsummary,
  ggfixest
)

## Load data -------------------------------------------------------------------
ps_data <- read_rds(file = here("data/processed/h3_ps_panel.rds")) |> 
  filter(days > 0)

ll_data <- read_rds(file = here("data/processed/h3_ll_panel.rds")) |> 
  filter(hhooks > 0)

# ESTIMATION ###################################################################

## Set fixest defaults ---------------------------------------------------------
setFixest_fml(
  # Outcomes
  ..ps_levels = ~c(cpue_tot_days, cpue_tot_sets),
  ..ps_logs = ~c(log(cpue_tot_days), log(cpue_tot_sets)),
  # Base specification
  ..dyn_twfe = ~i(year, near, 2009),
  ..post_twfe = ~post:near,
  # Fixed effects
  ..fe = ~id + year)

setFixest_dict(dict = c("post" = "Post"))


# PROCESSING ###################################################################

## Total CPUE ------------------------------------------------------------------
# Post regressions
feols(..ps_levels ~ ..post_twfe | ..fe,
      weights = ~num_sets,
      data = ps_data,
      se = "conley")
feols(..ps_logs ~ ..post_twfe | ..fe,
      weights = ~num_sets,
      data = ps_data,
      se = "conley")

# Event studies
feols(..ps_levels ~ ..dyn_twfe | ..fe,
      weights = ~num_sets,
      data = ps_data,
      se = "conley") |> 
  ggiplot(multi_style = "facet",
          geom_style = "ribbon")

feols(..ps_logs ~ ..dyn_twfe | ..fe,
      weights = ~num_sets,
      data = ps_data,
      se = "conley") |> 
  ggiplot(multi_style = "facet",
          geom_style = "ribbon")

## Species-level regressions ---------------------------------------------------
fit_spp <- function(spp, spec = "post", outcome = "levels", data){
  # browser()
  # Filter the data inside
  inside_data <- data |> 
    filter(if_any(contains(spp), ~ . > 0)) |> 
    select(id, lon, lat, year, post, near, num_sets, days, contains(spp))
  
  names <- colnames(inside_data)
  updated_names <- str_replace_all(names, spp, "tot")
  
  names(inside_data) <- updated_names
  
  if (spec == "post") {
    if (outcome == "levels") {
      model <- feols(..ps_levels ~ ..post_twfe | ..fe,
                     weights = ~num_sets,
                     data = inside_data,
                     se = "conley")
    } else if (outcome == "logs") {
      model <- feols(..ps_logs ~ ..post_twfe | ..fe,
                     weights = ~num_sets,
                     data = inside_data,
                     se = "conley")
    }
  } else if (spec == "es") {
    if (outcome == "levels") {
      model <- feols(..ps_levels ~ ..dyn_twfe | ..fe,
                     weights = ~num_sets,
                     data = inside_data,
                     se = "conley")
    } else if (outcome == "logs") {
      model <- feols(..ps_logs ~ ..dyn_twfe | ..fe,
                     weights = ~num_sets,
                     data = inside_data,
                     se = "conley")
    }
  }
  
  
  return(model)
}

# 1) Bigeye Tuna
# Pre/post models
bet_levels_post <- fit_spp(spp = "bet", outcome = "levels", data = ps_data)
bet_logs_post <- fit_spp(spp = "bet", outcome = "logs", data = ps_data)

# Event-study models
bet_levels_es <- fit_spp(spp = "bet", spec = "es", outcome = "levels", data = ps_data)
bet_logs_es <- fit_spp(spp = "bet", spec = "es", outcome = "logs", data = ps_data)

# 2) Skijpack models
# Pre/post models
skj_levels_post <- fit_spp(spp = "skj", outcome = "levels", data = ps_data)
skj_logs_post <- fit_spp(spp = "skj", outcome = "logs", data = ps_data)

# Event-study models
skj_levels_es <- fit_spp(spp = "skj", spec = "es", outcome = "levels", data = ps_data)
skj_logs_es <- fit_spp(spp = "skj", spec = "es", outcome = "logs", data = ps_data)

# 3) For Yellowfin Tuna
# Pre/post models
yft_levels_post <- fit_spp(spp = "yft", outcome = "levels", data = ps_data)
yft_logs_post <- fit_spp(spp = "yft", outcome = "logs", data = ps_data)

# Event-study models
yft_levels_es <- fit_spp(spp = "yft", spec = "es", outcome = "levels", data = ps_data)
yft_logs_es <- fit_spp(spp = "yft", spec = "es", outcome = "logs", data = ps_data)



# VISUALIZE ####################################################################

## Another step ----------------------------------------------------------------
ggplot(ps_data,
       aes(x = post, y = cpue_tot_days,
           color = near == 1, group = near)) + 
  stat_summary(geom = "pointrange",
               fun.data = "mean_se")

# ANALYSIS #####################################################################

## Almost last step ------------------------------------------------------------


# EXPORT #######################################################################


## The final step --------------------------------------------------------------  