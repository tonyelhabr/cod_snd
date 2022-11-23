library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)
library(broom)

## TODO: 
##   1. Possibly try features for number of ARs and SMGs left on each side.
##   2. Try terms for the game (e.g. "Cold War" or "Vanguard") and the map. (mixed-effects?)

source('scripts/helpers-plot.R')
source('scripts/helpers-wp.R')

both_pbp <- read_csv('data/cod_snd_pbp.csv', show_col_types = FALSE)

## model data prep ----
init_model_pbp <- both_pbp |> 
  mutate(
    across(
      c(is_initial_bomb_carrier_killed, is_kill_on_attempted_clinch, won_prior_round), 
      as.integer
    ),
    win_round = ifelse(team == round_winner, 'yes', 'no') |> factor()
  ) |> 
  select(
    ## ids
    engagement_id,
    round_id,
    
    ## contextual
    pbp_side,
    side, 
    seconds_elapsed,
    plant_second,
    defuse_second,
    round_has_plant,
    pre_plant_seconds_elapsed,
    post_plant_seconds_elapsed,
    
    ## features
    opponent_diff,
    is_initial_bomb_carrier_killed,
    is_kill_on_attempted_clinch,
    # round,
    prev_team_round_wl_diff,
    won_prior_round,
    
    ## outcome
    win_round,
    
    ## extra
    activity,
    is_post_plant,
    opponent_diff,
    killer_player,
    victim_player,
    is_negative_action
  )

## this will have duplicate records at the time of the plant, but that's fine
all_model_pbp <- bind_rows(
  init_model_pbp |> 
    filter(!is.na(pre_plant_seconds_elapsed)) |>
    mutate(is_pre_plant = TRUE, model_seconds_elapsed = pre_plant_seconds_elapsed) |> 
    select(-pre_plant_seconds_elapsed),
  init_model_pbp |> 
    filter(round_has_plant, !is.na(post_plant_seconds_elapsed)) |> 
    mutate(is_pre_plant = FALSE, model_seconds_elapsed = post_plant_seconds_elapsed) |> 
    select(-post_plant_seconds_elapsed)
)
## should have 0 rows
stopifnot(0 == (all_model_pbp |> filter(!is_pre_plant, !is_post_plant) |> distinct(round_id) |> nrow()))
## should only be plant activities
stopifnot(1 == (all_model_pbp |> filter(is_pre_plant, is_post_plant) |> count(activity) |> nrow()))

## model ----
model_lb <- all_model_pbp |> fit_wp_model_lb()
# augment(model_lb, all_model_pbp)
wp_grid_lb <- generate_wp_grid(model_lb)
autoplot(wp_grid_lb)

ggsave(
  filename = file.path('figs', 'wp_grid_lb.png'),
  width = 8,
  height = 6
)

model_xgb <- all_model_pbp |> fit_wp_model_xgb(tune = TRUE)
# augment(model_xgb, all_model_pbp) |> slice_max(wp) |> glimpse()
wp_grid_xgb <- generate_wp_grid(model_xgb)
autoplot(wp_grid_xgb)

ggsave(
  filename = file.path('figs', 'wp_grid_xgb.png'),
  width = 8,
  height = 6
)
