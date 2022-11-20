library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)
library(broom)

## TODO: 
##   0. Combine pre and post plant seconds elapsed terms into one column earlier.
##   1. Add term for whether bomb is down.
##   2. Possibly try features for number of ARs and SMGs left on each side.
##   3. Try terms for the game (e.g. "Cold War" or "Vanguard") and the map. (mixed-effects?)
##   3. Look into last 10 sec WPs more... shouldn't WP go down in the last 10 seconds or so pre-plant? Unless we know that they're planting.

source('scripts/helpers-plot.R')
source('scripts/helpers-wp.R')

both_pbp <- read_csv('data/cod_snd_pbp.csv', show_col_types = FALSE)

## naive round win rates ----
both_pbp |> 
  # filter(side == 'd') |> 
  # distinct(round_id)
  distinct(side, round_id, win_round = ifelse(team == round_winner, 'w', 'l')) |> 
  count(side, win_round) |> 
  group_by(side) |>
  mutate(
    prop = n / sum(n)
  ) |> 
  ungroup()

round_win_prop_by_xvy <- both_pbp |>
  arrange(round_id, pre_plant_seconds_elapsed, post_plant_seconds_elapsed) |> 
  group_by(round_id, side) |> 
  mutate(
    across(c(n_team_remaining, n_opponent_remaining), list(prev = ~lag(.x, n = 1, default = 4L)))
  ) |> 
  ungroup() |> 
  filter(n_team_remaining_prev > 0L, n_opponent_remaining_prev > 0L) |> 
  count(side, n_team_remaining_prev, n_opponent_remaining_prev, win_round = ifelse(team == round_winner, 'w', 'l')) |> 
  group_by(side, n_team_remaining_prev, n_opponent_remaining_prev) |> 
  mutate(
    prop = n / sum(n)
  ) |> 
  ungroup()

wide_round_win_prop_by_xvy <- round_win_prop_by_xvy |>
  pivot_wider(
    names_from = win_round,
    values_from = c(n, prop),
    values_fill = list(n = 0L, prop = 0)
  )

## model data prep ----
init_model_pbp <- both_pbp |> 
  mutate(
    across(
      c(is_initial_bomb_carrier_killed, is_during_attempted_plant, is_during_attempted_defuse), 
      as.integer
    ),
    opponent_diff = n_team_remaining - n_opponent_remaining,
    win_round = ifelse(team == round_winner, 'yes', 'no') |> factor()
  ) |> 
  arrange(round_id, pre_plant_seconds_elapsed, post_plant_seconds_elapsed) |> 
  group_by(round_id, side) |> 
  mutate(
    prev_opponent_diff = lag(opponent_diff, n = 1, default = 0L)
  ) |> 
  ungroup() |> 
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
    prev_opponent_diff,
    is_initial_bomb_carrier_killed,
    is_during_attempted_plant,
    is_during_attempted_defuse,
    
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

model_xgb <- all_model_pbp |> fit_wp_model_xgb(tune = FALSE)
# augment(model_xgb, all_model_pbp) |> slice_max(wp) |> glimpse()
wp_grid_xgb <- generate_wp_grid(model_xgb)
autoplot(wp_grid_xgb)

ggsave(
  filename = file.path('figs', 'wp_grid_xgb.png'),
  width = 8,
  height = 6
)
