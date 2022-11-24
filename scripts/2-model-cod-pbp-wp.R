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
      won_prior_round_side,
      ~case_when(
        is.na(.x) ~ 0L,
        .x ~ 1L,
        !.x ~ -1L
      )
    ),
    across(
      c(is_initial_bomb_carrier_killed, is_kill_on_attempted_clinch), 
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
    won_prior_round_side,
    
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
model_xgb <- all_model_pbp |> fit_wp_model_xgb(tune = FALSE)

coefs_plot <- autoplot(model_lb, type = 'coefs')
ggsave(
  coefs_plot,
  filename = file.path('figs', 'wp_coefs-lb.png'),
  width = 12,
  height = 8
)

plot_and_save_wp_by_feature <- function(model, method, feature_name) {
  p <- autoplot(model, type = 'grid', feature_name = feature_name)
  f_lab <- switch(
    method,
    'lb' = add_lb_plot_caption,
    'xgb' = add_xgb_plot_caption
  )
  p <- p + f_lab()
  ggsave(
    p,
    filename = file.path('figs', sprintf('wp_grid-%s-%s.png', method, feature_name)),
    width = 12,
    height = 8
  )
  invisible(p)
}

plot_and_save_wp_by_all_discrete_features <- function(model, method) {
  c(
    'won_prior_round_side',
    'prev_team_round_wl_diff',
    'is_kill_on_attempted_clinch',
    'is_initial_bomb_carrier_killed'
  ) |> 
    map(
      ~plot_and_save_wp_by_feature(
        model = model,
        method = method,
        feature_name = .x
      )
    )
}

list(
  'lb' = model_lb,
  'xgb' = model_xgb
) |> 
  iwalk(
    ~plot_and_save_wp_by_all_discrete_features(
      model = .x,
      method = .y
    )
  )
