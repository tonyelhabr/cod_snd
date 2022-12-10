library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)
library(broom)
library(qs)

## TODO: 
##   1. Possibly try features for number of ARs and SMGs left on each side.
##   2. Try terms for the game (e.g. "Cold War" or "Vanguard") and the map. (mixed-effects?)

source('scripts/helpers-plot.R')
source('scripts/helpers-wp.R')

both_pbp <- qs::qread('data/cod_snd_pbp.qs')

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
    offense_engagement_id,
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

    ## outcome
    win_round,
    
    ## extra
    game,
    n_team_pre_activity,
    n_opponent_pre_activity,
    n_team_remaining,
    n_opponent_remaining,
    activity,
    is_post_plant,
    opponent_diff,
    activity_player,
    activity_opposer,
    activity_team,
    activity_opponent,
    team,
    opponent,
    is_negative_action
  )

## this will have duplicate records at the time of the plant, but that's fine
all_model_pbp <- bind_rows(
  init_model_pbp |> 
    filter(!is.na(pre_plant_seconds_elapsed)) |>
    mutate(
      is_pre_plant = TRUE, 
      model_seconds_elapsed = pre_plant_seconds_elapsed,
      model_seconds_remaining = 90L - pre_plant_seconds_elapsed
    ) |> 
    select(-pre_plant_seconds_elapsed),
  init_model_pbp |> 
    filter(round_has_plant, !is.na(post_plant_seconds_elapsed)) |> 
    mutate(
      is_pre_plant = FALSE, 
      model_seconds_elapsed = post_plant_seconds_elapsed,
      model_seconds_remaining = 45L - post_plant_seconds_elapsed
    ) |> 
    select(-post_plant_seconds_elapsed)
)
## should have 0 rows
stopifnot(0 == (all_model_pbp |> filter(!is_pre_plant, !is_post_plant) |> distinct(round_id) |> nrow()))
## should only be plant activities
stopifnot(1 == (all_model_pbp |> filter(is_pre_plant, is_post_plant) |> count(activity) |> nrow()))
# all_model_pbp |> filter(is_pre_plant, is_post_plant) |> count(is_initial_bomb_carrier_killed)
# all_model_pbp |> filter(!is_pre_plant, is_post_plant) |> count(is_initial_bomb_carrier_killed)

## TODO:
all_model_pbp |> 
  filter(model_seconds_remaining < 0)

qs::qsave(all_model_pbp, file.path('data', 'wp_model_data.qs'))

d <- all_model_pbp |> 
  filter(side == 'o', !is_pre_plant, n_team_pre_activity == 1, n_team_remaining == 0, n_opponent_remaining == 1)
fit <- glm(win_round ~ model_seconds_elapsed, d, family = 'binomial')
round(predict(fit, tibble(model_seconds_elapsed = c(0:45)), type = 'response'), 2)

all_model_pbp |> 
  filter(side == 'o', !is_pre_plant, n_team_pre_activity == 1, n_team_remaining == 0, n_opponent_remaining == 1) |> 
  count(last_7.5sec = model_seconds_elapsed > 37.5, win_round) |> 
  tail(20)

all_model_pbp |> 
  filter(side == 'o', !is_pre_plant, n_team_pre_activity == 1, n_team_remaining == 0, n_opponent_remaining == 1) |> 
  filter(model_seconds_elapsed > 37.5, win_round == 'no')
  count(last_5sec = model_seconds_elapsed > 40, win_round)

## model ----
model_lb <- all_model_pbp |> fit_wp_model_lb()
qs::qsave(model_lb, file.path('data', 'wp_model-lb.qs'))

coefs_plot <- autoplot(model_lb, type = 'coefs')
ggsave(
  coefs_plot,
  filename = file.path('figs', 'wp_coefs-lb.png'),
  width = 12,
  height = 8
)

model_xgb <- all_model_pbp |> fit_wp_model_xgb(tune = FALSE)
qs::qsave(model_xgb, file.path('data', 'wp_model-xgb.qs'))

plot_and_save_wp_by_feature <- function(model, method, feature_name = NULL) {
  p <- autoplot(model, type = 'grid', feature_name = feature_name)
  f_lab <- switch(
    method,
    'lb' = add_lb_plot_caption,
    'xgb' = add_xgb_plot_caption
  )
  p <- p + f_lab()
  
  sep <- '-'
  if (is.null(feature_name)) {
    sep <- ''
    feature_name <- ''
  }
  ggsave(
    p,
    filename = file.path('figs', sprintf('wp_grid-%s%s%s.png', method, sep, feature_name)),
    width = 12,
    height = 8
  )
  invisible(p)
}

plot_and_save_wp_by_all_discrete_features <- function(model, method) {
  ps <- c(
    'is_kill_on_attempted_clinch',
    'is_initial_bomb_carrier_killed'
  ) |> 
    set_names() |> 
    map(
      ~plot_and_save_wp_by_feature(
        model = model,
        method = method,
        feature_name = .x
      )
    )
  
  agg_p <- plot_and_save_wp_by_feature(
    model = model,
    method = method
  )
  
  append(ps, list('agg' = agg_p))
}

list(
  # 'xgb' = model_xgb,
  'lb' = model_lb
) |> 
  iwalk(
    ~plot_and_save_wp_by_all_discrete_features(
      model = .x,
      method = .y
    )
  )
