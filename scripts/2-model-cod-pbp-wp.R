library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)
library(broom)
library(qs)
pkgload::load_all('../snd')

## TODO: 
##   1. Possibly try features for number of ARs and SMGs left on each side.
##   2. Try terms for the game (e.g. "Cold War" or "Vanguard") and the map. (mixed-effects?)


both_pbp <- qs::qread('data/cod_snd_pbp.qs')

## model data prep ----
init_model_pbp <- both_pbp |> 
  mutate(
    is_offense = side == 'o',
    across(
      won_prior_round_side,
      ~case_when(
        is.na(.x) ~ 0L,
        .x ~ 1L,
        !.x ~ -1L
      )
    ),
    across(
      c(
        is_offense,
        is_initial_bomb_carrier_killed, 
        is_kill_on_attempted_clinch,
        has_started_clinch
      ), 
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
    is_offense,
    is_initial_bomb_carrier_killed,
    is_kill_on_attempted_clinch,
    has_started_clinch,
    
    ## outcome
    win_round,
    
    ## extra
    game,
    team_round_wins,
    opponent_round_wins,
    
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
) |> 
  add_hardcoded_wp_cols()

## should have 0 rows
stopifnot(0 == (all_model_pbp |> filter(!is_pre_plant, !is_post_plant) |> distinct(round_id) |> nrow()))
## should only be plant activities
stopifnot(1 == (all_model_pbp |> filter(is_pre_plant, is_post_plant) |> count(activity) |> nrow()))
# all_model_pbp |> filter(is_pre_plant, is_post_plant) |> count(is_initial_bomb_carrier_killed)
# all_model_pbp |> filter(!is_pre_plant, is_post_plant) |> count(is_initial_bomb_carrier_killed)

## TODO:
all_model_pbp |> filter(model_seconds_remaining < 0)

qs::qsave(all_model_pbp, file.path('data', 'wp_model_data.qs'))

## model ----
model <- fit_wp_model(all_model_pbp)
qs::qsave(model, file.path('data', 'wp_model.qs'))

coefs_plot <- autoplot(model_lb, type = 'coefs')
ggsave(
  coefs_plot,
  filename = file.path('figs', 'wp_coefs.png'),
  width = 12,
  height = 8
)

plot_and_save_wp_by_feature <- function(model, feature_name = NULL) {
  p <- autoplot(model, type = 'grid', feature_name = feature_name)
  
  if (is.null(feature_name)) {
    feature_name <- ''
  }
  print(p)
  ggsave(
    p,
    filename = file.path('figs', sprintf('wp_grid-%s.png', feature_name)),
    width = 12,
    height = 8
  )
  invisible(p)
}

c(
  'is_kill_on_attempted_clinch',
  'has_started_clinch',
  'is_initial_bomb_carrier_killed'
) |> 
  set_names() |> 
  walk(
    ~plot_and_save_wp_by_feature(
      model = model,
      feature_name = .x
    )
  )

plot_and_save_wp_by_feature(
  model = model
)

new_data <- tibble::tibble(
  'side' = c('d', 'o'),
  'is_pre_plant' = TRUE,
  'model_seconds_elapsed' = 84,
  'n_team_remaining' = 3,
  'n_opponent_remaining' = 2,
  'is_kill_on_attempted_clinch' = 0,
  'has_started_clinch' = 0,
  'is_initial_bomb_carrier_killed' = 0,
  'activity' = 'Start Plant'
) |> 
  dplyr::mutate(
    # 'is_offense' = .data[['side']] == 'o',
    'opponent_diff' = .data[['n_team_remaining']] - .data[['n_opponent_remaining']],
    'model_seconds_remaining' = ifelse(
      .data[['is_pre_plant']],
      max_pre_plant_second - .data[['model_seconds_elapsed']],
      max_post_plant_second - .data[['model_seconds_elapsed']]
    )
  ) |> 
  add_hardcoded_wp_cols()

predict(
  model,
  new_data
)
