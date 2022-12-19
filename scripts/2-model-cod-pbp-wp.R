library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(qs)

pkgload::load_all('../snd')
data_dir <- getOption('snd.dir.data')
figs_dir <- getOption('snd.dir.figs')
theme_set_snd()

## TODO: 
##   1. Possibly try features for number of ARs and SMGs left on each side.
##   2. Try terms for the game (e.g. "Cold War" or "Vanguard") and the map. (mixed-effects?)
both_pbp <- qs::qread(file.path(data_dir, 'cod_snd_pbp.qs'))

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
    has_started_clinch,
    is_kill_on_attempted_clinch,
    
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
  ## TODO: better logic here... maybe don't even try to have heuristics here, just look at empirically if it was over
  mutate(
    is_1v0_post_plant = !is_pre_plant & (n_team_remaining == 1) & (n_opponent_remaining == 0),
    is_0v1_post_plant = !is_pre_plant & (n_team_remaining == 0) & (n_opponent_remaining == 1),
    is_Nv0_post_plant = !is_pre_plant & (n_team_remaining > 1) & (n_opponent_remaining == 0),
    is_0vN_post_plant = !is_pre_plant & (n_team_remaining == 0) & (n_opponent_remaining > 1),
    
    hardcoded_wp = case_when(
      
      side == 'o' & activity == 'Defuse' ~ 0,
      side == 'd' & activity == 'Defuse' ~ 1,
      
      side == 'o' & !is_pre_plant & (n_team_remaining == 0) & model_seconds_remaining >= 7.5 & activity == 'Start Defuse' ~ 0,
      side == 'd' & !is_pre_plant & (n_opponent_remaining == 0) & model_seconds_remaining >= 7.5 & activity == 'Start Defuse' ~ 1,
      
      is_1v0_post_plant & model_seconds_remaining >= 15 ~ 1,
      is_1v0_post_plant & model_seconds_remaining >= 12.5 ~ 0.9,
      is_1v0_post_plant & model_seconds_remaining >= 10 ~ 0.65,
      is_1v0_post_plant & model_seconds_remaining >= 7.5 ~ 0.5,
      is_1v0_post_plant & model_seconds_remaining < 7.5  ~ 0,
      
      is_0v1_post_plant & model_seconds_remaining >= 15 ~ 0,
      is_0v1_post_plant & model_seconds_remaining >= 12.5  ~ 0.1,
      is_0v1_post_plant & model_seconds_remaining >= 10 ~ 0.35,
      is_0v1_post_plant & model_seconds_remaining >= 7.5 ~ 0.5,
      is_0v1_post_plant & model_seconds_remaining < 7.5  ~ 1,
      
      is_Nv0_post_plant & model_seconds_remaining >= 12.5 ~ 1,
      is_Nv0_post_plant & model_seconds_remaining >= 10 ~ 0.75,
      is_Nv0_post_plant & model_seconds_remaining >= 7.5 ~ 0.5,
      is_Nv0_post_plant & model_seconds_remaining < 7.5  ~ 0.25,
      
      is_0vN_post_plant & model_seconds_remaining >= 12.5 ~ 0,
      is_0vN_post_plant & model_seconds_remaining >= 10 ~ 0.25,
      is_0vN_post_plant & model_seconds_remaining >= 7.5 ~ 0.5,
      is_0vN_post_plant & model_seconds_remaining < 7.5  ~ 0.75,
      
      TRUE ~ NA_real_
    ),
    is_wp_hardcoded = !is.na(hardcoded_wp)
  ) |>
  select(
    -matches('is_[10N]v[10N]_post_plant')
  )

## should have 0 rows
stopifnot(0 == (all_model_pbp |> filter(!is_pre_plant, !is_post_plant) |> distinct(round_id) |> nrow()))
## should only be plant activities
stopifnot(1 == (all_model_pbp |> filter(is_pre_plant, is_post_plant) |> count(activity) |> nrow()))
# all_model_pbp |> filter(is_pre_plant, is_post_plant) |> count(is_initial_bomb_carrier_killed)
# all_model_pbp |> filter(!is_pre_plant, is_post_plant) |> count(is_initial_bomb_carrier_killed)

## TODO: Fix these
all_model_pbp |> filter(model_seconds_remaining < 0)
qs::qsave(all_model_pbp, file.path(data_dir, 'wp_model_data.qs'))


## win props ----
all_model_pbp |> 
  distinct(side, round_id, win_round) |> 
  count(side, win_round) |> 
  group_by(side) |> 
  mutate(
    total = sum(n)
  ) |> 
  ungroup() |> 
  mutate(
    prop = n / total
  )

round_win_prop_by_xvy <- all_model_pbp |>
  # filter(activity != 'Plant')
  # filter(n_team_pre_activity > 0L, n_opponent_pre_activity > 0L) |> 
  count(side, is_post_plant, n_team_pre_activity, n_opponent_pre_activity, win_round) |> 
  right_join(
    crossing(
      side = c('o', 'd'),
      is_post_plant = c(TRUE, FALSE),
      n_team_pre_activity = 0L:4L, 
      n_opponent_pre_activity = 0L:4L,
      win_round = c('yes', 'no')
    ), 
    by = c('side', 'is_post_plant', 'n_team_pre_activity', 'n_opponent_pre_activity', 'win_round')
  ) |> 
  group_by(side, is_post_plant, n_team_pre_activity, n_opponent_pre_activity) |> 
  mutate(
    total = sum(n)
  ) |> 
  ungroup() |> 
  mutate(
    prop = n / total
  ) |> 
  arrange(desc(prop))

round_win_prop_by_xvy |> 
  filter(side == 'd', win_round) |> 
  arrange(desc(prop)) |> 
  select(-c(side, win_round))


## model ----
model <- fit_wp_model(all_model_pbp)
qs::qsave(model, file.path(data_dir, 'wp_model.qs'))

naive_model <- fit_naive_wp_model(all_model_pbp)

coefs_plot <- autoplot(model, type = 'coefs')
ggsave(
  coefs_plot,
  filename = file.path(figs_dir, 'wp_coefs.png'),
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
    filename = file.path(figs_dir, sprintf('wp_grid-%s.png', feature_name)),
    width = 12,
    height = 8
  )
  invisible(p)
}

c(
  'has_started_clinch',
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
  'has_started_clinch' = 0,
  'has_started_clinch' = 0,
  'is_initial_bomb_carrier_killed' = 0,
  'activity' = 'Start Plant'
) |> 
  dplyr::mutate(
    # 'is_offense' = side == 'o',
    'opponent_diff' = n_team_remaining - n_opponent_remaining,
    'model_seconds_remaining' = ifelse(
      is_pre_plant,
      max_pre_plant_second - .data[['model_seconds_elapsed']],
      max_post_plant_second - .data[['model_seconds_elapsed']]
    )
  ) |> 
  add_hardcoded_wp_cols()

predict(
  model,
  new_data
)
