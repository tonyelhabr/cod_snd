library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(qs)

pkgload::load_all('../snd')
data_dir <- getOption('snd.dir.data')

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

