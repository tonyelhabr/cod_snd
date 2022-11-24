
library(tidyverse)
library(googlesheets4)
library(qs)
library(janitor)

dir_data <- 'data'
dir.create(dir_data, showWarnings = FALSE)

read_snd_logs_sheet <- function(year, overwrite = FALSE) {
  
  path <- file.path(dir_data, sprintf('snd-pbp-%s.qs', year))
  if(file.exists(path) & !overwrite) {
    return(qs::qread(path))
  }
  
  year <- as.character(year)
  ss <- switch(
    year,
    '2021' = '11d74wib3M-8TGeVGtNtrtBMgIGrZgmaPl4jKy0tAbTc',
    '2022' = '1uecnoiksax9AcioO2KktGw46_ahe4dvsunR_s0aC0-c'
  )
  
  col_types <- 'ccicccincciicccccncccccciii'
  ## Additionaly column for situation length
  if (year == '2022') {
    col_types <- paste0(col_types, 'n')
  }
  
  raw <- read_sheet(
    ss, 
    sheet = 'Activity Feed - OFFICIAL',
    col_types = col_types
  )
  df <- raw |> janitor::clean_names()
  
  qs::qsave(df, path)
  df
}

game_mapping <- c(
  '2022' = 'Vanguard',
  '2021' = 'Cold War'
)

team_mapping <- c(
  'ATL' = 'FaZe',
  'BOS' = 'Breach',
  'DAL' = 'Empire',
  'FLA' = 'Mutineers',
  'LAG' = 'Guerrillas',
  'LAT' = 'Thieves',
  'LDN' = 'Royal Ravens',
  'LON' = 'Royal Ravens',
  'MIN' = 'Rokkr',
  'NYSL' = 'Subliners',
  'OC' = 'Optic',
  'OPTX' = 'Optic',
  'PAR' = 'Legion',
  'SEA' = 'Surge',
  'TOR' = 'Ultra'
)

# 2021 S2-036 r11, S2-040 r7, amd S3-014 r8 have '#REF!' for initial_bomb_carrier_killed, so need to manually fix those
prefixed_init_raw_pbp <- 2021:2022 |> 
  set_names() |> 
  map_dfr(read_snd_logs_sheet, .id = 'year') |> 
  mutate(
    across(year, as.integer),
    round_id = sprintf('%s-%s-%02d', year, map_id, round)
  )

## Issue with bomb timer left for 2021-SND-126-03 (bomb is never planted) + seconds elapsed is 20 seconds ahead of what it should be: https://youtu.be/3guxnrsIulQ?t=13387
## acitivity = 'Kill' for 2021-SND-285-03 when it should be a plant

## rows_patch was being inconsistent for me when joining with non-character keys (specifically seconds elapsed)
add_key <- function(df, .keep = 'all') {
  df |> 
    mutate(key = sprintf('%s-%s', round_id, seconds_elapsed), .keep = .keep)
}

changes <- bind_rows(
  tibble(
    round_id = '2021-SND-285-03',
    seconds_elapsed = 34,
    activity = 'Plant'
  ),
  tibble(
    round_id = '2021-SND-126-03',
    seconds_elapsed = c(71.4, 75.4, 94.3, 96.2, 98.9)
  ) |> 
    mutate(
      actual_seconds_elapsed = seconds_elapsed - 20
    )
)

fixed_init_raw_pbp <- rows_update(
  prefixed_init_raw_pbp |> add_key() |> mutate(actual_seconds_elapsed = NA_real_),
  changes |> add_key(.keep = 'unused'),
  by = 'key'
) |> 
  mutate(
    across(seconds_elapsed, ~coalesce(actual_seconds_elapsed, .x))
  ) |> 
  select(-actual_seconds_elapsed)

init_raw_pbp <- fixed_init_raw_pbp |> 
  rename(
    killer_player = initiating_player
  ) |> 
  mutate(
    across(activity, ~ifelse(.x == 'defuse', 'Defuse', .x)), # one bad name
    across(match_id, ~str_replace_all(.x, c('CHA' = 'CH', '!' = '1'))), ## bugs with labels in sheet
    across(killer_team, ~coalesce(.x, initiating_team)),
    ## first_blood and traded_out also use 'n/a', but we're not going to keep them around
    across(
      c(killer_team, killer_player, victim_player),
      ~na_if(.x, 'n/a')
    ),
    is_traded_out = case_when(
      traded_out == 'Y' ~ TRUE,
      traded_out == 'N' ~ FALSE,
      TRUE ~ NA
    ),
    is_kill_traded = trade_kill == 'Y',
    side = case_when(
      killer_off_def == 'Offense' ~ 'o',
      killer_off_def == 'Defense' ~ 'd',
      activity == 'Plant' ~ 'o',
      activity == 'Defuse' ~ 'd'
    ),
    is_negative_action = activity %in% c('Self Kill', 'Team Kill'),
    is_kill_on_attempted_clinch = activity %in% c('Kill Defuser', 'Kill Planter')
  )

initial_bomb_carrier_killed_times <- init_raw_pbp |> 
  filter(initial_bomb_carrier_killed == 'Y') |> 
  select(round_id, initial_bomb_carrier_killed_second = seconds_elapsed)

plant_times <- init_raw_pbp |> 
  filter(activity == 'Plant') |> 
  select(round_id, plant_second = seconds_elapsed)

defuse_times <- init_raw_pbp |> 
  filter(activity == 'Defuse') |> 
  select(round_id, defuse_second = seconds_elapsed)

## Checked a few of these. These seem to be kills immediately after the bomb is defused, when
##   there is a small window of time that you can still move your character.
##   In the 2022 and most of the 2021 spreadsheet, these instances are marked with bomb_timer_left = 0.
##   These activities are meaningless for win probability.
engagements_to_drop <- init_raw_pbp |> 
  inner_join(
    defuse_times,
    by = 'round_id'
  ) |> 
  filter(seconds_elapsed > defuse_second) |> 
  select(round_id, seconds_elapsed)

raw_pbp <- init_raw_pbp |> 
  anti_join(
    engagements_to_drop,
    by = c('round_id', 'seconds_elapsed')
  ) |> 
  left_join(
    initial_bomb_carrier_killed_times,
    by = 'round_id'
  ) |> 
  left_join(
    plant_times,
    by = 'round_id'
  ) |> 
  mutate(
    round_has_plant = !is.na(plant_second),
    is_post_plant = case_when(
      is.na(plant_second) ~ FALSE,
      seconds_elapsed < plant_second ~ FALSE,
      activity == 'Plant' ~ TRUE,
      TRUE ~ TRUE
    ),
    is_initial_bomb_carrier_killed = case_when(
      is_post_plant ~ NA,
      # is.na(initial_bomb_carrier_killed_second) ~ FALSE,
      # seconds_elapsed < initial_bomb_carrier_killed_second ~ FALSE,
      # TRUE ~ TRUE
      seconds_elapsed >= initial_bomb_carrier_killed_second ~ TRUE,
      TRUE ~ FALSE
    )
  ) |> 
  left_join(
    defuse_times,
    by = 'round_id'
  ) |> 
  mutate(
    ## there are some issues where bomb_timer_left is not filled in after plant (and, instead, round timer is)
    ##   so just compute these soley from seconds elapsed and activity implied timings.
    pre_plant_seconds_elapsed = case_when(
      !is_post_plant | (activity == 'Plant') ~ seconds_elapsed,
      TRUE ~ NA_real_
    ),
    post_plant_seconds_elapsed = case_when(
      activity == 'Plant' ~ 0,
      round_has_plant & is_post_plant ~ seconds_elapsed - plant_second,
      TRUE ~ NA_real_
    )
  ) |> 
  mutate(
    game = sprintf('%s (%s)', game_mapping[as.character(year)], year),
    .before = 1
  )
# raw_pbp |> select(round_id, activity, seconds_elapsed, round_has_plant, is_post_plant, pre_plant_seconds_elapsed, post_plant_seconds_elapsed, bomb_timer_left)

select_pbp_side <- function(df, ...) {
  df |> 
    transmute(
      side,
      game,
      round,
      map_id,
      round_id,
      
      seconds_elapsed,
      pre_plant_seconds_elapsed,
      post_plant_seconds_elapsed,
      
      ...,
      
      activity,
      map,
      weapon_or_bomb_site,
      killer_player,
      victim_player,
      killer_team,
      round_winner,
      
      plant_second,
      defuse_second,
      
      round_has_plant,
      is_post_plant,
      is_kill_on_attempted_clinch,
      is_initial_bomb_carrier_killed,
      
      is_traded_out,
      is_kill_traded,
      is_negative_action
    )
}

one_pbp <- bind_rows(
  raw_pbp |> 
    filter(side == 'o') |> 
    select_pbp_side(
      team = offense_team,
      opponent = defense_team,
      n_team_remaining = offense_remaining,
      n_opponent_remaining = defense_remaining
    ),
  raw_pbp |> 
    filter(side == 'd') |> 
    select_pbp_side(
      team = defense_team,
      opponent = offense_team,
      n_team_remaining = defense_remaining,
      n_opponent_remaining = offense_remaining
    )
) |> 
  mutate(
    across(c(team, opponent, round_winner), ~team_mapping[.x])
  ) |> 
  arrange(round_id, seconds_elapsed) |> 
  # group_by(round_id, side) |> 
  # mutate(
  #   across(
  #     c(n_team_remaining, n_opponent_remaining), 
  #     list(prev = ~lag(.x, n = 1, default = 4L))
  #   )
  # ) |> 
  # ungroup() |> 
  arrange(round_id, seconds_elapsed)

# one_pbp |>
#   filter(n_team_remaining_prev == 0L | n_opponent_remaining_prev == 0L, activity != 'Defuse') |> 
#   glimpse()
## 2021-SND-018-05, plant at the same time as the last kill?
## 2021-SND-086-04, team kill after already clinching win
## 2021-SND-292-02, team kill between time of last kill of opponent and defuse
## 2022-SND-025-10, killed last opponent before killing himself with glide bomb in 1v1
## 2022-SND-198-09, died while defusing in 1v0 situation
## 2022-SND-263-02, team kill between time of last kill of opponent and defuse

one_pbp_teams <- one_pbp |> 
  distinct(
    game,
    map_id,
    round,
    round_id,
    side,
    team,
    opponent,
    round_winner,
    plant_second,
    defuse_second,
    round_has_plant
  )

## fill in for when we only have one side (i.e. all kills from one team)
one_pbp_padded_side <- one_pbp_teams |> 
  rename(old_team = team, old_opponent = opponent, old_side = side) |> 
  inner_join(
    one_pbp_teams |> 
      count(round_id) |> 
      filter(n == 1L) |> 
      select(-n),
    by = 'round_id'
  ) |> 
  mutate(
    team = old_opponent,
    opponent = old_team,
    side = ifelse(old_side == 'o', 'd', 'o')
  ) |> 
  select(-starts_with('old'))

one_pbp_round_begin_events <- bind_rows(
  one_pbp_teams,
  one_pbp_padded_side
) |> 
  bind_cols(
    tibble(
      seconds_elapsed = 0L,
      pre_plant_seconds_elapsed = 0L,
      post_plant_seconds_elapsed = NA_real_,
      
      n_team_remaining = 4L,
      n_opponent_remaining = 4L,
      
      activity = 'Start round',
      weapon_or_bomb_site = NA_character_,
      killer_player = NA_character_,
      victim_player = NA_character_,
      killer_team = NA_character_,
      
      is_post_plant = FALSE,
      is_kill_on_attempted_clinch = NA,
      is_traded_out = NA,
      is_negative_action = NA
    )
  )

round_records <- one_pbp_round_begin_events |> 
  distinct(
    round_id, 
    map_id, 
    round,
    team, 
    opponent,
    win_round = team == round_winner, 
    lose_round = team != round_winner
  ) |> 
  arrange(map_id, round_id, team) |> 
  group_by(map_id, team) |> 
  mutate(
    team_round_wins = cumsum(win_round),
    opponent_round_wins = cumsum(lose_round),
    ## FALSE as the default makes sense for modeling
    # won_prior_round = lag(win_round, n = 1L),
    won_prior_round_side = lag(win_round, n = 2L)
  ) |> 
  ungroup() |> 
  mutate(
    team_round_wl_diff = team_round_wins - opponent_round_wins
  ) |> 
  group_by(map_id, team) |> 
  mutate(
    prev_team_round_wl_diff = dplyr::lag(team_round_wl_diff, n = 1L, default = 0L)
  ) |> 
  ungroup() |> 
  select(-c(map_id, round, win_round, lose_round))

padded_one_pbp <- bind_rows(
  one_pbp,
  one_pbp_round_begin_events
) |> 
  inner_join(
    round_records,
    by = c('round_id', 'team', 'opponent')
  ) |> 
  arrange(round_id, seconds_elapsed, side) |> 
  mutate(
    engagement_id = sprintf('%s-%s-%sv%s', round_id, side, n_team_remaining, n_opponent_remaining),
    .before = round_id
  )

both_pbp <- bind_rows(
  padded_one_pbp |> mutate(pbp_side = 'a', .before = 1),
  padded_one_pbp |> 
    mutate(
      across(
        c(n_team_remaining, n_opponent_remaining, team, opponent),
        list(orig = ~.x),
        .names = '{.fn}_{.col}'
      )
    ) |> 
    mutate(
      pbp_side = 'b',
      side = ifelse(side == 'd', 'o', 'd'),
      n_team_remaining = orig_n_opponent_remaining,
      n_opponent_remaining = orig_n_team_remaining,
      team = orig_opponent,
      opponent = orig_team
    ) |> 
    select(-starts_with('orig'))
) |> 
  mutate(
    opponent_diff = n_team_remaining - n_opponent_remaining
  ) |> 
  arrange(round_id, seconds_elapsed, pbp_side, side) |> 
  # arrange(round_id, pre_plant_seconds_elapsed, post_plant_seconds_elapsed) |> 
  group_by(round_id, side) |> 
  mutate(
    prev_opponent_diff = lag(opponent_diff, n = 1, default = 0L)
  ) |> 
  ungroup() |> 
  ## 2022-SND-223-07 has simultaneous kills to clinch
  filter(!(abs(prev_opponent_diff) == 4 & activity != 'Defuse')) |> 
  select(-prev_opponent_diff)

write_csv(both_pbp, 'data/cod_snd_pbp.csv')
