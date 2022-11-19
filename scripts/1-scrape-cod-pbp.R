
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

cod_game_mapping <- c(
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
  mutate(
    across(activity, ~ifelse(.x == 'defuse', 'Defuse', .x)), # one bad name
    across(match_id, ~str_replace_all(.x, c('CHA' = 'CH', '!' = '1'))), ## bugs with labels in sheet
    across(killer_team, ~coalesce(.x, initiating_team)),
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
    is_negative_action = activity %in% c('Self Kill', 'Team Kill')
  ) |> 
  select(-initiating_team) |> 
  rename(
    killer_player = initiating_player
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
  mutate(
    is_initial_bomb_carrier_killed = case_when(
      is.na(initial_bomb_carrier_killed_second) ~ FALSE,
      seconds_elapsed < initial_bomb_carrier_killed_second ~ FALSE,
      TRUE ~ TRUE
    )
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
    is_during_attempted_plant = case_when(
      ## TODO: What about plant_second NA?
      !is.na(plant_second) & (seconds_elapsed >= (plant_second - 5)) & (seconds_elapsed < plant_second) ~ TRUE,
      activity == 'Kill Planter' ~ TRUE,
      TRUE ~ FALSE
    )
  ) |> 
  left_join(
    defuse_times,
    by = 'round_id'
  ) |> 
  mutate(
    is_during_attempted_defuse = case_when(
      ## TODO: What about plant_second NA?
      !is.na(defuse_second) & (seconds_elapsed >= (defuse_second - 7.5)) & (seconds_elapsed < defuse_second) ~ TRUE,
      activity == 'Kill Defuser' ~ TRUE,
      TRUE ~ FALSE
    )
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
  )
# raw_pbp |> select(round_id, activity, seconds_elapsed, round_has_plant, is_post_plant, pre_plant_seconds_elapsed, post_plant_seconds_elapsed, bomb_timer_left)

select_pbp_side <- function(df, ...) {
  df |> 
    transmute(
      side,
      # year,
      # round,
      # map_id,
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
      is_during_attempted_plant,
      is_during_attempted_defuse,
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
  arrange(round_id, seconds_elapsed) |> 
  mutate(
    across(c(team, opponent, round_winner), ~team_mapping[.x])
  )

both_pbp <- bind_rows(
  one_pbp |> mutate(pbp_side = 'a', .before = 1),
  one_pbp |> 
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
    engagement_id = sprintf('%s-%s-%sv%s', round_id, side, n_team_remaining, n_opponent_remaining),
    .before = round_id
  ) |> 
  arrange(round_id, seconds_elapsed, pbp_side, side)

write_csv(both_pbp, 'data/cod_snd_pbp.csv')
one_pbp |> filter(round_id == '2021-SND-071-06')
model_pbp |> filter(round_id == '2021-SND-071-06') |> filter(is_pre_plant, is_post_plant) |> glimpse()
