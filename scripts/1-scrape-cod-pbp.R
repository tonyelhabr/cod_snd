
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

raw_pbp <- 2021:2022 |> 
  set_names() |> 
  map_dfr(read_snd_logs_sheet, .id = 'year') |> 
  mutate(
    across(year, as.integer),
    across(activity, ~ifelse(.x == 'defuse', 'Defuse', .x)), # one bad name
    across(match_id, ~str_replace_all(.x, c('CHA' = 'CH', '!' = '1'))), ## bugs with labels in sheet
    across(killer_team, ~coalesce(.x, initiating_team))
  ) |> 
  select(-initiating_team) |> 
  rename(
    killer_player = initiating_player
  )

cod_game_mapping <- c(
  '2022' = 'Vanguard',
  '2021' = 'Cold War',
  '2020' = 'MW'
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

pbp_ids <- raw_pbp |> 
  select(
    year,
    round,
    map,
    match_id,
    map_id,
    round,
    map,
    offense_team,
    defense_team,
    offense_remaining,
    defense_remaining,
    round_time_left,
    bomb_timer_left,
    activity
  )
stopifnot(nrow(pbp_ids) == nrow(distinct(pbp_ids)))

pbp <- bind_rows(
  raw_pbp |> 
    transmute(
      year,
      round,
      map,
      match_id,
      map_id,
      round,
      map,
      round_time_left,
      bomb_timer_left,
      activity,
      is_offense = TRUE,
      team = offense_team,
      opponent = defense_team,
      n_team_remaining = offense_remaining,
      n_opponent_remaining = defense_remaining,
      weapon_or_bomb_site,
      killer_player,
      victim_player,
      killer_team,
      killer_off_def,
      round_winner,
      seconds_elapsed,
      first_blood,
      traded_out,
      trade_kill,
      initial_bomb_carrier_killed,
      last_round_activity,
      x1v1_situation_entered_team = x1v_situation_entered_offense,
      x1v1_situation_entered_opponent = x1v_situation_entered_defense,
      situation_length
    ),
  raw_pbp |> 
    transmute(
      year,
      round,
      map,
      match_id,
      map_id,
      round,
      map,
      round_time_left,
      bomb_timer_left,
      activity,
      is_offense = FALSE,
      team = defense_team,
      opponent = offense_team,
      n_team_remaining = defense_remaining,
      n_opponent_remaining = offense_remaining,
      weapon_or_bomb_site,
      killer_player,
      victim_player,
      killer_team,
      killer_off_def,
      round_winner,
      seconds_elapsed,
      first_blood,
      traded_out,
      trade_kill,
      initial_bomb_carrier_killed,
      last_round_activity,
      x1v1_situation_entered_opponent = x1v_situation_entered_offense,
      x1v1_situation_entered_team = x1v_situation_entered_defense,
      situation_length
    )
) |> 
  mutate(
    across(c(team, opponent, round_winner), ~team_mapping[.x])
  )

write_csv(pbp, 'data/cod_snd_pbp.csv')

