
library(tidyverse)
library(googlesheets4)
library(qs)
library(janitor)

dir_data <- 'data'
dir.create(dir_data, showWarnings = FALSE)

read_snd_logs_sheet <- function(year, overwrite = FALSE) {
  
  path <- file.path(dir_data, sprintf('snd-logs-%s.qs', year))
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

cod_rounds <- read_csv('data/cod_rounds.csv')
cod_rounds

raw_logs <- 2021:2022 |> 
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

log_ids <- raw_logs |> 
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
log_ids |> distinct()
# raw_logs |> filter(is.na(weapon_or_bomb_site))
# raw_logs |> filter(is.na(offense_remaining))

raw_logs |> 
  distinct(year, round, map, match_id, map_id, round, offense_team, round_winner) |> 
  count(offense_team == round_winner)

logs |> 
  distinct(year, round, map, match_id, map_id, round, team, is_offense, round_winner) |> 
  count(is_offense, team == round_winner)

logs <- bind_rows(
  raw_logs |> 
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
      # offense_players_remaining,
      # defense_players_remaining,
      last_round_activity,
      x1v1_situation_entered_team = x1v_situation_entered_offense,
      x1v1_situation_entered_opponent = x1v_situation_entered_defense,
      situation_length
    ),
  raw_logs |> 
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
      # offense_players_remaining,
      # defense_players_remaining,
      last_round_activity,
      x1v1_situation_entered_opponent = x1v_situation_entered_offense,
      x1v1_situation_entered_team = x1v_situation_entered_defense,
      situation_length
    )
) |> 
  mutate(
    across(c(team, opponent, round_winner), ~team_mapping[.x])
  )
logs |> 
  count(is_offense, team == round_winner)
write_csv(logs, 'data/logs.csv')

## extra ----
translate_event <- function(event) {
  # year <- str_sub(event, 1, 4)
  stage <- str_sub(event, 9, 9)
  ifelse(stage == 'h', 'CH-', sprintf('S%s', stage))
}

rounds <- read_csv('data/rounds.csv')
rounds

# |> 
#   mutate(
#     game = sprintf('%s (%s)', game_mapping[as.character(year)], year),
#     .before = 1
#   ) |> 
#   mutate(
#     across(game, ~fct_reorder(.x, year)),
#     map = sprintf('%s - %s', map, game)
#   )

rounds |> 
  filter(game == 'Vanguard (2022)') |> 
  select(event, series, team, map, round, is_offense, win_round)
logs |> 
  head(100) |> 
  inner_join(
    rounds |> 
      select(year, game, event, team, opponent) |> 
      mutate(
        match_id = translate_event(event)
      ),
    by = c('year', 'team', 'opponent')
  )
  filter(map == 'Desert Siege', team == 'LDN', opponent == 'SEA') |> 
  count(match_id)
