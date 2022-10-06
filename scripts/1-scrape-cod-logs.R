
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

logs <- 2021:2022 |> 
  map_dfr(read_snd_logs_sheet) |> 
  mutate(
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

logs |> 
  select(
    round,
    map,
    match_id,
    map_id,
    round,
    map,
    offense_team,
    defense_team,
    round_time_left,
    bomb_timer_left,
    activity,
    weapon_or_bomb_site,
    offense_remaining,
    defense_remaining,
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
    offense_players_remaining,
    defense_players_remaining,
    last_round_activity,
    x1v_situation_entered_offense,
    x1v_situation_entered_defense,
    situation_length
  )
