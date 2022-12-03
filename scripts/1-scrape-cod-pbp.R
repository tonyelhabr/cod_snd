
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

# team_mapping <- c(
#   'ATL' = 'FaZe',
#   'BOS' = 'Breach',
#   'DAL' = 'Empire',
#   'FLA' = 'Mutineers',
#   'LAG' = 'Guerrillas',
#   'LAT' = 'Thieves',
#   'LDN' = 'Royal Ravens',
#   'LON' = 'Royal Ravens',
#   'MIN' = 'Rokkr',
#   'NYSL' = 'Subliners',
#   'OC' = 'Optic',
#   'OPTX' = 'Optic',
#   'PAR' = 'Legion',
#   'SEA' = 'Surge',
#   'TOR' = 'Ultra'
# )

team_mapping <- c(
  'ATL' = 'ATL',
  'BOS' = 'BOS',
  'DAL' = 'DAL',
  'FLA' = 'FLA',
  'LAG' = 'LAG',
  'LAT' = 'LAT',
  'LDN' = 'LDN',
  'LON' = 'LDN',
  'MIN' = 'MIN',
  'NYSL' = 'NYSL',
  'OC' = 'OPTX',
  'OPTX' = 'OPTX',
  'PAR' = 'PAR',
  'SEA' = 'SEA',
  'TOR' = 'TOR'
)

# 2021 S2-036 r11, S2-040 r7, amd S3-014 r8 have '#REF!' for initial_bomb_carrier_killed, so need to manually fix those
prefixed_init_raw_pbp <- 2021:2022 |> 
  set_names() |> 
  map_dfr(read_snd_logs_sheet, .id = 'year') |> 
  mutate(
    across(year, as.integer),
    round_id = sprintf('%s-%s-%02d', year, map_id, round)
  ) |> 
  mutate(
    seconds_elapsed = case_when(
      round_id == '2021-SND-012-04' & activity == 'Plant' ~ 88,
      TRUE ~ seconds_elapsed
    )
  )

## rows_patch was being inconsistent for me when joining with non-character keys (specifically seconds elapsed)
add_key <- function(df, .keep = 'all') {
  df |> 
    mutate(key = sprintf('%s-%s', round_id, seconds_elapsed), .keep = .keep)
}

changes <- list(
  ## activity = 'Kill' for 2021-SND-285-03 when it should be a plant
  tibble(
    round_id = '2021-SND-285-03',
    seconds_elapsed = 34,
    activity = 'Plant'
    # ## not sure why, but these fields become NA with rows_patch
    # offense_team = 'OC',
    # defense_team = 'LAT'
  ),
  ## Issue with bomb timer left for 2021-SND-126-03 (bomb is never planted) + seconds elapsed is 20 seconds ahead of what it should be: https://youtu.be/3guxnrsIulQ?t=13387
  tibble(
    round_id = '2021-SND-126-03',
    seconds_elapsed = c(71.4, 75.4, 94.3, 96.2, 98.9)
    
    # offense_team = rep('DAL', 5),
    # defense_team = rep('ATL', 5),
    # activity = rep('Kill', 5)
  ) |> 
    mutate(
      actual_seconds_elapsed = seconds_elapsed - 20
    ),
  ## this is a last kill tied with a plant, so increment by 0.1 to make it non-problematic
  tibble(
    round_id = rep('2021-SND-012-04', 2),
    seconds_elapsed = c(88, 88.6),
    round_time_left = c(2L, NA_integer_),
    bomb_timer_left = c(45L, 44L)
    
    # offense_team = rep('DAL', 2),
    # defense_team = rep('SEA', 2),
    # activity = c('Plant', 'Kill')
  ),
  ## defuse happens 7 seconds after implied last second, so assume that plant happens 10 seconds later than indicated
  tibble(
    round_id = '2021-SND-247-01',
    seconds_elapsed = 15
  ) |> 
    mutate(
      actual_seconds_elapsed = seconds_elapsed + 10
    ),
  ## sides should be inversed
  tibble(
    round_id = '2022-SND-279-10',
    seconds_elapsed = c(9, 33, 42, 46),
    offense_team = rep('ATL', 4),
    defense_team = rep('LAT', 4),
    offense_players_remaining = c('Cellium-Arcitys-Simp', 'Cellium-Arcitys-', 'Cellium-', '-'),
    defense_players_remaining = rep('Octane-Drazah-Kenny-Envoy', 4)
  )
)

## maybe there's a way to do this with accumulate, but whatev
fixed_init_raw_pbp <- prefixed_init_raw_pbp
for(change in changes) {
  print(change)
  fixed_init_raw_pbp <- rows_update(
    fixed_init_raw_pbp |> add_key() |> mutate(actual_seconds_elapsed = NA_real_),
    change |> add_key(.keep = 'unused'),
    by = 'key'
  )
}

fixed_init_raw_pbp <- fixed_init_raw_pbp |> 
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
    across(
      c(offense_players_remaining, defense_players_remaining, initial_bomb_carrier_killed),
      ~na_if(.x, '#REF!')
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
  mutate(
    victim_team = case_when(
      is.na(killer_team) ~ NA_character_,
      killer_team == offense_team ~ defense_team,
      killer_team == defense_team ~ offense_team
    ),
    activity_team = case_when(
      activity %in% c('Defuse', 'Kill Planter') ~ defense_team,
      activity %in% c('Plant', 'Kill Defuser') ~ offense_team,
      activity %in% c('Kill', 'Self Kill', 'Team Kill') ~ killer_team
    ),
    activity_opponent = case_when(
      activity %in% c('Defuse', 'Kill Planter') ~ offense_team,
      activity %in% c('Plant', 'Kill Defuser') ~ defense_team,
      activity %in% c('Kill', 'Self Kill', 'Team Kill') ~ victim_team
    )
  ) |> 
  rename(
    activity_player = killer_player,
    activity_opposer = victim_player
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
      is_post_plant & activity != 'Plant' ~ NA,
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
      activity_player,
      activity_opposer,
      activity_team,
      activity_opponent,
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
      n_opponent_remaining = defense_remaining,
      team_players_remaining = offense_players_remaining,
      opponent_players_remaining = defense_players_remaining
    ),
  raw_pbp |> 
    filter(side == 'd') |> 
    select_pbp_side(
      team = defense_team,
      opponent = offense_team,
      n_team_remaining = defense_remaining,
      n_opponent_remaining = offense_remaining,
      team_players_remaining = defense_players_remaining,
      opponent_players_remaining = offense_players_remaining
    )
) |> 
  mutate(
    across(
      c(
        team, 
        opponent,
        round_winner, 
        activity_team, 
        activity_opponent
      ), 
      ~team_mapping[.x]
    )
  ) |> 
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

all_team_players <- one_pbp |> 
  distinct(game, map_id, team, player = team_players_remaining) |> 
  separate_rows(player, sep = '-') |> 
  filter(player != '') |> 
  distinct(game, map_id, team, player) |> 
  arrange(game, map_id, team, player)

all_opponent_players <- one_pbp |> 
  distinct(game, map_id, team = opponent, player = opponent_players_remaining) |> 
  separate_rows(player, sep = '-') |> 
  filter(player != '') |> 
  distinct(game, map_id, team, player) |> 
  arrange(game, map_id, team, player)

all_players <- bind_rows(
  all_team_players,
  all_opponent_players
) |> 
  distinct(game, map_id, team, player) |> 
  arrange(game, map_id, team, player)


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
one_pbp_padded_side

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
      
      activity = 'Start',
      weapon_or_bomb_site = NA_character_,
      killer_player = NA_character_,
      victim_player = NA_character_,
      killer_team = NA_character_,
      
      is_post_plant = FALSE,
      is_kill_on_attempted_clinch = FALSE,
      is_initial_bomb_carrier_killed = FALSE,
      
      is_traded_out = NA,
      is_kill_traded = NA,
      is_negative_action = FALSE
    )
  ) |> 
  inner_join(
    all_players |> 
      group_by(game, map_id, team) |> 
      summarize(team_players_remaining = paste0(player, collapse = '-')) |> 
      ungroup(),
    by = c('game', 'map_id', 'team')
  ) |> 
  inner_join(
    all_players |> 
      group_by(game, map_id, opponent = team) |> 
      summarize(opponent_players_remaining = paste0(player, collapse = '-')) |> 
      ungroup(),
    by = c('game', 'map_id', 'opponent')
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
    prev_team_round_wl_diff = lag(team_round_wl_diff, n = 1L, default = 0L)
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
  group_by(round_id, side) |> 
  mutate(
    team_players_pre_activity = lag(team_players_remaining, n = 1L),
    opponent_players_pre_activity = lag(opponent_players_remaining, n = 1L)
  ) |> 
  ungroup() |> 
  mutate(
    across(team_players_pre_activity, ~coalesce(.x, team_players_remaining)),
    across(opponent_players_pre_activity, ~coalesce(.x, opponent_players_remaining))
  ) |> 
  select(
    -ends_with('players_remaining')
  )

both_pbp <- bind_rows(
  padded_one_pbp |> mutate(pbp_side = 'a', .before = 1),
  padded_one_pbp |> 
    mutate(
      across(
        c(
          team,
          opponent,
          n_team_remaining,
          n_opponent_remaining,
          team_players_pre_activity,
          opponent_players_pre_activity
        ),
        list(orig = ~ .x),
        .names = '{.fn}_{.col}'
      )
    ) |> 
    mutate(
      pbp_side = 'b',
      side = ifelse(side == 'd', 'o', 'd'),
      n_team_remaining = orig_n_opponent_remaining,
      n_opponent_remaining = orig_n_team_remaining,
      team = orig_opponent,
      opponent = orig_team,
      team_players_pre_activity = orig_opponent_players_pre_activity,
      opponent_players_pre_activity = orig_opponent_players_pre_activity
    ) |> 
    select(-starts_with('orig'))
) |> 
  ## we don't need 4 records for the round start (i.e. pbp_side=a, side=o, pbp_side=b, side=o, pbp_side=a, side=d, pbp_side=b, side=d)
  ##   so filter out 2 of the records and re-assign the pbp_side to be NA
  filter(
    !(pbp_side == 'b' & activity == 'Start')
  ) |> 
  mutate(
    across(pbp_side, ~ifelse(activity == 'Start', NA_character_, .x)),
    engagement_id = sprintf('%s-%s-%sv%s-%s', round_id, side, n_team_remaining, n_opponent_remaining, activity),
    .before = round_id
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

numbered_players <- both_pbp |> 
  distinct(game, map_id, team, player = team_players_pre_activity) |> 
  separate_rows(player, sep = '-') |> 
  filter(player != '') |> 
  distinct(game, map_id, team, player) |> 
  group_by(game, map_id, team) |> 
  mutate(
    player_rn = row_number(player)
  ) |> 
  ungroup()

round_map_team_mapping <- both_pbp |> 
  distinct(game, map_id, round_id, team, side)

round_engagement_mapping <- both_pbp |> 
  distinct(round_id, engagement_id, side)

round_team_player_mapping <- round_map_team_mapping |> 
  left_join(
    numbered_players,
    by = c('game', 'map_id', 'team'),
    multiple = 'all'
  )

# 649,544
grid <- round_engagement_mapping |> 
  left_join(
    round_team_player_mapping,
    by = c('round_id', 'side'),
    multiple = 'all'
  )

init <- both_pbp |> 
  distinct(game, map_id, engagement_id, round_id, team, player = team_players_pre_activity) |> 
  separate_rows(player, sep = '-') |> 
  filter(player != '') |> 
  distinct(game, map_id, engagement_id, round_id, team, player) |> 
  left_join(
    numbered_players,
    by = c('game', 'map_id', 'team', 'player')
  ) |> 
  mutate(value = 1)

long_participation <- grid |> 
  left_join(
    init,
    by = c('game', 'map_id', 'engagement_id', 'round_id', 'team', 'player', 'player_rn')
  ) |> 
  mutate(
    across(value, ~coalesce(.x, 0))
  )

wide_participation <- long_participation |> 
  filter(side == 'o') |> 
  pivot_wider(
    names_sort = TRUE,
    names_from = player_rn,
    values_from = c(player, value),
    names_glue = 'o_{.value}_{player_rn}'
  )

str_extract_engagement_id <- function(engagement_id, field) {
  i <- switch(
    field,
    'year' = 1,
    'map_id' = 2,
    'round' = 3,
    'side' = 4,
    'n_team_remaining' = 5,
    'n_opponent_remaining' = 6,
    'activity' = 7
  )
  str_replace(engagement_id, '(^[0-2]{4})-(SND-[0-9]{3})-([0-9]{2})-([od])-([0-4])v([0-4])-(.*$)', sprintf('\\%d', i))
}


symmetrics_engagement_ids <- wide_participation |> 
  select(engagement_id) |> 
  mutate(
    across(
      engagement_id,
      list(
        year = ~str_extract_engagement_id(.x, 'year'),
        map_id = ~str_extract_engagement_id(.x, 'map_id'),
        round = ~str_extract_engagement_id(.x, 'round'),
        side = ~str_extract_engagement_id(.x, 'side'),
        n_team_remaining = ~str_extract_engagement_id(.x, 'n_team_remaining'),
        n_opponent_remaining = ~str_extract_engagement_id(.x, 'n_opponent_remaining'),
        activity = ~str_extract_engagement_id(.x, 'activity')
      ),
      .names = '{fn}'
    ),
    round_id = sprintf('%s-%s-%s', year, map_id, round),
    symmetric_engagement_id = sprintf('%s-%s-%sv%s-%s', round_id, ifelse(side == 'd', 'o', 'd'), n_opponent_remaining, n_team_remaining, activity),
  ) |> 
  select(
    engagement_id,
    symmetric_engagement_id
  )

long_participation |> 
  inner_join(
    symmetrics_engagement_ids,
    by = 'engagement_id'
  )

wide_participation |> 
  inner_join(
    symmetrics_engagement_ids,
    by = 'engagement_id'
  ) |> 
  left_join(
    wide_participation
  )


both_pbp |> 
  filter(
    engagement_id == '2021-SND-011-01-d-0v2-Kill'
  )

wide_participation |> 
  filter(
    engagement_id == '2021-SND-011-01-d-0v2-Kill'
  ) |> 
  glimpse()

wide_participation |> 
  filter(
    engagement_id == '2021-SND-011-01-o-2v0-Kill'
  ) |> 
  glimpse()

both_pbp |> 
  distinct(engagement_id, seconds_elapsed) |> 
  inner_join(
    wide_participation
  ) |> 
  filter(
    engagement_id == '2021-SND-011-01-o-2v0-Kill'
  )


## timing ----
base <- both_pbp |> 
  # filter(round_id == first(round_id)) |> 
  filter(!is.na(pbp_side), activity %in% c('Kill', 'Kill Planter', 'Kill Defuser')) |> 
  filter(side == 'o') |> 
  select(round_id, engagement_id, seconds_elapsed, activity_player, activity_opposer)


overlaps <- base |> 
  inner_join(
    base |> 
      transmute(
        round_id, 
        other_engagement_id = engagement_id,
        other_seconds_elapsed = seconds_elapsed,
        prev_seconds_elapsed = seconds_elapsed - 5, 
        next_seconds_elapsed = seconds_elapsed + 5
      ),
    join_by(
      round_id,
      seconds_elapsed >= prev_seconds_elapsed, 
      seconds_elapsed <= next_seconds_elapsed
    )
  ) |> 
  filter(
    engagement_id != other_engagement_id
  ) |> 
  mutate(
    other_is_after = other_seconds_elapsed > seconds_elapsed
  )

nested_overlaps <- overlaps |> 
  select(engagement_id, other_engagement_id) |> 
  nest(other_engagement_ids = -c(engagement_id))

count(engagement_id) |> 
  count(n)
