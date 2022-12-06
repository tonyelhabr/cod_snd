
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
  ## seconds elapsed are completely busted for this round
  filter(
    round_id != '2022-SND-120-01'
  ) |> 
  mutate(
    ## need to break tie on seconds elapsed prior to changes
    seconds_elapsed = case_when(
      round_id == '2021-SND-012-04' & activity == 'Plant' ~ 88,
      round_id == '2021-SND-018-05' & activity == 'Plant' ~ 80.5,
      TRUE ~ seconds_elapsed
    )
  )

changes <- list(
  ## activity = 'Kill' for 2021-SND-285-03 when it should be a plant
  tibble(
    round_id = '2021-SND-285-03',
    seconds_elapsed = 34,
    activity = 'Plant'
  ),
  ## Issue with bomb timer left for 2021-SND-126-03 (bomb is never planted) + seconds elapsed is 20 seconds ahead of what it should be: https://youtu.be/3guxnrsIulQ?t=13387
  tibble(
    round_id = '2021-SND-126-03',
    seconds_elapsed = c(71.4, 75.4, 94.3, 96.2, 98.9)
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
    offense_remaining = c(3L, 2L, 1L, 0L),
    defense_remaining = rep(4L, 4L),
    offense_players_remaining = c('Cellium-Arcitys-Simp', 'Cellium-Arcitys-', 'Cellium-', '-'),
    defense_players_remaining = rep('Octane-Drazah-Kenny-Envoy', 4)
  ),
  tibble(
    round_id ='2021-SND-018-05',
    seconds_elapsed = c(80.5, 81.5),
    round_time_left = c(10, 81),
    bomb_timer_left = c(45, 44),
    defense_players_remaining = c('-SlasheR-', '-'),
    offense_remaining = rep(2L, 2L),
    defense_remaining = c(1L, 0L),
    last_round_activity = c(0L, 1L)
  ),
  tibble(
    round_id ='2021-SND-042-05',
    seconds_elapsed = 70.8,
    ## guessing that it's shotzzy who planted... mack couldn't be the planter since he's on the other team
    initiating_player = 'Shotzzy' 
  )
)

## rows_patch was being inconsistent for me when joining with non-character keys (specifically seconds elapsed)
add_key <- function(df, .keep = 'all') {
  df |> 
    mutate(key = sprintf('%s-%s', round_id, seconds_elapsed), .keep = .keep)
}

## we'd need something in between rows_update and rows_patch to do this all with one df
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


## Create all_players to use for the dummy starting events. This helps a lot when
##   it comes to participation, since it helps identify players in rounds when
##   one side eliminates all opponent players without being eliminated themselves
##   (pivot_wider NAs).
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

## players that played on more than one team
all_players |> 
  distinct(game, team, player) |> 
  count(game, player) |> 
  filter(n > 1)

## TODO: Somehow fix these weird situations where a player is assigned to the wrong team
##   or an incorrect player is credited.
##   Each case should be treated on an individual basis. Perhaps the safer assumption is that
##   the player is correct (see below).
##   This creates issues with the participation weights, leading to engagement sums not equal to 0
##   (or 1, in the case that there's a defuse after all offense players are eliminated).
one_pbp |> 
  inner_join(
    all_players |> 
      select(game, map_id, activity_player = player, activity_team2 = team),
    by = c('game', 'map_id', 'activity_player')
  ) |> 
  filter(
    activity_team != activity_team2
  )

one_pbp |> 
  inner_join(
    all_players |> 
      select(game, map_id, activity_opposer = player, activity_opponent2 = team),
    by = c('game', 'map_id', 'activity_opposer')
  ) |> 
  filter(
    activity_opponent != activity_opponent2
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
    n_team_pre_activity = lag(n_team_remaining, n = 1L, default = 4L),
    n_opponent_pre_activity = lag(n_opponent_remaining, n = 1L, default = 4L),
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
          n_team_pre_activity,
          n_opponent_pre_activity,
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
      team = orig_opponent,
      opponent = orig_team,
      n_team_remaining = orig_n_opponent_remaining,
      n_opponent_remaining = orig_n_team_remaining,
      n_team_pre_activity = orig_n_opponent_pre_activity,
      n_opponent_pre_activity = orig_n_team_pre_activity,
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
    offense_engagement_id = sprintf(
      '%s-%sv%s-%s', 
      round_id, 
      ifelse(side == 'o', n_team_remaining, n_opponent_remaining),
      ifelse(side == 'o', n_opponent_remaining, n_team_remaining),
      activity
    ),
    engagement_id = sprintf('%s-%s-%sv%s-%s', round_id, side, n_team_remaining, n_opponent_remaining, activity),
    .before = round_id
  ) |> 
  mutate(
    opponent_diff = n_team_remaining - n_opponent_remaining
  ) |> 
  arrange(round_id, seconds_elapsed, pbp_side, side) |> 
  group_by(round_id, side) |> 
  mutate(
    prev_opponent_diff = lag(opponent_diff, n = 1, default = 0L)
  ) |> 
  ungroup() |> 
  ## 2022-SND-223-07 has simultaneous kills to clinch
  filter(!(abs(prev_opponent_diff) == 4 & activity != 'Defuse')) |> 
  select(-prev_opponent_diff)
qs::qsave(both_pbp, file.path('data', 'cod_snd_pbp.qs'))

## participation ----
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
  distinct(round_id, offense_engagement_id)

round_team_player_mapping <- round_map_team_mapping |> 
  left_join(
    numbered_players,
    by = c('game', 'map_id', 'team'),
    multiple = 'all'
  )

# 324k
grid_participation <- round_engagement_mapping |> 
  left_join(
    round_team_player_mapping,
    by = 'round_id',
    multiple = 'all'
  )

init_participation <- both_pbp |> 
  distinct(game, map_id, offense_engagement_id, team, player = team_players_pre_activity) |> 
  separate_rows(player, sep = '-') |> 
  filter(player != '') |> 
  distinct(game, map_id, offense_engagement_id, team, player) |> 
  mutate(indicator = 1)

long_participation <- grid_participation |> 
  left_join(
    init_participation,
    by = c('game', 'map_id', 'offense_engagement_id', 'team', 'player')
  ) |> 
  mutate(
    across(indicator, ~coalesce(.x, 0))
  ) |> 
  left_join(
    both_pbp |>
      select(
        engagement_id,
        offense_engagement_id,
        round_id,
        team,
        side,
        activity,
        activity_player,
        activity_opposer,
        activity_team,
        activity_opponent,
        n_team_pre_activity,
        n_opponent_pre_activity,
        is_negative_action
      ),
    by = c('offense_engagement_id', 'round_id', 'team', 'side')
  ) |>
  mutate(
    ## i think this system is probably fine for every engagement, except for perhaps self kills or team kills
    wt = case_when(
      activity == 'Start' ~ NA_real_,
      is_negative_action & player == activity_player ~ -1,
      is_negative_action & player != activity_player ~ 0,
      
      player == activity_player & n_team_pre_activity > 1 ~ 0.6,
      player == activity_player & n_team_pre_activity == 1 ~ 1,
      !is.na(activity_opposer) & player == activity_opposer & n_team_pre_activity > 1 ~ -0.6,
      !is.na(activity_opposer) & player == activity_opposer & n_team_pre_activity == 1 ~ -1,
      
      activity == 'Plant' & team == activity_team & n_team_pre_activity > 1  ~ 0.4 / (n_team_pre_activity - 1),
      activity == 'Plant' & team == activity_team & n_team_pre_activity == 1  ~ 0.4,
      
      activity == 'Defuse' & team == activity_team & n_team_pre_activity > 1  ~ 0.4 / (n_team_pre_activity - 1),
      activity == 'Defuse' & team == activity_team & n_team_pre_activity == 1  ~ 0.4,
      
      activity == 'Defuse' & team != activity_team & n_team_pre_activity > 1 ~ -1 / n_team_pre_activity,
      activity == 'Defuse' & team != activity_team & n_team_pre_activity == 1  ~ -1,
      activity == 'Defuse' & n_team_pre_activity == 0  ~ 0,
      
      activity == 'Plant' & team != activity_team & n_team_pre_activity > 1 ~ -1 / n_team_pre_activity,
      activity == 'Plant' & team != activity_team & n_team_pre_activity == 1  ~ -1,
      
      team == activity_team & n_team_pre_activity > 1  ~ 0.4 / (n_team_pre_activity - 1),
      team == activity_team & n_team_pre_activity == 1  ~ 0.4,
      !is.na(activity_opposer) & team == activity_opponent & n_team_pre_activity > 1 ~ -0.4 / (n_team_pre_activity - 1),
      !is.na(activity_opposer) & team == activity_opponent & n_team_pre_activity == 1  ~ -0.4
    ),
    across(wt, ~.x * indicator)
  ) |> 
  relocate(indicator, .after = wt)

## should it always equal 1?
## should be equal to 1 for defuses with no defense
## also equal to 1 for activity='Kill' for '2022-SND-223-07', where there is a trade-out on the last kill at the exact same time
# long_participation |> 
#   filter(activity != 'Start') |> 
#   group_by(offense_engagement_id, activity) |> 
#   summarize(
#     across(wt, sum)
#   ) |> 
#   ungroup() |> 
#   arrange(desc(wt)) |> 
#   # filter(wt == 1) |> 
#   mutate(across(wt, round, 2))

qs::qsave(long_participation, file.path('data', 'cod_snd_participation.qs'))

## timing ----
# d <- tibble(
#   seconds_elapsed = c(1, 3, 6, 12, 13, 14),
#   player = c('a', 'x', 'b', 'b', 'z', 'b'),
#   opposer = c('w', 'a', 'x', 'y', 'c', 'z'),
#   team = c(1, 2, 1, 1, 2, 1),
#   opponent = c(2, 1, 2, 2, 1, 2),
#   is_chain = c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE),
#   chain_id = c(
#     list(NULL), 
#     list('1'),
#     list(c('1', '2')),
#     list(NULL), 
#     list(NULL), 
#     list('5')
#   )
# ) |>
#   mutate(
#     id = as.character(row_number()),
#     .before = 1
#   )
# d
# 
# dd <- bind_rows(
#   d |> mutate(flipped = FALSE),
#   d |> 
#     mutate(
#       across(
#         c(
#           player,
#           opposer,
#           team,
#           opponent
#         ),
#         list(orig = ~ .x),
#         .names = '{.fn}_{.col}'
#         
#       )
#     ) |> 
#     mutate(
#       flipped = TRUE,
#       player = orig_opposer,
#       opposer = orig_player,
#       team = orig_opponent,
#       opponent = orig_team
#     ) |> 
#     select(-starts_with('orig'))
# ) |> 
#   arrange(id, flipped)
# 
# pc <- dd |>
#   filter(flipped) |> 
#   inner_join(
#     dd |>
#       filter(!flipped) |> 
#       transmute(
#         past_id = id,
#         past_seconds_elapsed = seconds_elapsed,
#         next_seconds_elapsed = seconds_elapsed + 5,
#         player,
#         past_opposer = opposer
#       ),
#     join_by(
#       player,
#       seconds_elapsed > past_seconds_elapsed,
#       seconds_elapsed <= next_seconds_elapsed
#     )
#   )
# pc |> 
#   group_by(id)
# 
# pc <- d |>
#   inner_join(
#     d |>
#       transmute(
#         past_id = id,
#         past_seconds_elapsed = seconds_elapsed,
#         next_seconds_elapsed = seconds_elapsed + 5,
#         past_player = player,
#         past_opposer = opposer
#       ),
#     join_by(
#       seconds_elapsed > past_seconds_elapsed,
#       seconds_elapsed <= next_seconds_elapsed
#     )
#   ) |>
#   filter(
#     id != past_id
#   )
# 
# d |>
#   left_join(
#     pc |>
#       group_by(id) |>
#       summarize(
#         past_id = list(past_id),
#         past_players = list(past_player),
#         past_opposers = list(past_opposer)
#       ),
#     by = c('id')
#   ) |>
#   filter(
#     opposer %in% c(past_players)
#   )


init_chained <- both_pbp |> 
  ## i think this only works when viewing things from one side
  # filter(pbp_side == 'a') |> 
  filter(activity %in% c('Kill', 'Kill Planter', 'Kill Defuser')) |> 
  select(pbp_side, round_id, engagement_id, seconds_elapsed, activity_player, activity_opposer)

# https://doug-liebe.medium.com/exploring-win-probability-added-a-framework-for-measuring-impact-in-call-of-duty-d360f733ff6
past_chained <- init_chained |> 
  inner_join(
    init_chained |> 
      transmute(
        pbp_side,
        round_id, 
        past_engagement_id = engagement_id,
        past_seconds_elapsed = seconds_elapsed,
        next_seconds_elapsed = seconds_elapsed + 5,
        past_activity_player = activity_player,
        past_activity_opposer = activity_opposer
      ),
    join_by(
      round_id,
      pbp_side,
      seconds_elapsed > past_seconds_elapsed,
      seconds_elapsed <= next_seconds_elapsed
    )
  ) |> 
  filter(
    engagement_id != past_engagement_id
  ) |> 
  select(-next_seconds_elapsed)

actual_chains <- past_chained |> 
  select(round_id, engagement_id, past_engagement_id, past_activity_player) |> 
  nest(past_engagements = c(past_engagement_id, past_activity_player)) |> 
  mutate(past_engagements = map(past_engagements, deframe)) |> 
  inner_join(
    init_chained,
    by = c('round_id', 'engagement_id')
  ) |> 
  mutate(
    keep_data = map2_lgl(past_engagements, activity_opposer, ~..2 %in% ..1)
  ) |> 
  filter(keep_data) |> 
  select(-keep_data) |> 
  mutate(
    actual_past_engagements = map2(
      past_engagements,
      activity_opposer,
      ~which(..1 == ..2)
    ),
    past_engagement_id = map(actual_past_engagements, names)
  ) |> 
  select(
    round_id,
    engagement_id,
    pbp_side,
    seconds_elapsed,
    activity_player,
    activity_opposer,
    past_engagement_id
  ) |> 
  unnest(past_engagement_id)
actual_chains |> 
  distinct(
    past_engagement_id = engagement_id
  ) |> 
  inner_join(
    actual_chains |> 
      distinct(past_engagement_id),
    by = 'past_engagement_id'
  )


actual_chains |> 
  count(
    round_id,
    engagement_id,
    pbp_side,
    sort = TRUE
  ) |> 
  count(n)
actual_chains |> 
  filter(engagement_id == '2021-SND-039-02-d-2v1-Kill')
one_pbp |> 
  filter(round_id == '2021-SND-039-02') |> 
  glimpse()
actual_chains |> 
  mutate(z = map_int(past_engagement_id, length)) |> count(z)
both_pbp |> 
  filter(round_id == '2021-SND-117-09', team == 'SEA')


# past_chained |> count(engagement_id, sort = TRUE)
chained <- init_chained |> 
  left_join(
    past_chained |> 
      ## we don't actually need the past engagement ids! we just need to know how many
      group_by(round_id, engagement_id) |> 
      summarize(
        past_engagement_ids = list(past_engagement_id),
        past_activity_players = list(past_activity_player)
      ),
    by = c('round_id', 'engagement_id')
  ) |> 
  filter(
    activity_opposer %in% past_activity_players
  ) |> 
  mutate(
    n_past_engagements = map_int(past_engagement_id, length)
  )
chained |> arrange(desc(n_past_engagements))
chained |> 
  filter(
    engagement_id == '2021-SND-117-09-d-1v0-Kill'
  ) |> 
  select(engagement_id, past_engagement_id, activity_opposer, past_activity_players) |> 
  unnest(past_engagement_id, past_activity_players)
both_pbp |> 
  filter(round_id == '2021-SND-117-09', team == 'SEA')
past_chained |> 
  filter(round_id == '2021-SND-117-09') |> 
  group_by(round_id, engagement_id) |> 
  summarize(
    past_engagement_id = list(engagement_id),
    past_activity_players = list(activity_player)
  )

both_pbp |> 
  filter(round_id == '2021-SND-117-09') |> 
  filter(activity_player == 'Slacked')

both_pbp |> 
  filter(round_id == '2021-SND-117-09') |> 
  filter(activity_opposer == 'Slacked')

qs::qsave(chained, file.path('data', 'cod_snd_chains.qs'))
