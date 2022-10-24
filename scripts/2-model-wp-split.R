library(tidyverse)
logs <- read_csv('data/logs.csv')

## features: side, sec elapsed, opponent diff, number of teammates
## two models: pre- and post-plant
## with data, need to be careful with removing last row per year-map-round (i.e. id)

init <- logs |> 
  arrange(year, map_id, round, is_offense) |> 
  transmute(
    id = sprintf('%s-%s-%02d', year, map_id, round),
    side = ifelse(is_offense, 'o', 'd'),
    pre_plant_sec_elapsed = ifelse(!is.na(bomb_timer_left) & activity != 'Plant', NA_real_, ceiling(seconds_elapsed)),
    post_plant_sec_elapsed = ceiling(45L - bomb_timer_left),
    # n_team_remaining,
    n_opponent_remaining,
    opponent_diff = n_team_remaining - n_opponent_remaining,
    # activity,
    is_post_plant = ifelse(activity == 'Plant', 'yes', NA_character_),
    win_round = ifelse(team == round_winner, 'yes', 'no')
  ) |> 
  mutate(
    across(ends_with('plant_sec_elapsed'), as.integer)
  ) |> 
  group_by(id, side) |> 
  fill(is_post_plant, .direction = 'down') |> 
  mutate(
    across(is_post_plant, ~coalesce(.x, 'no'))
  ) |> 
  ungroup()
init |> filter(id == first(id))
init
init |> filter(id == '2021-SND-012-04')

plant_timings <- init |> 
  filter(!is.na(post_plant_sec_elapsed)) |> 
  group_by(id, side) |> 
  summarize(
    # across(ends_with('plant_sec_elapsed'), max, .names = 'last_{.col}')
    last_pre_plant_sec_elapsed = first(pre_plant_sec_elapsed),
    last_post_plant_sec_elapsed = max(post_plant_sec_elapsed)
  ) |> 
  ungroup()
plant_timings

has_plant <- init |> 
  group_by(id, side) |> 
  summarize(
    last_pre_plant_sec_elapsed = max(pre_plant_sec_elapsed),
    has_plant = any(is_post_plant == 'yes')
  ) |> 
  ungroup()
has_plant


grid <- crossing(
  id = unique(init$id),
  side = c('o', 'd'),
  pre_plant_sec_elapsed = 0L:90L
) |> 
  inner_join(
    has_plant |> select(id, last_pre_plant_sec_elapsed),
    by = 'id'
  ) |> 
  filter(pre_plant_sec_elapsed < last_pre_plant_sec_elapsed) |> 
  select(-last_pre_plant_sec_elapsed) |> 
  left_join(
    init |> select(id, side, pre_plant_sec_elapsed, n_opponent_remaining, opponent_diff, win_round),
    by = c('id', 'side', 'pre_plant_sec_elapsed')
  ) |> 
  arrange(id, side, pre_plant_sec_elapsed) |> 
  group_by(id, side) |> 
  fill(n_opponent_remaining, opponent_diff, win_round) |> 
  ungroup() |> 
  mutate(
    across(n_opponent_remaining, coalesce, 4L),
    across(opponent_diff, coalesce, 0L)
  )
grid
  filter(pre_plant_sec_lab)
