library(tidyverse)
logs <- read_csv('data/logs.csv')

# features: side, sec elapsed, opponent diff, number of teammates
# two models: pre- and post-plant

to_yes_no <- function(...) {
  ifelse(..., 'yes', 'no')
}

init <- logs |> 
  transmute(
    id = sprintf('%s-%s-%02d', year, map_id, round),
    side = ifelse(is_offense, 'o', 'd') |> factor(),
    pre_plant_sec_elapsed = seconds_elapsed,
    post_plant_sec_elapsed = 45L - bomb_timer_left,
    n_team_remaining,
    n_opponent_remaining,
    opponent_diff = n_team_remaining - n_opponent_remaining,
    activity,
    is_post_plant = to_yes_no(activity == 'Plant') |> factor(),
    win_round = to_yes_no(team == round_winner)
  ) |> 
  group_by(id) |> 
  fill(is_post_plant, .direction = 'down') |> 
  mutate(
    across(is_post_plant, ~coalesce(.x, 'no'))
  ) |> 
  ungroup()
init

has_plant <- init |> 
  group_by(id) |> 
  summarize(
    last_pre_plant_sec_elapsed = max(pre_plant_sec_elapsed),
    has_plant = any(activity == 'Plant')
  ) |> 
  ungroup()
has_plant

crossing(
  id = unique(init$id),
  pre_plant_sec_elapsed = 0L:90L
)
