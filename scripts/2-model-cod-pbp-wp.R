library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)
library(broom)

## TODO: 
##   0. Combine pre and post plant seconds elapsed terms into one column earlier.
##   1. Add term for whether bomb is down.
##   2. Possibly try features for number of ARs and SMGs left on each side.
##   3. Try terms for the game (e.g. "Cold War" or "Vanguard") and the map. (mixed-effects?)
##   3. Look into last 10 sec WPs more... shouldn't WP go down in the last 10 seconds or so pre-plant? Unless we know that they're planting.

source('scripts/helpers-plot.R')
source('scripts/helpers-wp.R')

both_pbp <- read_csv('data/cod_snd_pbp.csv', show_col_types = FALSE)

init_model_pbp <- both_pbp |> 
  mutate(
    across(
      c(is_initial_bomb_carrier_killed, is_during_attempted_plant, is_during_attempted_defuse), 
      as.integer
    ),
    opponent_diff = n_team_remaining - n_opponent_remaining,
    win_round = as.integer(team == round_winner)
  ) |> 
  arrange(round_id, pre_plant_seconds_elapsed, post_plant_seconds_elapsed) |> 
  group_by(round_id, side) |> 
  mutate(
    prev_opponent_diff = lag(opponent_diff, n = 1, default = 0L)
  ) |> 
  ungroup() |> 
  select(
    ## ids
    engagement_id,
    round_id,
    
    ## contextual
    pbp_side,
    side, 
    seconds_elapsed,
    plant_second,
    defuse_second,
    pre_plant_seconds_elapsed,
    post_plant_seconds_elapsed,
    
    ## features
    prev_opponent_diff,
    is_initial_bomb_carrier_killed,
    is_during_attempted_plant,
    is_during_attempted_defuse,
    
    ## outcome
    win_round,
    
    ## extra
    is_post_plant,
    opponent_diff,
    killer_player,
    victim_player,
    is_negative_action
  )

## TODO
## this will have duplicate records at the time of the plant, but that's fine imo
model_pbp <- bind_rows(
  init_model_pbp |> 
    filter(!is.na(pre_plant_seconds_elapsed)) |> 
    mutate(is_pre_plant = TRUE, model_seconds_elapsed = pre_plant_seconds_elapsed) |> 
    select(-pre_plant_seconds_elapsed),
  init_model_pbp |> 
    filter(!is.na(post_plant_seconds_elapsed)) |> 
    mutate(is_pre_plant = FALSE, model_seconds_elapsed = post_plant_seconds_elapsed) |> 
    select(-post_plant_seconds_elapsed)
)
## should have 0 rows
model_pbp |> filter(!is_pre_plant, !is_post_plant) |> distinct(round_id)

init_model_pbp |> 
  filter(round_id == '2021-SND-126-03') |> 
  glimpse()
model_pbp |> 
  filter(round_id == '2021-SND-126-03') |> 
  count(is_post_plant, is_pre_plant)
model_pbp |> 
  filter(round_id == '2021-SND-126-03') |> 
  filter(!is.na(post_plant_seconds_elapsed)) |> 
  mutate(is_pre_plant = FALSE, model_seconds_elapsed = post_plant_seconds_elapsed) |> 
  select(-post_plant_seconds_elapsed)
pbp |> filter(!is_pre_plant, !is_post_plant) |> count(round_id)

nonplant_pbp <- pbp |> filter(activity != 'Plant')

# nonplant_pbp |> count(is_post_plant)
# nonplant_pbp |> count(is_offense, is_post_plant)
# nonplant_pbp |> filter(is_post_plant) |> count(post_plant_time = is.na(pre_plant_seconds_elapsed), plant_time = (pre_plant_seconds_elapsed == plant_second))

## these are rounds when there are no kills pre-plant
# nonplant_pbp |> 
#   filter(is_post_plant) |> 
#   filter(!is.na(pre_plant_seconds_elapsed) & (pre_plant_seconds_elapsed != plant_second)) |> 
#   distinct(round_id)

## don't need is_offense here
seconds_elapsed <- nonplant_pbp |> 
  group_by(round_id) |> 
  summarize(
    is_plant_in_round = any(!is_pre_plant),
    last_pre_plant_seconds_elapsed = max_quietly(pre_plant_seconds_elapsed, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  mutate(
    ## this happens when there are no kills before the plant
    across(last_pre_plant_seconds_elapsed, ~ifelse(is.infinite(.x), NA_integer_, .x))
  ) |> 
  left_join(
    nonplant_pbp |> 
      group_by(round_id) |> 
      filter(any(is_post_plant)) |> 
      summarize(
        last_post_plant_seconds_elapsed = max_quietly(post_plant_seconds_elapsed, na.rm = TRUE)
      ) |> 
      ungroup(),
    by = 'round_id'
  )

pre_plant_pbp <- nonplant_pbp |> 
  inner_join(
    seconds_elapsed |> 
      select(round_id, is_plant_in_round, last_seconds_elapsed = last_pre_plant_seconds_elapsed),
    by = 'round_id'
  ) |> 
  rename(
    round_seconds_elapsed = seconds_elapsed, ## this combines both pre- and post-plant
    seconds_elapsed = pre_plant_seconds_elapsed
  ) |> 
  filter(!is_post_plant) |> 
  select(-c(post_plant_seconds_elapsed, is_post_plant)) 

pre_plant_seconds_coefs <- pre_plant_pbp |> 
  filter(!is.na(seconds_elapsed)) |> ## drop post-plant records
  # filter(!(!is_plant_in_round & (seconds_elapsed == last_seconds_elapsed))) |> ## drop the last kill when all kills are pre-plant
  select(-last_seconds_elapsed) |> 
  estimate_window_coefs(is_pre_plant = TRUE, overwrite = TRUE)

## ----
library(tidymodels)

o_pre_plant_grid <- pre_plant_pbp |> 
  filter(side == 'o') |> 
  select(-side)

seconds_to_exclude <- o_pre_plant_grid |> 
  group_by(id) |> 
  slice_max(pre_plant_sec_elapsed, n = 1) |> 
  ungroup()

o_pre_plant_grid <- o_pre_plant_grid |> 
  anti_join(seconds_to_exclude, by = c('id', 'pre_plant_sec_elapsed'))

ids <- o_pre_plant_grid |> distinct(id) |> pull(id)
set.seed(42)
ids_trn <- sample(ids, size = 0.75 * length(ids))
trn <- o_pre_plant_grid |> filter(id %in% ids_trn)
tst <- o_pre_plant_grid |> filter(!id %in% ids_trn)

rec <- recipe(win_round ~ pre_plant_sec_elapsed + opponent_diff, data = trn)
spec <- boost_tree(mode = 'classification') |> 
  set_engine(engine = 'xgboost', monotone_constraints = c(1, -1))
wf <- workflow(preprocessor = rec, spec = spec)
fit <- fit(wf, trn)
fit$fit$fit$fit

## ----
pre_plant_model <- fit_wp_model(pre_plant_seconds_coefs)
pre_plant_grid_preds <- generate_wp_grid(pre_plant_model, is_pre_plant = TRUE)
pre_plant_grid_preds |> tail(20)
pre_plant_grid_preds |> 
  filter(is_offense == 1L, is_initial_bomb_carrier_killed == 0L, is_during_attempted_plant == 0L) |> 
  plot_wp_grid() +
  labs(title = 'Offense Pre-Plant Win Probability')
ggsave(
  filename = file.path('figs', 'pre_plant_offensive_wp.png'),
  width = 8,
  height = 6
)

post_plant_pbp <- nonplant_pbp |> 
  inner_join(
    seconds_elapsed |> 
      filter(is_plant_in_round) |> 
      distinct(round_id, last_seconds_elapsed = last_post_plant_seconds_elapsed),
    by = 'round_id'
  ) |> 
  rename( 
    round_seconds_elapsed = seconds_elapsed,
    seconds_elapsed = post_plant_seconds_elapsed
  ) |> 
  filter(is_post_plant) |> 
  select(-c(pre_plant_seconds_elapsed, is_post_plant)) 

post_plant_seconds_coefs <- post_plant_pbp |> 
  filter(seconds_elapsed != last_seconds_elapsed) |> ## drop the last kill
  select(-last_seconds_elapsed) |> 
  estimate_window_coefs(is_pre_plant = FALSE, overwrite = TRUE)

post_plant_model <- fit_wp_model(post_plant_seconds_coefs)
post_plant_grid_preds <- generate_wp_grid(post_plant_model, is_pre_plant = FALSE)
plot_wp_grid(post_plant_grid_preds) +
  labs(title = 'Offense Post-Plant Win Probability')
ggsave(
  filename = file.path('figs', 'post_plant_offensive_wp.png'),
  width = 8,
  height = 6
)

## usage ----
augment.cod_wp_model <- function(x, data, ...) {
  data$wp <- predict(
    x,
    data,
    ...
  )
  data
}

base_point_df <- crossing(seconds_elapsed = 0L, opponent_diff = 0L, is_offense = c('yes', 'no'))
pred_base_points <- bind_rows(
  augment(pre_plant_model, base_point_df) |> mutate(is_pre_plant = TRUE),
  augment(post_plant_model, base_point_df) |> mutate(is_pre_plant = FALSE)
)

add_wpa <- function(data, is_pre_plant = TRUE) {
  pred_base_point <- pred_base_points |> filter(is_pre_plant == !!is_pre_plant)
  data |> 
    mutate(
      across(
        wp,
        ~case_when(
          !is.na(.x) ~ .x,
          win_round == 'yes' ~ 1,
          win_round == 'no' ~ 0
        )
      )
    ) |> 
    left_join(
      pred_base_point |> select(is_offense, default_wp = wp),
      by = 'is_offense'
    ) |> 
    group_by(round_id, is_offense) |> 
    mutate(
      wpa = wp - coalesce(lag(wp), default_wp)
    ) |> 
    ungroup() |> 
    select(-default_wp)
}

pre_plant_pbp <- pre_plant_model |> 
  augment(pre_plant_pbp) |> 
  add_wpa(is_pre_plant = TRUE)

post_plant_pbp <- post_plant_model |> 
  augment(post_plant_pbp) |> 
  add_wpa(is_pre_plant = FALSE)


pbp_wp <- bind_rows(
  pre_plant_pbp,
  post_plant_pbp
)

## summarize ----
pbp_wp |> filter(round_id == '2021-SND-011-01')

full_join(
  pbp_wp |> 
    group_by(player = killer_player, is_post_plant) |> 
    summarize(
      kills = n(),
      wpa_kills = sum(wpa)
    ) |> 
    ungroup(),
  pbp_wp |> 
    group_by(player = victim_player, is_post_plant) |> 
    summarize(
      deaths = n(),
      wpa_deaths = sum(-wpa)
    ) |> 
    ungroup(),
  by = c('player', 'is_post_plant')
) |> 
  mutate(
    across(is_post_plant, ~ifelse(.x, 'post_plant', 'pre_plant'))
  ) |> 
  pivot_wider(
    names_from = is_post_plant,
    values_from = c(kills, wpa_kills, deaths, wpa_deaths)
  ) |> 
  mutate(
    wpa_total = wpa_kills_pre_plant + wpa_kills_post_plant + wpa_deaths_pre_plant + wpa_deaths_post_plant
  ) |> 
  arrange(desc(wpa_total))

pbp_wp |> 
  filter(killer_player == 'Shotzzy') |> 
  ggplot() +
  aes(x = wpa) +
  geom_histogram()


