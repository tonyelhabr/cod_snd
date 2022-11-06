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

source('scripts/2-helpers.R')

raw <- read_csv('data/cod_snd_pbp.csv') |> 
  arrange(year, map_id, round) |> 
  mutate(
    across(side, factor),
    round_id = sprintf('%s-%s-%02d', year, map_id, round), 
    engagement_id = sprintf('%s-%s-%sv%s', round_id, side, n_team_remaining, n_opponent_remaining)
  )

plant_times <- raw |> 
  filter(activity == 'Plant') |> 
  select(round_id, plant_second = seconds_elapsed)

pbp <- raw |> 
  mutate(
    engagement_id,
    round_id,
    side,
    seconds_elapsed,
    # round_timer_left, ## need to keep around for timer fix
    pre_plant_seconds_elapsed = ifelse(
      !is.na(bomb_timer_left) & activity != 'Plant', 
      NA_real_, 
      seconds_elapsed
    ),
    post_plant_seconds_elapsed = 45L - bomb_timer_left,
    opponent_diff = n_team_remaining - n_opponent_remaining,
    is_post_plant = ifelse(activity == 'Plant', TRUE, NA),
    win_round = ifelse(team == round_winner, 'yes', 'no') |> factor()
  ) |> 
  arrange(round_id, pre_plant_seconds_elapsed, post_plant_seconds_elapsed) |> 
  group_by(round_id, side) |> 
  mutate(
    prev_opponent_diff = lag(opponent_diff, n = 1, default = 0L)
  ) |> 
  ungroup() |> 
  left_join(
    plant_times,
    by = 'round_id'
  ) |> 
  mutate(
    across(
      is_post_plant,
      ~case_when(
        is.na(plant_second) ~ FALSE,
        seconds_elapsed < plant_second ~ FALSE,
        TRUE ~ TRUE
      )
    )
  ) |> 
  select(
    engagement_id,
    round_id,
    side, 
    is_post_plant,
    seconds_elapsed,
    plant_second,
    pre_plant_seconds_elapsed,
    post_plant_seconds_elapsed,
    opponent_diff,
    prev_opponent_diff,
    win_round,
    
    ## extra
    killer_player,
    victim_player,
    is_negative_action,
    activity,
    killer_player,
    victim_player,
    is_negative_action
  )

kills_pbp <- pbp |> filter(activity == 'Kill')

# kills_pbp |> count(is_post_plant)
# kills_pbp |> count(side, is_post_plant)
# kills_pbp |> filter(is_post_plant) |> count(post_plant_time = is.na(pre_plant_seconds_elapsed), plant_time = (pre_plant_seconds_elapsed == plant_second))

## these are rounds when there are no kills pre-plant
# kills_pbp |> 
#   filter(is_post_plant) |> 
#   filter(!is.na(pre_plant_seconds_elapsed) & (pre_plant_seconds_elapsed != plant_second)) |> 
#   distinct(round_id)

## don't need side here
seconds_elapsed <- kills_pbp |> 
  group_by(round_id) |> 
  summarize(
    is_plant_in_round = any(is_post_plant),
    last_pre_plant_seconds_elapsed = max_quietly(pre_plant_seconds_elapsed, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  mutate(
    ## this happens when there are no kills before the plant
    across(last_pre_plant_seconds_elapsed, ~ifelse(is.infinite(.x), NA_integer_, .x))
  ) |> 
  left_join(
    kills_pbp |> 
      group_by(round_id) |> 
      filter(any(is_post_plant)) |> 
      summarize(
        last_post_plant_seconds_elapsed = max_quietly(post_plant_seconds_elapsed, na.rm = TRUE)
      ) |> 
      ungroup(),
    by = 'round_id'
  )

pre_plant_pbp <- kills_pbp |> 
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

pre_plant_model <- fit_wp_model(pre_plant_seconds_coefs)
pre_plant_grid_preds <- generate_wp_grid(pre_plant_model, is_pre_plant = TRUE)
pre_plant_grid_preds |> tail()
plot_wp_grid(pre_plant_grid_preds) +
  labs(title = 'Offense Pre-Plant Win Probability')
ggsave(
  filename = file.path('figs', 'pre_plant_offensive_wp.png'),
  width = 8,
  height = 6
)

post_plant_pbp <- kills_pbp |> 
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

base_point_df <- crossing(seconds_elapsed = 0L, opponent_diff = 0L, side = c('d', 'o'))
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
      pred_base_point |> select(side, default_wp = wp),
      by = 'side'
    ) |> 
    group_by(round_id, side) |> 
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


