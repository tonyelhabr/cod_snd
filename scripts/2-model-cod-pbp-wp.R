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

both_pbp |> 
  # filter(side == 'd') |> 
  # distinct(round_id)
  distinct(side, round_id, win_round = ifelse(team == round_winner, 'w', 'l')) |> 
  count(side, win_round) |> 
  group_by(side) |>
  mutate(
    prop = n / sum(n)
  ) |> 
  ungroup()

both_pbp |>
  arrange(round_id, pre_plant_seconds_elapsed, post_plant_seconds_elapsed) |> 
  group_by(round_id, side) |> 
  mutate(
    across(c(n_team_remaining, n_opponent_remaining), list(prev = ~lag(.x, n = 1, default = 4L)))
  ) |> 
  ungroup() |> 
  filter(n_team_remaining_prev == 0L | n_opponent_remaining_prev == 0L, activity != 'Defuse') |> 
  count(activity, weapon_or_bomb_site)

round_win_prop_by_xvy <- both_pbp |>
  arrange(round_id, pre_plant_seconds_elapsed, post_plant_seconds_elapsed) |> 
  group_by(round_id, side) |> 
  mutate(
    across(c(n_team_remaining, n_opponent_remaining), list(prev = ~lag(.x, n = 1, default = 4L)))
  ) |> 
  ungroup() |> 
  filter(n_team_remaining_prev > 0L, n_opponent_remaining_prev > 0L) |> 
  count(side, n_team_remaining_prev, n_opponent_remaining_prev, win_round = ifelse(team == round_winner, 'w', 'l')) |> 
  group_by(side, n_team_remaining_prev, n_opponent_remaining_prev) |> 
  mutate(
    prop = n / sum(n)
  ) |> 
  ungroup()

wide_round_win_prop_by_xvy <- round_win_prop_by_xvy |>
  pivot_wider(
    names_from = win_round,
    values_from = c(n, prop),
    values_fill = list(n = 0L, prop = 0)
  )

both_pbp |> 
  filter(pbp_side == 'a', n_team_remaining == 0, n_opponent_remaining == 0) |> 
  glimpse()

init_model_pbp <- both_pbp |> 
  mutate(
    across(
      c(is_initial_bomb_carrier_killed, is_during_attempted_plant, is_during_attempted_defuse), 
      as.integer
    ),
    opponent_diff = n_team_remaining - n_opponent_remaining,
    win_round = ifelse(team == round_winner, 'yes', 'no') |> factor()
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
    round_has_plant,
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
    activity,
    is_post_plant,
    opponent_diff,
    killer_player,
    victim_player,
    is_negative_action
  )

## this will have duplicate records at the time of the plant, but that's fine
all_model_pbp <- bind_rows(
  init_model_pbp |> 
    filter(!is.na(pre_plant_seconds_elapsed)) |>
    mutate(is_pre_plant = TRUE, model_seconds_elapsed = pre_plant_seconds_elapsed) |> 
    select(-pre_plant_seconds_elapsed),
  init_model_pbp |> 
    filter(round_has_plant, !is.na(post_plant_seconds_elapsed)) |> 
    mutate(is_pre_plant = FALSE, model_seconds_elapsed = post_plant_seconds_elapsed) |> 
    select(-post_plant_seconds_elapsed)
)
## should have 0 rows
stopifnot(0 == (all_model_pbp |> filter(!is_pre_plant, !is_post_plant) |> distinct(round_id) |> nrow()))
## should only be plant activities
stopifnot(1 == (all_model_pbp |> filter(is_pre_plant, is_post_plant) |> count(activity) |> nrow()))

model_pbp <- all_model_pbp |> 
  filter(side == 'o') |> 
  filter(activity == 'Kill')

last_pre_plant_seconds_elapsed <- model_pbp |> 
  filter(is_pre_plant) |> 
  group_by(round_id) |> 
  slice_max(seconds_elapsed, n = 1) |> 
  ungroup() |> 
  select(round_id, round_has_plant, last_pre_plant_seconds_elapsed = seconds_elapsed)

last_post_plant_seconds_elapsed <- model_pbp |> 
  filter(round_has_plant, !is_pre_plant) |> 
  group_by(round_id) |> 
  slice_max(seconds_elapsed, n = 1) |> 
  ungroup() |> 
  select(round_id, last_post_plant_seconds_elapsed = seconds_elapsed)

pre_plant_model_pbp <- model_pbp |> 
  inner_join(
    last_pre_plant_seconds_elapsed,
    by = 'round_id'
  ) |> 
  filter(is_pre_plant) |> 
  select(-c(is_post_plant)) 

# pre_plant_seconds_coefs <- pre_plant_model_pbp |> 
#   estimate_window_coefs(is_pre_plant = TRUE, overwrite = TRUE)

## ----
library(tidymodels)

round_ids <- pre_plant_model_pbp |> distinct(round_id) |> pull(round_id)
set.seed(42)
round_ids_trn <- sample(round_ids, size = 0.75 * length(round_ids))
trn <- pre_plant_model_pbp |> filter(round_id %in% round_ids_trn)
tst <- pre_plant_model_pbp |> filter(!(round_id %in% round_ids_trn))

rec <- recipe(
  win_round ~ 
    seconds_elapsed +
    prev_opponent_diff +
    is_initial_bomb_carrier_killed +
    is_during_attempted_plant +
    is_during_attempted_defuse, 
  data = trn
)

spec <- boost_tree(mode = 'classification') |> 
  set_engine(engine = 'xgboost', monotone_constraints = c(1, -1, -1, 1, -1)) # 1, -1))
wf <- workflow(preprocessor = rec, spec = spec)
fit <- fit(wf, trn)
pre_plant_grid_preds <- generate_pred_grid(fit, is_pre_plant = TRUE)
pre_plant_grid_preds$wp <- predict(fit, pre_plant_grid_preds, type = 'prob')$`.pred_yes`

pre_plant_grid_preds <- generate_pred_grid(fit, is_pre_plant = TRUE)
pre_plant_grid_preds$wp <- predict(fit, pre_plant_grid_preds, type = 'prob')$`.pred_yes`

summarize_pred_grid_across_features <- function(df, col) {
  df |> 
    group_by(seconds_elapsed, prev_opponent_diff, value = .data[[col]]) |> 
    summarize(
      across(wp, mean)
    ) |> 
    ungroup()
}

c(
  'is_initial_bomb_carrier_killed',
  'is_during_attempted_defuse', 
  'is_during_attempted_plant'
) |> 
  set_names() |> 
  map_dfr(
    ~summarize_pred_grid_across_features(pre_plant_grid_preds, .x), 
    .id = 'feature'
  ) |> 
  unite('feature_value', feature, value) |> 
  ggplot() +
  aes(x = seconds_elapsed, y = wp) +
  geom_hline(
    color = 'white',
    aes(yintercept = 0.5)
  ) +
  geom_step(
    size = 1,
    aes(color = factor(prev_opponent_diff))
  ) +
  guides(
    color = guide_legend(
      title = 'Net # of players',
      override.aes = list(size = 3)
    )
  ) +
  ggsci::scale_color_tron() +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~feature_value, dir = 'v', nrow = 2) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(
    legend.position = 'top'
  ) +
  labs(
    title = 'Offensive Win Probability',
    x = 'Seconds Elapsed',
    y = 'Win Probability'
  )

pre_plant_grid_preds |> 
  filter(
    is_during_attempted_defuse == 0, 
    is_during_attempted_plant == 0, 
    is_initial_bomb_carrier_killed == 0
  ) |> 
  # count(seconds_elapsed, prev_opponent_diff)
  ggplot() +
  aes(x = seconds_elapsed, y = wp) +
  geom_hline(
    color = 'white',
    aes(yintercept = 0.5)
  ) +
  geom_step(
    size = 1.5,
    aes(group = factor(prev_opponent_diff), color = factor(prev_opponent_diff))
  ) +
  guides(
    color = guide_legend(
      title = 'Net # of players', 
      override.aes = list(size = 3)
    )
  ) +
  ggsci::scale_color_tron() +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(
    legend.position = 'top'
  ) +
  labs(
    title = 'Offensive Win Probability',
    x = 'Seconds Elapsed',
    y = 'Win Probability'
  )

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

post_plant_pbp <- model_pbp |> 
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


