library(tidyverse)
logs <- read_csv('data/logs.csv')

## two models: pre- and post-plant
## with data, need to be careful with removing last row per year-map-round (i.e. id)

init <- logs |> 
  arrange(year, map_id, round, is_offense) |> 
  transmute(
    id = sprintf('%s-%s-%02d', year, map_id, round),
    # idx = row_number(),
    side = ifelse(is_offense, 'o', 'd') |> factor(),
    pre_plant_sec_elapsed = ifelse(
      !is.na(bomb_timer_left) & activity != 'Plant', 
      NA_real_, 
      ceiling(seconds_elapsed)
    ),
    post_plant_sec_elapsed = ceiling(45L - bomb_timer_left),
    opponent_diff = n_team_remaining - n_opponent_remaining,
    is_post_plant = ifelse(activity == 'Plant', 'yes', NA_character_),
    win_round = ifelse(team == round_winner, 'yes', 'no') |> factor()
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

init |> 
  distinct(id, side, win_round) |> 
  count(win_round)

has_plant <- init |> 
  group_by(id) |> 
  summarize(
    last_pre_plant_sec_elapsed = max(pre_plant_sec_elapsed),
    has_plant = any(is_post_plant == 'yes')
  ) |> 
  ungroup()

## to prove that we don't need to do `crossing(side = ...)`
# init |> 
#   group_by(id) |> 
#   summarize(has_both = any(side == 'o') & any(side == 'd')) |> 
#   ungroup() |> 
#   count(has_both)

pre_plant_grid <- init |> 
  distinct(id, side, win_round) |> 
  crossing(
    pre_plant_sec_elapsed = 0L:90L
  ) |> 
  inner_join(
    has_plant |> select(id, last_pre_plant_sec_elapsed),
    by = 'id'
  ) |> 
  filter(pre_plant_sec_elapsed < last_pre_plant_sec_elapsed) |> 
  select(-last_pre_plant_sec_elapsed) |> 
  left_join(
    init |> select(id, side, pre_plant_sec_elapsed, opponent_diff, win_round),
    by = c('id', 'side', 'pre_plant_sec_elapsed', 'win_round')
  ) |> 
  arrange(id, side, pre_plant_sec_elapsed) |> 
  group_by(id, side) |> 
  fill(opponent_diff, win_round) |> 
  ungroup() |> 
  mutate(
    across(opponent_diff, coalesce, 0L)
  )
pre_plant_grid

fit_pre_plant_wp_model <- function(grid, overwrite = FALSE) {
  
  path <- file.path('data', 'pre-plant-wp-coefs.csv')
  if (file.exists(path) & isFALSE(overwrite)) {
    return(read_csv(path))
  }
  min_sec <- c(seq(0, 30, 3), seq(30, 50, 2), seq(50, 75, 1), seq(75, 85, 1))
  max_sec <- c(seq(15, 45, 3), seq(40, 60, 2), seq(60, 85, 1), seq(80, 90, 1))
  ids <- pre_plant_grid |> distinct(id) |> pull(id)
  set.seed(42)
  ids_trn <- sample(ids, size = 0.75 * length(ids))
  trn <- grid |> filter(id %in% ids_trn)
  tst <- grid |> filter(!id %in% ids_trn)
  
  fit_window <- function(i, overwrite = TRUE) {

    path <- file.path('data', sprintf('pre-plant-wp-coefs-%02d.csv', i))
    if (file.exists(path) & !overwrite) {
      return(read_csv(path))
    }
    
    filt <- trn |> 
      filter(side == 'o', pre_plant_sec_elapsed >= min_sec[i], pre_plant_sec_elapsed <= max_sec[i])
    fit <- glm(
      win_round ~ opponent_diff - 1, 
      data = filt, 
      family = binomial(link = 'logit')
    )
    
    coefs <- fit |>
      broom::tidy() |>
      mutate(
        min_sec = min_sec[i],
        max_sec = max_sec[i]
      )
    write_csv(coefs, path)
    invisible(coefs)
  }
  
  coefs <- 1:length(max_sec) |> 
    map_dfr(fit_window, overwrite = TRUE)
  write_csv(coefs, path)
  coefs
}

pre_plant_coefs <- fit_pre_plant_wp_model(pre_plant_grid, overwrite = TRUE)
wide_pre_plant_coefs <- pre_plant_coefs |> 
  transmute(
    term,
    across(estimate, round, 3),
    min_sec,
    max_sec
  ) |> 
  distinct() |> 
  pivot_wider(
    names_from = term,
    values_from = estimate
  )

pre_plant_coefs |>
  ggplot() +
  aes(x = min_sec, y = estimate, group = term) +
  facet_wrap(~term, ncol = 1, scales = 'free_y') +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_point(size = 0.5) +
  geom_ribbon(
    aes(ymax = estimate + 2 * std.error, ymin = estimate - 2 * std.error),
    alpha = 0.5
  ) +
  geom_smooth(
    method = 'loess',
    formula = 'y~x'
  ) +
  labs(
    x = 'Seconds Left',
    y = 'Coefficient Estimate',
    title = 'Win Probability Model Coefficients Over Time'
  )

opponent_diff_fit <- loess(
    estimate ~ min_sec, 
    data = filter(pre_plant_coefs, term == 'opponent_diff'),
    span = 0.5
  )

logit <- function(x) {
  ex <- exp(x)
  case_when(
    ex == Inf ~ 1,
    ex == -Inf ~ 0,
    TRUE ~ ex / (1 + ex)
  )
}

compute_wp <- function(pre_plant_sec_elapsed, opponent_diff, side) {

  opponent_diff_pred <- predict(opponent_diff_fit, newdata = pre_plant_sec_elapsed)
  side_multiplier <- ifelse(side == 'o', 1L, -1L)
  log_odds <- side_multiplier * opponent_diff_pred * opponent_diff
  
  logit(log_odds)
}

pred_grid <- crossing(
  pre_plant_sec_elapsed = 0L:89L,
  opponent_diff = -3L:3L
) |> 
  mutate(
    side = 'o'
  )

pred_grid$wp = compute_wp(pred_grid$pre_plant_sec_elapsed, pred_grid$opponent_diff, pred_grid$side)
pred_grid |> 
  ggplot() +
  aes(x = pre_plant_sec_elapsed, y = wp) +
  geom_step(
    aes(color = factor(opponent_diff))
  )

## xgb ----
library(tidymodels)

o_pre_plant_grid <- pre_plant_grid |> 
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


## post plant ----
# plant_timings <- init |> 
#   filter(!is.na(post_plant_sec_elapsed)) |> 
#   group_by(id, side) |> 
#   summarize(
#     last_pre_plant_sec_elapsed = first(pre_plant_sec_elapsed),
#     last_post_plant_sec_elapsed = max(post_plant_sec_elapsed)
#   ) |> 
#   ungroup()
