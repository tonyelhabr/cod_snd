library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

raw_pbp <- read_csv('data/logs.csv') |> 
  filter(is_offense) |> 
  arrange(year, map_id, round) |> 
  transmute(
    id = sprintf('%s-%s-%02d', year, map_id, round),
    # round_timer_left, ## need to keep around for timer fix
    pre_plant_seconds_elapsed = ifelse(
      !is.na(bomb_timer_left) & activity != 'Plant', 
      NA_real_, 
      ceiling(seconds_elapsed)
    ),
    post_plant_seconds_elapsed = ceiling(45L - bomb_timer_left),
    opponent_diff = n_team_remaining - n_opponent_remaining,
    is_post_plant = activity == 'Plant',
    win_round = ifelse(team == round_winner, 'yes', 'no') |> factor()
  ) |> 
  mutate(
    across(ends_with('plant_seconds_elapsed'), as.integer)
  ) |> 
  group_by(id) |> 
  fill(is_post_plant, .direction = 'down') |> 
  mutate(
    across(is_post_plant, ~coalesce(.x, FALSE)) # ,
    ## potential fixing rounds
    # post_plant_seconds_elapsed = ifelse(is_post_plant == 'yes' & !is.na(pre_plant_seconds_elapsed), ceiling(45L - round_timer_left))
  ) |> 
  ungroup()

## One of these has a weird scenario where the kill happens at the same second as the plant.
##   I believe the others have issues with the raw data, where round_timer_left has seconds after the plant, but bomb_timer_left doesn't.
##   These can be reasonably fixed with some smart logic, but the easiest solution is to just drop those rounds.
bad_ids <- raw_pbp |> 
  group_by(id) |> 
  filter(any(is_post_plant == 'yes')) |> 
  filter(all(is.na(post_plant_seconds_elapsed))) |> 
  ungroup() |> 
  distinct(id)

pbp <- raw_pbp |> anti_join(bad_ids, by = 'id')

max_quietly <- function(...) {
  f <- quietly(max)
  f(...)$result
}

seconds_elapsed <- pbp |> 
  group_by(id) |> 
  summarize(
    last_pre_plant_seconds_elapsed = max_quietly(pre_plant_seconds_elapsed, na.rm = TRUE),
    seconds_elapsed = any(is_post_plant)
  ) |> 
  ungroup() |> 
  mutate(
    ## this happens when there are no kills before the plant
    across(last_pre_plant_seconds_elapsed, ~ifelse(is.infinite(.x), NA_integer_, .x))
  ) |> 
  left_join(
    pbp |> 
      group_by(id) |> 
      filter(any(is_post_plant == 'yes')) |> 
      summarize(
        last_post_plant_seconds_elapsed = max_quietly(post_plant_seconds_elapsed, na.rm = TRUE)
      ) |> 
      ungroup(),
    by = 'id'
  )

compute_wp <- function(fit, seconds_elapsed, opponent_diff, is_offense = TRUE) {
  
  pred <- predict(fit, newdata = seconds_elapsed)
  side_multiplier <- ifelse(is_offense, 1L, -1L)
  log_odds <- side_multiplier * pred * opponent_diff
  
  plogis(log_odds) ## logit
}

generate_seconds_grid <- function(is_pre_plant = TRUE) {
  if (isTRUE(is_pre_plant)) {
    min_sec <- c(seq(0, 30, 3), seq(30, 50, 2), seq(50, 75, 1), seq(75, 85, 1), seq(86, 90))
    max_sec <- c(seq(15, 45, 3), seq(40, 60, 2), seq(60, 85, 1), seq(80, 90, 1), rep(90, 5))
  } else {
    min_sec <- c(seq(0, 30, 2), seq(30, 38, 1), seq(39, 45))
    max_sec <- c(seq(10, 40, 2), seq(37, 45, 1), rep(45, 7))
  }
  tibble(
    min_sec = min_sec,
    max_sec = max_sec
  )
}

get_max_second <- function(is_pre_plant = TRUE) {
  ifelse(isTRUE(is_pre_plant), 90L, 45L)
}

estimate_window_coefs <- function(pbp, is_pre_plant = TRUE) {
  
  max_second <- get_max_second(is_pre_plant)
  init_pbp_grid <- pbp |> 
    distinct(id, win_round, last_sec_elapsed) |> 
    crossing(
      seconds_elapsed = 0L:max_second
    ) |> 
    filter(seconds_elapsed < last_sec_elapsed) |> 
    select(-last_sec_elapsed) |> 
    left_join(
      df |> select(id, seconds_elapsed, opponent_diff),
      by = c('id', 'seconds_elapsed')
    ) |> 
    arrange(id, seconds_elapsed) |> 
    group_by(id) |> 
    fill(opponent_diff, win_round) |> 
    ungroup() |> 
    mutate(
      across(opponent_diff, coalesce, 0L)
    )
  
  ## the last kill is data leakage
  seconds_to_exclude <- init_pbp_grid |> 
    group_by(id) |> 
    slice_max(seconds_elapsed, n = 1) |> 
    ungroup()
  
  init_pbp_grid |> 
    anti_join(seconds_to_exclude, by = c('id', 'seconds_elapsed'))
}

estimate_window_coefs <- function(pbp_grid, is_pre_plant = TRUE, overwrite = FALSE) {
  
  prefix <- ifelse(is_pre_plant, 'pre', 'post')
  path <- file.path('data', sprintf('%s_plant_wp_coefs.csv', prefix))
  if (file.exists(path) & isFALSE(overwrite)) {
    return(read_csv(path))
  }
  
  seconds_grid <- generate_seconds_grid(is_pre_plant = is_pre_plant)
  
  coefs <- seconds_grid |>
    mutate(
      opponent_diff = 
        map2_dbl(
          min_sec, max_sec,
          ~{
            filt <- pbp_grid |> 
              filter(seconds_elapsed >= ..1, seconds_elapsed <= ..2)
            
            fit <-glm(
              win_round ~ opponent_diff - 1, 
              data = filt, 
              family = binomial(link = 'logit')
            )
            coefs <- coefficients(fit)
            coefs[["opponent_diff"]]
          }
        )
    )
  write_csv(coefs, path)
  coefs
}

fit_model <- function(data) {
  loess(
    opponent_diff ~ min_sec, 
    data = data,
    span = 0.5
  )
}

generate_wp_grid <- function(fit, is_pre_plant = TRUE) {
  max_second <- get_max_second(is_pre_plant)
  
  pred_grid <- crossing(
    seconds_elapsed = 0L:(max_second - 1L),
    opponent_diff = 1L:3L
  )
  
  pred_grid$wp <- compute_wp(
    fit,
    seconds_elapsed = pred_grid$seconds_elapsed,
    opponent_diff = pred_grid$opponent_diff,
    is_offense = TRUE
  )
  
  pred_grid
}

plot_wp_grid <- function(pred_grid) {
  pred_grid |> 
    ggplot() +
    aes(x = seconds_elapsed, y = wp) +
    geom_step(
      aes(color = factor(opponent_diff))
    )
}

pre_plant_seconds_coefs <- pbp |> 
  inner_join(
    seconds_elapsed |> select(id, last_sec_elapsed = last_pre_plant_seconds_elapsed),
    by = 'id'
  ) |> 
  rename(
    seconds_elapsed = pre_plant_seconds_elapsed
  ) |> 
  estimate_window_coefs(is_pre_plant = TRUE, overwrite = TRUE)

pre_plant_fit <- fit_model(pre_plant_seconds_coefs)
pre_plant_preds <- generate_wp_grid(pre_plant_fit, is_pre_plant = TRUE)
plot_wp_grid(pre_plant_preds)

post_plant_seconds_coefs <- pbp |> 
  inner_join(
    seconds_elapsed |> 
      filter(seconds_elapsed) |> 
      distinct(id, last_sec_elapsed = last_post_plant_seconds_elapsed),
    by = 'id'
  ) |> 
  rename(
    seconds_elapsed = post_plant_seconds_elapsed
  ) |> 
  estimate_window_coefs(is_pre_plant = FALSE, overwrite = TRUE)

post_plant_fit <- fit_model(post_plant_seconds_coefs)
post_plant_preds <- generate_wp_grid(post_plant_fit, is_pre_plant = FALSE)
plot_wp_grid(post_plant_preds)

bind_rows(
  pre_plant_preds |> mutate(is_pre_plant = TRUE),
  post_plant_preds |> mutate(across(seconds_elapsed, ~.x + 90L), is_pre_plant = FALSE)
) |> 
  plot_wp_grid() +
  geom_vline(
    aes(xintercept = 90L)
  ) +
  coord_cartesian(ylim = c(0.5, 1))

