library(tidyverse)
logs <- read_csv('data/logs.csv')
logs |> distinct(year, match_id, round)
logs |> distinct(year, map_id, round)

init <- logs |> 
  transmute(
    # year,
    # map_id, 
    # round,
    id = sprintf('%s-%s-%02d', year, map_id, round),
    side = ifelse(is_offense, 'o', 'd') |> factor(),
    # seconds_elapsed,
    # round_time_left,
    # bomb_timer_left,
    sec_left = ifelse(is.na(bomb_timer_left), round_time_left, bomb_timer_left),
    n_team_remaining,
    n_opponent_remaining,
    opponent_diff = n_team_remaining - n_opponent_remaining,
    activity,
    # weapon_or_bomb_site,
    win_round = ifelse(team == round_winner, 'yes', 'no') |> factor()
  ) |> 
  group_by(id) |> 
  mutate(
    bomb_planted = ifelse(lag(activity, default = 'no') == 'Plant', 'yes', 'no') |> factor(),
    sec_left = min(sec_left)
  ) |> 
  ungroup()

df <- init |> 
  filter(
    activity != 'Plant', ## we can learn this implicity with the bomb_planted feature
    activity != 'Defuse', ## round ending
    ## these are the round ending kills
    n_team_remaining > 0L,
    n_opponent_remaining > 0L
  )
df

max_sec <- c(seq(90, 45, -2.5), seq(45, 15, -2), seq(15, 3, -1), 1)
min_sec <- c(seq(70, 25, -2.5), seq(35, 5, -2), seq(12, 0, -1), 0)

ids <- df |> distinct(id) |> pull(id)
set.seed(42)
ids_trn <- sample(ids, size = 0.75 * length(ids))
trn <- df |> filter(id %in% ids_trn)
tst <- df |> filter(!id %in% ids_trn)

fit_window <- function(i, overwrite = TRUE) {
  i <- 1
  message(sprintf('window %d', i))
  
  path <- file.path('data', sprintf('wp-coefs-%02d.csv', i))
  if (file.exists(path) & !overwrite) {
    return(read_csv(path))
  }
  
  filt <- trn |> filter(sec_left >= min_sec[i], sec_left <= max_sec[i])
  undetected_round_outcomes <- trn |> 
    filter(id %in% unique(filt$id)) |> 
    distinct(id, side, win_round) |> 
    arrange(id, side, win_round)
  if (i == 1) {

    extra <- undetected_round_outcomes |> 
      mutate(
        opponent_diff = 0L,
        n_team_remaining = 4L,
        bomb_planted = 'no'
      )
  }
  fit <- glm(
    bomb_planted ~ side + opponent_diff + n_team_remaining + bomb_planted - 1, 
    data = filt, 
    family = binomial(link = 'logit')
  )
  
  coefs <- fit |>
    broom::tidy() |>
    mutate(min_sec = min_sec[i], max_sec = max_sec[i])
  write_csv(coefs, path)
  invisible(coefs)
}
