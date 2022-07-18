library(tidyverse)
library(RSQLite)
library(DBI)
library(janitor)
library(jsonlite)

## https://www.kaggle.com/datasets/visualize25/valorant-pro-matches-full-data?resource=download
con <- dbConnect(RSQLite::SQLite(), 'data/valorant.sqlite')
query_db <- function(conn, statement) {
  DBI::dbGetQuery(conn = conn, statement = statement) |> 
    tibble() |> 
    janitor::clean_names()
}

## for some reason there is no mapping of team ids -> team_abbrv, which is needed to identify round winners properly.
## we have to make it ourselves (see `team_mapping`)
init_val_games <- query_db(con, 'select * from Games')
val_matches <- query_db(con, 'select * from Matches')
val_scoreboard <- query_db(con, 'select * from Game_Scoreboard')
raw_val_rounds <- query_db(con, 'select * from Game_Rounds')

clean_val_json <- function(x) {
  x |> 
    str_replace_all("\\'", '\\"') |> 
    str_replace_all('([A-z0-9]+)[:]', '"\\1":') |> 
    fromJSON()
}
possibly_clean_json <- possibly(clean_val_json, otherwise = NULL)

init_val_rounds <- raw_val_rounds |> 
  mutate(
    round_history = map(round_history, possibly_clean_json)
  ) |> 
  unnest_longer(round_history) |> 
  unnest_wider(round_history) |> 
  clean_names() |> 
  mutate(
    across(round_winner, str_squish)
  ) |> 
  mutate(
    round = as.integer(round_history_id),
    .before = 'round_winner'
  ) |> 
  select(-round_history_id) |> 
  filter(!is.na(round)) |> 
  mutate(
    across(
      score_after_round,
      list(
        cumu_w = ~str_remove(.x, '-.*$') |> as.integer(),
        cumu_l = ~str_remove(.x, '^.*-') |> as.integer()
      ),
      .names = '{fn}'
    )
  )

init_val_series_outcomes <- init_val_rounds |> 
  group_by(game_id) |> 
  slice_max(round, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  select(game_id, round, cumu_w, cumu_l) |> 
  mutate(
    max_cumu_wl = ifelse(cumu_w > cumu_l, cumu_w, cumu_l),
    min_cumu_wl = ifelse(cumu_w > cumu_l, cumu_l, cumu_w)
  )

weird_val_series_outcomes <- init_val_series_outcomes |> 
  filter(max_cumu_wl > 13 & (min_cumu_wl < (max_cumu_wl - 2)))

val_series_outcomes <- init_val_series_outcomes |> 
  anti_join(
    weird_val_series_outcomes |> select(game_id),
    by = 'game_id'
  )

val_games <- init_val_games |> 
  inner_join(
    val_series_outcomes |> select(game_id),
    by = 'game_id'
  )

val_team_abbrvs <- val_scoreboard |> 
  inner_join(
    val_series_outcomes |> select(game_id),
    by = 'game_id'
  ) |> 
  distinct(game_id, team_abbrv = team_abbreviation) |> 
  mutate(across(team_abbrv, ~toupper(.x) |> str_squish()))

raw_val_team_mapping <- bind_rows(
  val_games |>
    distinct(game_id, team_id = team1id, team = team1),
  val_games |>
    distinct(game_id, team_id = team2id, team = team2)
) |>
  distinct(game_id, team_id, team) |>
  arrange(team_id) |> 
  inner_join(val_team_abbrvs, by = 'game_id') |> 
  distinct(game_id, team_id, team, team_abbrv) |> 
  count(team_id, team, team_abbrv) |> 
  group_by(team_id, team) |> 
  slice_max(n, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  arrange(desc(n))

val_team_mapping <- raw_val_team_mapping |> 
  filter(team_abbrv != '', n >= 5)

init_val_rounds_side <- init_val_rounds |> 
  inner_join(
    val_series_outcomes |> select(game_id),
    by = 'game_id'
  ) |> 
  inner_join(
    val_team_mapping |> select(team1id = team_id, team_1 = team_abbrv), 
    by = 'team1id'
  ) |> 
  inner_join(
    val_team_mapping |> select(team2id = team_id, team_2 = team_abbrv), 
    by = 'team2id'
  ) |> 
  inner_join(
    val_games |> 
      inner_join(
        val_team_mapping |> select(winner = team, winner_abbrv = team_abbrv),
        by = 'winner'
      ) |> 
      transmute(
        game_id, 
        map,
        team1_side_first_half,
        total_rounds = team1_total_rounds + team2_total_rounds,
        series_winner = winner_abbrv
      ) |> 
      distinct(),
    by = 'game_id'
  ) |> 
  transmute(
    game_id,
    team_1,
    team_2,
    map,
    round,
    cumu_w,
    cumu_l,
    team1_side_first_half,
    score_after_round,
    # round_winner,
    # series_winner,
    # total_rounds,
    is_offense_1 = case_when(
      round <= 13 & team1_side_first_half == 'attack' ~ TRUE,
      round > 13 & team1_side_first_half == 'defend' ~ TRUE,
      TRUE ~ FALSE
    ),
    win_round_1 = team_1 == round_winner,
    win_series_1 = team_1 == series_winner
  ) |> 
  arrange(game_id, round) |> 
  group_by(game_id) |> 
  mutate(
    pre_cumu_w = lag(cumu_w, default = 0L),
    pre_cumu_l = lag(cumu_l, default = 0L)
  ) |> 
  ungroup()

val_rounds <- bind_rows(
  init_val_rounds_side |> 
    select(
      game_id,
      team = team_1,
      opponent = team_2,
      map,
      round,
      is_offense = is_offense_1,
      win_round = win_round_1,
      win_series = win_series_1,
      pre_cumu_w,
      pre_cumu_l,
      cumu_w,
      cumu_l,
    ),
  init_val_rounds_side |> 
    transmute(
      game_id,
      team = team_2,
      opponent = team_1,
      map,
      round,
      is_offense = !is_offense_1,
      win_round = !win_round_1,
      win_series = !win_series_1,
      pre_cumu_w = pre_cumu_l,
      cumu_w = cumu_l
    ) |> 
    mutate(
      pre_cumu_l = round - pre_cumu_w,
      cumu_l = round - cumu_w
    )
) |> 
  arrange(game_id, round, is_offense)
qs::qsave(val_rounds, 'valorant_rounds.qs')
