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
val_games <- query_db(con, 'select * from Games') |> 
  mutate(
    across(c(game_id, match_id), as.integer)
  )

val_matches <- query_db(
  con, 
  "
  select * from Matches
  where eventName like '%Champions Tour%'
  "
) |> 
  mutate(
    across(c(match_id, event_id), as.integer),
    across(date, ~lubridate::ymd_hms(.x) |> lubridate::date())
  )

val_scoreboard <- query_db(
  con, 
  'select * from Game_Scoreboard'
) |> 
  mutate(
    across(c(game_id, player_id), as.integer)
  )

raw_val_rounds <- query_db(
  con, 
  'select * from Game_Rounds'
) |> 
  mutate(
    across(game_id, as.integer)
  )
dbDisconnect(con)

clean_val_json <- function(x) {
  x |> 
    str_replace_all("\\'", '\\"') |> 
    str_replace_all('([A-z0-9]+)[:]', '"\\1":') |> 
    fromJSON()
}
possibly_clean_json <- possibly(clean_val_json, otherwise = NULL)

init_val_rounds_side <- raw_val_rounds |> 
  inner_join(
    val_games |> distinct(game_id, match_id),
    by = 'game_id'
  ) |> 
  semi_join(
    val_matches |> distinct(match_id),
    by = 'match_id'
  ) |> 
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

## note that the notion of a series may be confusing. in valorant, we typically think
##  of a match/game as the first to 13 round wins, and the series as the first to 2 match/game/map wins.
##  however, i'm treating match/game/map as a "series" since we're not really interested in cumulative
##  match/game/map wins between 2 teams.
val_series_outcomes <- init_val_rounds_side |>
  group_by(game_id) |>
  slice_max(round, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(game_id, round, series_winner = round_winner, cumu_w, cumu_l) |>
  mutate(
    max_cumu_wl = ifelse(cumu_w > cumu_l, cumu_w, cumu_l),
    min_cumu_wl = ifelse(cumu_w > cumu_l, cumu_l, cumu_w)
  )

init_weird_val_series_outcomes <- bind_rows(
  val_series_outcomes |>
    filter(max_cumu_wl > 13 & (min_cumu_wl < (max_cumu_wl - 2))),
  val_series_outcomes |> 
    filter(max_cumu_wl < 13)
)

val_team_mapping <- bind_rows(
  init_val_rounds_side |> 
    group_by(game_id) |> 
    filter(lag(cumu_w) != cumu_w) |> 
    ungroup() |> 
    distinct(game_id, team_id = team1id, team = round_winner),
  init_val_rounds_side |> 
    group_by(game_id) |>
    filter(lag(cumu_l) != cumu_l) |> 
    ungroup() |> 
    distinct(game_id, team_id = team2id, team = round_winner)
) |> 
  distinct(game_id, team_id, team) |> 
  arrange(game_id, team_id)

## note that i've decided to use val_rounds -> val_series_outcomes for determining the last round
##   instead of val_matches. this is more self coherent and i think val_matches has some errors.
##   when there is a discrepancy in the last round between val_series_outcomes and val_matches, 
##   i'm relying on vlr.gg instead of liquepedia as the source of truth.
##   liquepedia (https://liquipedia.net/valorant/VALORANT_Champions_Tour/2021/Southeast_Asia/Stage_2/Challengers_3/Malaysia_and_Singapore) has this as ending in 17 rounds, as well as val_matches.
##   but vlr.gg (https://www.vlr.gg/14384/paper-rex-vs-team-smg-champions-tour-malaysia-singapore-stage-2-challengers-3-main-event-grand/?game=25724&tab=overview) has this as ending in 19 rounds, as well as val_rounds.
# init_val_rounds_side |> filter(game_id == 25726)
val_rounds_side <- init_val_rounds_side |> 
  inner_join(
    val_team_mapping |> select(game_id, team1id = team_id, team_1 = team), 
    by = c('game_id', 'team1id')
  ) |> 
  inner_join(
    val_team_mapping |> select(game_id, team2id = team_id, team_2 = team), 
    by = c('game_id', 'team2id')
  ) |> 
  inner_join(
    val_matches |> 
      transmute(
        match_id,
        team1id,
        team2id,
        team_1_series_w = team1_map_score,
        team_2_series_w = team2_map_score
      ),
    by = c('match_id', 'team1id', 'team2id')
  ) |> 
  inner_join(
    val_series_outcomes |> 
      select(
        game_id,
        total_rounds = round,
        series_winner
      ),
    by = 'game_id'
  ) |> 
  inner_join(
    val_games |> 
      transmute(
        game_id,
        match_id,
        map,
        team1_side_first_half
      ),
    by = c('game_id', 'match_id')
  ) |> 
  transmute(
    series_id = game_id,
    match_id,
    team_1,
    team_2,
    map,
    round,
    cumu_w,
    cumu_l,
    total_rounds,
    is_offense_1 = case_when(
      round <= 13 & team1_side_first_half == 'attack' ~ TRUE,
      round > 13 & team1_side_first_half == 'defend' ~ TRUE,
      round > 24 & ((round %% 2) == 1) & team1_side_first_half == 'attack' ~ TRUE,
      TRUE ~ FALSE
    ),
    win_round_1 = team_1 == round_winner,
    win_series_1 = team_1 == series_winner,
    team_1_series_w,
    team_2_series_w,
    team_1_buy_type = team1buy_type,
    team_2_buy_type = team2buy_type,
    team_1_bank = team1bank,
    team_2_bank = team2bank,
    win_type
  ) |> 
  arrange(series_id, round) |> 
  group_by(series_id) |> 
  mutate(
    pre_cumu_w = lag(cumu_w, default = 0L),
    pre_cumu_l = lag(cumu_l, default = 0L)
  ) |> 
  ungroup()

more_weird_val_series_outcomes <- bind_rows(
  val_rounds_side |> 
    filter(win_series_1) |> 
    group_by(series_id, team_1) |> 
    slice_max(round, n = 1) |> 
    ungroup() |> 
    filter(cumu_w <= cumu_l),
  val_rounds_side |> 
    filter(!win_series_1) |> 
    group_by(series_id, team_1) |> 
    slice_max(round, n = 1) |> 
    ungroup() |> 
    filter(cumu_w >= cumu_l)
)

val_rounds <- bind_rows(
  val_rounds_side |> 
    select(
      series_id,
      team = team_1,
      opponent = team_2,
      map,
      round,
      is_offense = is_offense_1,
      team_buy_type = team_1_buy_type,
      opponent_buy_type = team_2_buy_type,
      team_bank = team_1_bank,
      opponent_bank = team_2_bank,
      win_type,
      win_round = win_round_1,
      win_series = win_series_1,
      pre_cumu_w,
      pre_cumu_l,
      cumu_w,
      cumu_l,
      team_series_w = team_1_series_w,
      opponent_series_w = team_2_series_w
    ),
  val_rounds_side |> 
    transmute(
      series_id,
      team = team_2,
      opponent = team_1,
      map,
      round,
      is_offense = !is_offense_1,
      team_buy_type = team_2_buy_type,
      opponent_buy_type = team_1_buy_type,
      team_bank = team_2_bank,
      opponent_bank = team_1_bank,
      win_type,
      win_round = !win_round_1,
      win_series = !win_series_1,
      pre_cumu_w,
      pre_cumu_l,
      cumu_w,
      cumu_l,
      team_series_w = team_2_series_w,
      opponent_series_w = team_1_series_w
    ) |> 
    mutate(
      pre_cumu_z = pre_cumu_l,
      cumu_z = cumu_l
    ) |> 
    select(-c(pre_cumu_l, cumu_l)) |> 
    mutate(
      pre_cumu_l = pre_cumu_w,
      cumu_l = cumu_w
    ) |> 
    mutate(
      pre_cumu_w = pre_cumu_z,
      cumu_w = cumu_z
    ) |> 
    select(-c(pre_cumu_z, cumu_z))
) |> 
  inner_join(
    val_rounds_side |> 
      group_by(series_id, match_id) |> 
      slice_max(round, n = 1, with_ties = FALSE) |> 
      ungroup() |> 
      select(series_id, match_id, n_rounds = round),
    by = 'series_id'
  ) |> 
  inner_join(
    val_matches |> 
      transmute(
        match_id,
        event_id,
        event_name,
        event_stage,
        date,
        team_1_series_w = team1_map_score,
        team_2_series_w = team2_map_score
      ),
    by = 'match_id'
  ) |> 
  arrange(date, series_id, round, is_offense) |> 
  transmute(
    series_id,
    event_id,
    event_name,
    event_stage,
    date,
    map,
    team,
    opponent,
    round,
    n_rounds,
    is_offense,
    team_buy_type,
    opponent_buy_type,
    team_bank,
    opponent_bank,
    win_round,
    win_series,
    pre_cumu_w,
    pre_cumu_l,
    cumu_w,
    cumu_l,
    team_series_w,
    opponent_series_w
  ) |> 
  anti_join(
    init_weird_val_series_outcomes |> distinct(series_id = game_id),
    by = 'series_id'
  ) |> 
  anti_join(
    more_weird_val_series_outcomes |> distinct(series_id),
    by = 'series_id'
  )
qs::qsave(val_rounds, 'data/valorant_rounds.qs')

# val_rounds |> 
#   group_by(team, series_id) |> 
#   slice_max(round, n = 1) |> 
#   ungroup() |> 
#   select(
#     series_id,
#     team,
#     opponent,
#     cumu_w,
#     cumu_l,
#     win_series
#   ) |> 
#   arrange(series_id, team, opponent) |> 
#   filter(win_series, cumu_w <= cumu_l)
# 
# val_rounds |> 
#   filter(is_offense) |> 
#   mutate(
#     across(
#       c(team_buy_type, opponent_buy_type),
#       ~case_when(
#         .x %in% c('full-buy', 'eco') ~ .x,
#         TRUE ~ 'other'
#       )
#     )
#   ) |> 
#   count(team_buy_type, opponent_buy_type, sort = TRUE)
