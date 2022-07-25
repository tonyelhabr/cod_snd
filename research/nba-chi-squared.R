
seasons <- 2022
f_q <- quietly(nbastatR::game_logs)
logs <- seasons %>% 
  map_dfr(
    ~f_q(.x, result_types = 'team', season_types = 'Playoffs', assign_to_environment = FALSE) %>% 
      pluck('result')
  ) %>% 
  janitor::clean_names() 
logs

slim_logs <- logs %>% 
  # filter(slug_team == slug_team_winner) %>%
  select(
    year = year_season,
    date = date_game,
    id_game,
    tm_w = slug_team_winner,
    tm_l = slug_team_loser,
    slug_matchup,
    is_win,
    outcome_game
  )
slim_logs

sked |> 
  filter(series_type == 'playoff', series_title == 'Playoff Series') |> 
  select(game_id, season, home = home_abbreviation, away = away_abbreviation, series_summary) |> 
  mutate(
    across(
      series_summary,
      list(
        team = ~str_sub(.x, 1, 3),
        line = ~str_sub(.x, -3, -1)
      ),
      .names = '{fn}'
    )
  )
