## WPA reference: https://twitter.com/dougliebe/status/1565133671140675586?s=20&t=rMTpfQNQAJ0k_9HImsIwEA
library(qs)
library(dplyr)
library(broom)
library(tidyr)

source('scripts/helpers-wp.R')
source('scripts/helpers-plot.R')
data_dir <- 'data'
logos_dir <- file.path('figs', 'img')

breakingpointgg_team_mapping <- c(
  'ATL' = 'Atlanta FaZe',
  'BOS' = 'Boston Breach',
  'DAL' = 'Dallas Empire',
  'FLA' = 'Florida Mutineers',
  'LAG' = 'Los Angeles Guerrillas',
  'LAT' = 'Los Angeles Thieves',
  'LDN' = 'London Royal Ravents',
  'MIN' = 'Minnesota RØKKR',
  'NYSL' = 'New York Subliners',
  'OPTX' = 'OpTic Texas',
  'PAR' = 'Las Vegas Legion',
  'SEA' = 'Seattle Surge',
  'TOR' = 'Toronto Ultra'
)

breakingpointgg_team_logo_urls_mapping <- c(
  'Minnesota RØKKR' = file.path(logos_dir, 'Minnesota-Rokkr-Logo-119x128_cropped.png'),
  'Boston Breach' = file.path(logos_dir, 'Boston-Breach-128x122_cropped.png'),
  'London Royal Ravens' = file.path(logos_dir, 'London-Royal-Ravens-Logo-128x68_cropped.png'),
  'OpTic Texas' = file.path(logos_dir, 'OpTic-Texas-Logo-128x72_cropped.png'),
  'Los Angeles Guerrillas' = file.path(logos_dir, 'Los-Angeles-Guerrillas-Logo-93x128_cropped.png'),
  'Toronto Ultra' = file.path(logos_dir, 'Toronto-Ultra-Logo-128x128_cropped.png'),
  'Atlanta FaZe' = file.path(logos_dir, 'Atlanta-FaZe-Logo-128x62_cropped.png'),
  'Los Angeles Thieves' = file.path(logos_dir, 'Los-Angeles-Thieves-Logo-128x128_cropped.png'),
  'Florida Mutineers' = file.path(logos_dir, 'Florida-Mutineers-Logo-102x128_cropped.png'),
  'Seattle Surge' = file.path(logos_dir, 'Seattle-Surge-Logo-99x128_cropped.png'),
  'Las Vegas Legion' = file.path(logos_dir, 'Paris-Legion-Logo-104x128_cropped.png'),
  'New York Subliners' = file.path(logos_dir, 'New-York-Subliners-Logo-1-103x128_cropped.png')
)

team_color_mapping <- c(
  'ATL' = '#e43d30',
  'BOS' = '#02FF5B',
  'DAL' = '#B5A26A',
  'FLA' = '#025157',
  'LAG' = '#60269e',
  'LAT' = '#ef3232',
  'LDN' = '#CF152D',
  'MIN' = '#351f68',
  'NYSL' = '#FEE306',
  'OPTX' = '#9dc73b',
  'PAR' = '#EE7623',
  'SEA' = '#16667D',
  'TOR' = '#773dbd'
)

all_model_pbp <- qs::qread(file.path(data_dir, 'wp_model_data.qs')) |> 
  mutate(
    'team_label' = breakingpointgg_team_mapping[.data[['team']]],
    'opponent_label' = breakingpointgg_team_mapping[.data[['opponent']]],
    'team_logo_url' = breakingpointgg_team_logo_urls_mapping[.data[['team_label']]],
    'opponent_logo_url' = breakingpointgg_team_logo_urls_mapping[.data[['opponent_label']]],
    'team_color' = team_color_mapping[.data[['team']]],
    'opponent_color' = team_color_mapping[.data[['opponent']]],
    'activity_team_color' = case_when(
      team == activity_team ~ team_color,
      team == activity_opponent ~ opponent_color,
      TRUE ~ NA_character_
    ),
    'activity_opponent_color' = case_when(
      opponent == activity_opponent ~ opponent_color,
      opponent == activity_team ~ team_color,
      TRUE ~ NA_character_
    )
  )
 
model_lb <- qs::qread(file.path(data_dir, 'wp_model-lb.qs'))
long_participation <- qs::qread(file.path(data_dir, 'cod_snd_participation.qs'))
chains <- qs::qread(file.path('data', 'cod_snd_chains.qs'))

all_model_pbp <- augment(
  model_lb,
  data = all_model_pbp
)

team_wpa <- all_model_pbp |> 
  group_by(round_id, side) |> 
  mutate(
    team_wpa = wp - lag(wp, n = 1L)
  ) |> 
  ungroup() |> 
  relocate(wp, team_wpa, .after = wp)

long_participation |> 
  filter(round_id == '2021-SND-046-02') |> 
  # filter(engagement_id == '2021-SND-046-02-o-1v0-Kill') |> 
  filter(
    offense_engagement_id %in% c(
      '2021-SND-046-02-2v2-Kill',
      '2021-SND-046-02-2v1-Kill',
      '2021-SND-046-02-1v1-Kill',
      '2021-SND-046-02-1v0-Kill'
    )
  ) |> 
  filter(indicator == 1L) |> 
  select(
    round_id,
    offense_engagement_id,
    engagement_id,
    team,
    side,
    player,
    indicator,
    wt
  ) # |> 
  # left_join(
  #   chains,
  #   by = 'engagement_id',
  #   multiple = 'all'
  # )

long_wpa <- team_wpa |> 
  left_join(
    long_participation |> 
      select(
        offense_engagement_id,
        side,
        player,
        player_rn,
        indicator,
        wt
      ),
    by = c('offense_engagement_id', 'side'),
    multiple = 'all'
  ) |> 
  mutate(
    ## team wpa will already be negative for negative actions, so abs wt to prevent double negatives
    wpa = team_wpa * abs(wt)
  )

long_wpa |> count(activity, sort = TRUE)
kill_death_activities <- long_wpa |> 
  filter(
    activity %in% c('Kill', 'Kill Planter', 'Kill Defuser')
  )

kdr_by_player <- full_join(
  kill_death_activities |> 
    count(game, player = activity_player, name = 'kills'),
  kill_death_activities |> 
    count(game, player = activity_opposer, name = 'deaths'),
  by = c('game', 'player')
) |> 
  mutate(
    engagements = coalesce(kills, 0L) + coalesce(deaths, 0L),
    kdr = kills / deaths
  ) |> 
  arrange(desc(kdr))

wpa_by_side_player <- long_wpa |> 
  filter(!is.na(pbp_side)) |> 
  # filter(wpa < 0) |> 
  group_by(game, side, player) |> 
  summarize(
    n_wpa_engagements = n(),
    across(wpa, sum, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  group_by(game, player) |> 
  mutate(
    n_wpa_engagements = sum(n_wpa_engagements),
    total_wpa = sum(wpa)
  ) |> 
  ungroup()|> 
  pivot_wider(
    names_glue = '{side}{.value}',
    names_from = side,
    values_from = wpa
  ) |> 
  arrange(desc(abs(total_wpa)))

stats_by_player <- wpa_by_side_player |> 
  inner_join(
    kdr_by_player,
    by = c('game', 'player')
  )
stats_by_player

stats_by_player |> 
  summarize(
    across(
      c(
        total_wpa,
        dwpa,
        owpa,
        kdr
      ),
      list(
        mean = mean,
        sd = sd
      )
    )
  )

# ex <- all_model_pbp |> 
#   filter(round_id == '2021-SND-011-01')
# 
# ex$wp <- predict(model_lb, ex)
# ex |> 
#   select(
#     pbp_side,
#     side,
#     is_pre_plant,
#     activity,
#     seconds_elapsed,
#     model_seconds_elapsed,
#     opponent_diff,
#     is_initial_bomb_carrier_killed,
#     is_kill_on_attempted_clinch,
#     wp
#   )

# engagement_id_n <- all_model_pbp |> count(engagement_id)
# engagement_ids_missing_wp <- all_model_pbp |> 
#   filter(is.na(wp)) |> 
#   distinct(engagement_id)
# stopifnot(nrow(engagement_ids) == 0L)

## second clutches play of Vanguard, done by Scump: https://youtu.be/WxowLIf6HuI?t=818
example_round <- '2022-SND-177-01'

autoplot(
  model_lb,
  type = 'round',
  data = all_model_pbp,
  round_id = example_round,
  side = 'o',
  expand = FALSE,
  save = TRUE
)

 autoplot(
  model_lb,
  type = 'round',
  data = all_model_pbp,
  round_id = example_round,
  side = 'o',
  expand = TRUE,
  save = TRUE
)
