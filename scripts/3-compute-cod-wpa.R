## WPA reference: https://twitter.com/dougliebe/status/1565133671140675586?s=20&t=rMTpfQNQAJ0k_9HImsIwEA
library(qs)
library(dplyr)
library(broom)
library(tidyr)
library(purrr)

pkgload::load_all('../snd')
data_dir <- getOption('snd.dir.data')

all_model_pbp <- qs::qread(file.path(data_dir, 'wp_model_data.qs')) |> 
  add_aesthetic_cols()

model <- qs::qread(file.path(data_dir, 'wp_model.qs'))
long_participation <- qs::qread(file.path(data_dir, 'cod_snd_participation.qs'))
chains <- qs::qread(file.path('data', 'cod_snd_chains.qs'))

all_model_pbp <- augment(
  model,
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
  )

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

## second clutches play of Vanguard, done by Scump: https://youtu.be/WxowLIf6HuI?t=818
example_round_id <- '2022-SND-177-01'

autoplot(
  model,
  type = 'round',
  data = all_model_pbp,
  round_id = example_round_id,
  side = 'd',
  expand = TRUE,
  save = TRUE
)

## missing end
autoplot(
  model,
  type = 'round',
  data = all_model_pbp,
  round_id = '2022-SND-177-02',
  side = 'd',
  expand = TRUE,
  save = TRUE
)

## missing end
autoplot(
  model,
  type = 'round',
  data = all_model_pbp,
  round_id = '2022-SND-177-03',
  side = 'd',
  expand = TRUE,
  save = TRUE
)

## missing end
autoplot(
  model,
  type = 'round',
  data = all_model_pbp,
  round_id = '2022-SND-177-04',
  side = 'd',
  expand = TRUE,
  save = TRUE
)

## some text is removed?
autoplot(
  model,
  type = 'round',
  data = all_model_pbp,
  round_id = '2022-SND-177-05',
  side = 'd',
  expand = TRUE,
  save = TRUE
)

## last point is off here
autoplot(
  model,
  type = 'round',
  data = all_model_pbp,
  round_id = '2022-SND-177-06',
  side = 'd',
  expand = TRUE,
  save = TRUE
)

## missing end
autoplot(
  model,
  type = 'round',
  data = all_model_pbp,
  round_id = '2022-SND-177-07',
  side = 'd',
  expand = TRUE,
  save = TRUE
)
