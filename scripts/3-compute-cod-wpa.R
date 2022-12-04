## WPA reference: https://twitter.com/dougliebe/status/1565133671140675586?s=20&t=rMTpfQNQAJ0k_9HImsIwEA
library(qs)
library(dplyr)
library(broom)

source('scripts/helpers-wp.R')
source('scripts/helpers-plot.R')
model_lb <- qs::qread(file.path('data', 'wp_model-lb.qs'))
all_model_pbp <- qs::qread(file.path('data', 'wp_model_data.qs'))

all_model_pbp <- augment(
  model_lb,
  data = all_model_pbp
)

wide_participation <- qs::qread(file.path('data', 'cod_snd_participation.qs'))

team_wpa <- all_model_pbp |> 
  group_by(round_id, side) |> 
  mutate(
    team_wpa = wp - lag(wp, n = 1L)
  ) |> 
  ungroup() |> 
  relocate(wp, team_wpa, .after = wp)

long_participation <- qs::qread(file.path('data', 'long_cod_snd_participation.qs'))
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
hist(long_participation$wt)
sum(long_participation$wt, na.rm = TRUE)

long_wpa |> 
  filter(activity != 'Start') |>
  # filter(engagement_id == '2021-SND-011-01-d-0v2-Kill')
  filter(offense_engagement_id == '2021-SND-011-01-2v0-Kill') |> 
  glimpse()
  group_by(engagement_id, side, team_wpa) |> 
  summarize(
    across(wpa, sum)
  )

wpa_by_side_player <- long_wpa |> 
  filter(!is.na(pbp_side)) |> 
  # filter(wpa < 0) |> 
  group_by(side, player) |> 
  summarize(
    n = n(),
    across(wpa, sum, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  group_by(player) |> 
  mutate(
    n = sum(n),
    total_wpa = sum(wpa)
  ) |> 
  ungroup()|> 
  pivot_wider(
    names_glue = '{side}{.value}',
    names_from = side,
    values_from = c(wpa, n)
  ) |> 
  arrange(desc(abs(total_wpa)))
wpa_by_side_player
hist(wpa_by_side_player$total_wpa)
hist(wpa_by_side_player$owpa)
hist(wpa_by_side_player$dwpa)

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

autoplot(
  model_lb,
  type = 'round',
  data = all_model_pbp,
  round_id = '2021-SND-011-02',
  side = 'o',
  expand = FALSE
)

autoplot(
  model_lb,
  type = 'round',
  data = all_model_pbp,
  round_id = '2021-SND-011-02',
  side = 'o',
  expand = TRUE
)


