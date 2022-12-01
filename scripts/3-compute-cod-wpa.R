## WPA reference: https://twitter.com/dougliebe/status/1565133671140675586?s=20&t=rMTpfQNQAJ0k_9HImsIwEA
library(qs)
library(dplyr)

source('scripts/helpers-wp.R')
source('scripts/helpers-plot.R')
model_lb <- qs::qread(file.path('data', 'wp_model-lb.qs'))
all_model_pbp <- qs::qread(file.path('data', 'wp_model_data.qs'))

ex <- all_model_pbp |> 
  filter(round_id == first(round_id))

ex$wp <- predict(model_lb, ex)
ex |> 
  select(
    pbp_side,
    side,
    is_pre_plant,
    activity,
    seconds_elapsed,
    model_seconds_elapsed,
    opponent_diff,
    is_initial_bomb_carrier_killed,
    is_kill_on_attempted_clinch,
    wp
  )

all_model_pbp <- all_model_pbp |> 
  mutate(
    'wp' = predict(model_lb, all_model_pbp),
    .before = 1
  )
class(all_model_pbp) <- c('wp_model_df', class(all_model_pbp))

engagement_id_n <- all_model_pbp |> count(engagement_id)

all_model_pbp$wp <- predict(model_lb, all_model_pbp)
engagement_ids_missing_wp <- all_model_pbp |> 
  filter(is.na(wp)) |> 
  distinct(engagement_id)
stopifnot(nrow(engagement_ids) == 0L)
autoplot(all_model_pbp, round_id = '2021-SND-011-02', side = 'o')

