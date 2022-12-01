## WPA reference: https://twitter.com/dougliebe/status/1565133671140675586?s=20&t=rMTpfQNQAJ0k_9HImsIwEA
library(qs)
library(dplyr)

source('scripts/helpers-wp.R')
source('scripts/helpers-plot.R')
model_lb <- qs::qread(file.path('data', 'wp_model-lb.qs'))
all_model_pbp <- qs::qread(file.path('data', 'wp_model_data.qs')) |> 
  mutate(
    'wp' = predict(model_lb, all_model_pbp),
    .before = 1
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


