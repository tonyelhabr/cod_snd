## WPA reference: https://twitter.com/dougliebe/status/1565133671140675586?s=20&t=rMTpfQNQAJ0k_9HImsIwEA
library(qs)
library(dplyr)

source('scripts/helpers-wp.R')
all_model_pbp <- qs::qread(file.path('data', 'wp_model_data.qs')) |> 
  filter(activity != 'Start')

engagement_id_n <- all_model_pbp |> count(engagement_id)
model_lb <- qs::qread(file.path('data', 'wp_model-lb.qs'))

all_model_pbp$wp <- predict(model_lb, all_model_pbp)
engagement_ids_missing_wp <- all_model_pbp |> 
  filter(is.na(wp)) |> 
  distinct(engagement_id, is_pre_plant, is_post_plant, activity)

c(
  '2021-SND-177-03-d-1v0-Kill',
  '2021-SND-247-01-d-1v0-Defuse',
  '2022-SND-038-09-o-1v0-Kill',
  '2022-SND-134-02-o-0v0-Kill'
)

all_model_pbp |> 
  filter(!is_pre_plant) |> 
  semi_join(
    engagement_ids_missing_wp,
    by = 'engagement_id'
  )
debugonce(predict.wp_model_lb_state)
debugonce(predict.wp_model)
predict(
  model_lb,
  all_model_pbp |> 
    filter(
      engagement_id == '2021-SND-177-03-d-1v0-Kill'
    )
)



