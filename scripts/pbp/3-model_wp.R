library(ggplot2)
library(qs)
library(purrr)
library(dplyr)
library(tibble)

pkgload::load_all('../snd')
data_dir <- getOption('snd.dir.data')
figs_dir <- getOption('snd.dir.figs')
theme_set_snd()

all_model_pbp <- qs::qread(file.path(data_dir, 'wp_model_data.qs'))

## model ----
model <- fit_wp_model(all_model_pbp)
qs::qsave(model, file.path(data_dir, 'wp_model.qs'))

naive_model <- fit_naive_wp_model(all_model_pbp)
qs::qsave(naive_model, file.path(data_dir, 'naive_wp_model.qs'))

coefs_plot <- autoplot(model, type = 'coefs')
ggsave(
  coefs_plot,
  filename = file.path(figs_dir, 'wp_coefs.png'),
  width = 12,
  height = 8
)

plot_and_save_wp_by_feature <- function(model, feature_name = NULL) {
  p <- autoplot(model, type = 'grid', feature_name = feature_name)
  
  if (is.null(feature_name)) {
    feature_name <- ''
  }
  print(p)
  ggsave(
    p,
    filename = file.path(figs_dir, sprintf('wp_grid-%s.png', feature_name)),
    width = 12,
    height = 8
  )
  invisible(p)
}

c(
  'has_started_clinch',
  'has_started_clinch',
  'is_initial_bomb_carrier_killed'
) |> 
  set_names() |> 
  walk(
    ~plot_and_save_wp_by_feature(
      model = model,
      feature_name = .x
    )
  )

plot_and_save_wp_by_feature(
  model = model
)

new_data <- tibble::tibble(
  'side' = c('d', 'o'),
  'is_pre_plant' = TRUE,
  'model_seconds_elapsed' = 84,
  'n_team_remaining' = 3,
  'n_opponent_remaining' = 2,
  'has_started_clinch' = 0,
  'has_started_clinch' = 0,
  'is_initial_bomb_carrier_killed' = 0,
  'activity' = 'Start Plant'
) |> 
  dplyr::mutate(
    # 'is_offense' = side == 'o',
    'opponent_diff' = n_team_remaining - n_opponent_remaining,
    'model_seconds_remaining' = ifelse(
      is_pre_plant,
      max_pre_plant_second - .data[['model_seconds_elapsed']],
      max_post_plant_second - .data[['model_seconds_elapsed']]
    )
  ) |> 
  add_hardcoded_wp_cols()

predict(
  model,
  new_data
)
