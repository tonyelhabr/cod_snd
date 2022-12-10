
## yardstick ----
## TODO: Actually use this for xgboost model
library(yardstick)
brier_score <- function(data, ...) {
  UseMethod("brier_score")
}

brier_score <- yardstick::new_prob_metric(brier_score, direction = "minimize")

brier_score_vec <- function(truth, estimate, na_rm = TRUE, event_level, ...) {
  
  brier_score_impl <- function(truth, estimate, event_level, ...) {
    truth <- 1 - (as.numeric(truth) - 1)
    
    if (event_level == "second") {
      truth <- 1 - truth
    }
    mean((truth - estimate)^2)
  }
  
  yardstick::metric_vec_template(
    metric_impl = brier_score_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = c("factor", "numeric"),
    estimator = "binary",
    event_level = event_level,
    ...
  )
}

brier_score.data.frame <- function(data, truth, estimate, na_rm = TRUE, event_level = "first", ...) {
  yardstick::metric_summarizer(
    metric_nm = "brier_score",
    metric_fn = brier_score_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    na_rm = na_rm,
    event_level = event_level,
    ...
  )
}

brier_skill_score <- function(data, ...) {
  UseMethod("brier_skill_score")
}

brier_skill_score <- yardstick::new_prob_metric(brier_skill_score, direction = "maximize")

brier_skill_score_vec <- function(truth, estimate, ref_estimate, na_rm = TRUE, event_level, ...) {
  
  brier_skill_score_impl <- function(truth, estimate, ref_estimate, event_level, ...) {
    truth_quo <- rlang::enquo(truth)
    
    estimate_bs <- brier_score_vec(
      truth = truth,
      estimate = estimate,
      na_rm = na_rm,
      event_level = event_level,
      ...
    )
    
    ref_bs <- brier_score_vec(
      truth = truth,
      estimate = ref_estimate,
      na_rm = na_rm,
      event_level = event_level,
      ...
    )
    
    1 - (estimate_bs / ref_bs)
  }
  
  yardstick::metric_vec_template(
    metric_impl = brier_skill_score_impl,
    truth = truth,
    estimate = estimate,
    ref_estimate = ref_estimate,
    cls = c("factor", "numeric"),
    estimator = "binary",
    event_level = event_level,
    ...
  )
}

## generic ----
target_name <- 'win_round'
default_side <- 'o'
max_pre_plant_second <- 90L
max_post_plant_second <- 45L
max_pre_plant_second_buffer <- 0L ## for plot

## TODO: Add validation for is_pre_plant
## Add extra second so that LOESS span can handle exactly on the last second
generate_seconds_grid <- function(is_pre_plant) {
  if (isTRUE(is_pre_plant)) {
    min_second <- c(seq(0, 75, by = 3), seq(75, 85, by = 1), seq(86, 90), 91)
    max_second <- c(seq(15, 90, by = 3), seq(80, 90, by = 1), rep(90, 5), 91)
  } else {
    min_second <- c(seq(0, 30, 2), seq(30, 38, 1), seq(39, 46))
    max_second <- c(seq(10, 40, 2), seq(37, 45, 1), rep(45, 7), 46)
  }
  tibble(
    min_second = as.integer(min_second),
    max_second = as.integer(max_second)
  )
}

get_max_second <- function(is_pre_plant) {
  ifelse(isTRUE(is_pre_plant), 90L, 45L)
}

## TODO: Add more validation for `method`
get_all_features <- function(is_pre_plant, named, method = 'xgb') {
  
  if (!is.na(method)) {
    stopifnot('`method` must be one of "lb" or "xgb" if not `NA`' = method %in% c('lb', 'xgb'))
  }
  
  ## TODO:
  ## Ideally, xgboost would be monotonically decreasing when opponent diff is negative
  ##   and monotonocially increasing when opponent_diff is positive.
  ##   This is technically possible by splitting out opponent_diff into 3 features (positive, negative, and neutral) 
  ##   and applying separate monotonic constraints to each, or perhaps by having
  ##   two models, one where a positive constraint is applied and one where a negative constraint
  ##   is applied, and then using the appropriate prediction given the data.
  all_features <- c(
    'model_seconds_elapsed' = 0, # 1,
    'opponent_diff' = -1,
    'is_kill_on_attempted_clinch' = 1,
    'is_initial_bomb_carrier_killed' = -1
  )
  
  if (isFALSE(is_pre_plant)) {
    idx_to_drop <- which(names(all_features) == 'is_initial_bomb_carrier_killed')
    idx_to_keep <- setdiff(seq.int(1, length(all_features)), idx_to_drop)
    all_features <- all_features[idx_to_keep]
  }
  
  if (isFALSE(named)) {
    all_features <- names(all_features)
  }
  
  if (method != 'lb') {
    return(all_features)
  }
  
  ## if method == 'lb'
  setdiff(all_features, 'model_seconds_elapsed')
  # c(setdiff(all_features, 'model_seconds_elapsed'), '(Intercept)')
}

get_lb_window_feature_names <- function(is_pre_plant) {
  get_all_features(
    is_pre_plant = is_pre_plant, 
    named = FALSE,
    method = 'lb'
  )
}

validate_colnames <- function(data) {
  nms_diff <- setdiff(
    c(
      'is_pre_plant', 
      'side',
      'n_team_remaining',
      'n_opponent_remaining',
      'model_seconds_remaining'
    ),
    colnames(data)
  )
  
  if (length(nms_diff) > 0L) {
    stop(
      sprintf('Missing essential non-feature columns:\n%s', paste0(nms_diff, collapse = ', '))
    )
  }
  
  all_feature_names <- c(TRUE, FALSE) |> 
    purrr::map(
      ~get_all_features(
        is_pre_plant = .x, 
        named = FALSE
      )
    ) |> 
    purrr::flatten_chr() |> 
    unique()
  
  nms_diff <- setdiff(
    all_feature_names,
    colnames(data)
  )
  
  if (length(nms_diff) > 0L) {
    stop(
      sprintf('Missing feature names:\n%s', paste0(nms_diff, collapse = ', '))
    )
  }
  
  invisible(data)
}

split_model_data <- function(data) {
  
  validate_colnames(data = data)
  
  model_data <- dplyr::filter(data, .data[['side']] == default_side)
  
  list(
    'pre' = dplyr::filter(model_data, .data[['is_pre_plant']]),
    'post' = dplyr::filter(model_data, !.data[['is_pre_plant']])
  )
}

fit_wp_model_states <- function(data, method, ...) {
  
  model_data <- split_model_data(data = data)
  
  ## TODO: Use invoke?
  f <- switch(
    method,
    'lb' = fit_wp_model_lb_state,
    'xgb' = fit_wp_model_xgb_state
  )
  
  ## things get messy if using imap here, so just be explicit instead
  models <- list(
    'pre' = f(model_data[['pre']], is_pre_plant = TRUE, ...),
    'post' = f(model_data[['post']], is_pre_plant = FALSE, ...)
  )
  
  class(models) <- c('wp_model', sprintf('wp_model_%s', method), class(models))
  models
}

## TODO: Do less hard-coding here with features
generate_pred_grid <- function(is_pre_plant) {
  binary_feature_names <- c(
    'is_kill_on_attempted_clinch',
    'is_initial_bomb_carrier_killed'
  )
  
  binary_l <- vector(mode = 'list', length(binary_feature_names))
  names(binary_l) <- binary_feature_names
  for(el in names(binary_l)) {
    binary_l[[el]] <- c(0L, 1L)
  }
  
  max_second <- get_max_second(is_pre_plant)
  max_players <- 4L
  
  binary_values <- 0L:1L
  is_initial_bomb_carrier_killed <- if (isTRUE(is_pre_plant)) {
    binary_values
  } else {
    NA_integer_
  }
  tidyr::crossing(
    'side' = c('o', 'd'),
    'is_pre_plant' = is_pre_plant,
    'model_seconds_elapsed' = 0L:(max_second - 1L),
    'n_team_remaining' = 1L:max_players,
    'n_opponent_remaining' = 1L:max_players,
    'is_kill_on_attempted_clinch' = binary_values,
    'is_initial_bomb_carrier_killed' = is_initial_bomb_carrier_killed
  ) |> 
    mutate(
      'opponent_diff' = .data[['n_team_remaining']] - .data[['n_opponent_remaining']],
      'model_seconds_remaining' = ifelse(
        .data[['is_pre_plant']],
        max_pre_plant_second - .data[['model_seconds_elapsed']],
        max_post_plant_second - .data[['model_seconds_elapsed']]
      )
    )
}

## TODO: Could fit a model for the specific situation of a post-plant 1v0 defuse, for which the defense should be given 0 WP if the seconds remaining is less than 7.5, but something between 0 and 1 if there are between 15 and 7.5 seconds remaining.
## Could even allow for 2v0, 3v0, and 4v0 predictions. These would not have a hard rule for the last 7.5 seconds since a person could be defusing while a teammate eliminates an opponent with less than 7.5 seconds remaining.
# d <- all_model_pbp |> filter(side == 'o', !is_pre_plant, n_team_pre_activity == 1, n_team_remaining == 0, n_opponent_remaining == 1)
# fit <- glm(win_round ~ model_seconds_elapsed, d, family = 'binomial')
# round(predict(fit, tibble(model_seconds_elapsed = c(0:45)), type = 'response'), 2)

predict.wp_model <- function(object, new_data, ...) {
  
  validate_colnames(data = new_data)
  
  pred <- ifelse(
    new_data[['is_pre_plant']],
    predict(object[['pre']], new_data, ...),
    predict(object[['post']], new_data, ...)
  )
  
  rep_data <- function(x) {
    rep(x, nrow(new_data))
  }
  
  is_1v0_post_plant <- !new_data[['is_pre_plant']] & (new_data[['n_team_remaining']] == 1) & (new_data[['n_opponent_remaining']] == 0)
  is_0v1_post_plant <- !new_data[['is_pre_plant']] & (new_data[['n_team_remaining']] == 0) & (new_data[['n_opponent_remaining']] == 1)
  is_Nv0_post_plant <- !new_data[['is_pre_plant']] & (new_data[['n_team_remaining']] > 1) & (new_data[['n_opponent_remaining']] == 0)
  is_0vN_post_plant <- !new_data[['is_pre_plant']] & (new_data[['n_team_remaining']] == 0) & (new_data[['n_opponent_remaining']] > 1)
  
  dplyr::case_when(
    is_1v0_post_plant & new_data[['model_seconds_remaining']] >= 15 ~ rep_data(1),
    is_1v0_post_plant & new_data[['model_seconds_remaining']] >= 12.5 ~ rep_data(0.9),
    is_1v0_post_plant & new_data[['model_seconds_remaining']] >= 10 ~ rep_data(0.65),
    is_1v0_post_plant & new_data[['model_seconds_remaining']] >= 7.5 ~ rep_data(0.5),
    is_1v0_post_plant & new_data[['model_seconds_remaining']] < 7.5  ~ rep_data(0),
    
    is_0v1_post_plant & new_data[['model_seconds_remaining']] >= 15 ~ rep_data(0),
    is_0v1_post_plant & new_data[['model_seconds_remaining']] >= 12.5  ~ rep_data(0.1),
    is_0v1_post_plant & new_data[['model_seconds_remaining']] >= 10 ~ rep_data(0.35),
    is_0v1_post_plant & new_data[['model_seconds_remaining']] >= 7.5 ~ rep_data(0.5),
    is_0v1_post_plant & new_data[['model_seconds_remaining']] < 7.5  ~ rep_data(1),
    
    is_Nv0_post_plant & new_data[['model_seconds_remaining']] >= 12.5 ~ rep_data(1),
    is_Nv0_post_plant & new_data[['model_seconds_remaining']] >= 10 ~ rep_data(0.75),
    is_Nv0_post_plant & new_data[['model_seconds_remaining']] >= 7.5 ~ rep_data(0.5),
    is_Nv0_post_plant & new_data[['model_seconds_remaining']] < 7.5  ~ rep_data(0.25),
    
    is_0vN_post_plant & new_data[['model_seconds_remaining']] >= 12.5 ~ rep_data(0),
    is_0vN_post_plant & new_data[['model_seconds_remaining']] >= 10 ~ rep_data(0.25),
    is_0vN_post_plant & new_data[['model_seconds_remaining']] >= 7.5 ~ rep_data(0.5),
    is_0vN_post_plant & new_data[['model_seconds_remaining']] < 7.5  ~ rep_data(0.75),
    
    TRUE ~ pred
  )
  
  # ifelse(
  #   new_data[['side']] == 'o',
  #   pred,
  #   1 - pred
  # )
}

augment.wp_model <- function(x, data, ...) {
  data[['wp']] <- predict(x, data, ...)
  data
}

generate_wp_state_grid <- function(model, is_pre_plant) {
  
  pred_grid <- generate_pred_grid(is_pre_plant = is_pre_plant)
  
  augment(
    model,
    pred_grid
  )
}

generate_wp_grid <- function(model) {
  c(TRUE, FALSE) |> 
    purrr::map_dfr(
      ~generate_wp_state_grid(
        model,
        is_pre_plant = .x
      )
    )
}

summarize_pred_grid_across_features <- function(df, binary_feature_name) {
  df |> 
    group_by(
      .data[['is_pre_plant']],
      .data[['model_seconds_elapsed']],
      .data[['opponent_diff']],
      value = .data[[binary_feature_name]]
    ) |> 
    summarize(
      'wp' = mean(.data[['wp']])
    ) |> 
    ungroup()
}

# RColorBrewer::brewer.pal(7, 'PRGn') |> datapasta::vector_paste_vertical()
player_diff_pal <- c(
  '#762A83',
  '#AF8DC3',
  '#E7D4E8',
  '#F7F7F7',
  '#D9F0D3',
  '#7FBF7B',
  '#1B7837'
) |> 
  rlang::set_names(as.character(seq(-3, 3, by = 1)))

scale_x_continuous_wp_states <- function(...) {
  list(
    ...,
    ggplot2::scale_x_continuous(
      breaks = c(
        0, 
        max_pre_plant_second / 2, 
        max_pre_plant_second, 
        max_pre_plant_second + max_pre_plant_second_buffer / 2, 
        max_pre_plant_second + max_pre_plant_second_buffer, 
        max_pre_plant_second + max_pre_plant_second_buffer + 25, 
        max_pre_plant_second + max_pre_plant_second_buffer + 45
      ),
      labels = c('90', '45', '', 'Plant', '', '20', '0')
    ),
    ggplot2::theme(
      legend.position = 'top'
    ),
    ggplot2::labs(
      x = 'Seconds remaining'
    )
  )
}

# weights_by_second <- seconds_grid |>
#   dplyr::transmute(
#     'model_seconds_elapsed' = .data[['min_second']],
#     'n' = purrr::map2_int(
#       .data[['min_second']], 
#       .data[['max_second']], 
#       ~dplyr::filter(
#         all_model_pbp,
#         .data[['model_seconds_elapsed']] >= ..1, 
#         .data[['model_seconds_elapsed']] <= ..2
#       ) |> nrow()
#     )
#   ) |> 
#   dplyr::mutate('scaler' = .data[['n']] / max(.data[['n']]))
# scaler_by_second <- \(x) x^(-0.5)
plot_wp_grid <- function(data, feature_name, ...) {
  
  filt <- dplyr::filter(
    data, 
    .data[['side']] == default_side
  )
  
  agg <- if (!is.null(feature_name)) {
    filt |> 
      group_by(
        .data[['is_pre_plant']],
        .data[['model_seconds_elapsed']],
        .data[['opponent_diff']],
        'feature_value' = .data[[feature_name]]
      ) |> 
      summarize(
        'wp' = mean(.data[['wp']])
      ) |> 
      ungroup() |> 
      mutate(
        'feature' = sprintf(
          '%s: %s',
          feature_name,
          .data[['feature_value']]
        )
      )
  } else {
    filt |> 
      group_by(
        .data[['is_pre_plant']],
        .data[['model_seconds_elapsed']],
        .data[['opponent_diff']]
      ) |> 
      summarize(
        'wp' = mean(.data[['wp']])
      ) |> 
      ungroup()
  }
  
  df <- dplyr::bind_rows(
    dplyr::filter(agg, .data[['is_pre_plant']]),
    agg |> 
      dplyr::filter(!.data[['is_pre_plant']]) |> 
      dplyr::mutate(
        'model_seconds_elapsed' = .data[['model_seconds_elapsed']] + max_pre_plant_second + max_pre_plant_second_buffer
      )
  )
  
  p <- df |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = .data[['model_seconds_elapsed']], y = .data[['wp']]) +
    ggplot2::geom_hline(
      color = 'white',
      ggplot2::aes(yintercept = 0.5)
    ) +
    ggplot2::geom_vline(
      color = 'white',
      ggplot2::aes(xintercept = max_pre_plant_second)
    ) +
    ggplot2::geom_step(
      size = 1,
      ggplot2::aes(color = factor(.data[['opponent_diff']]))
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        title = 'Net # of players',
        override.aes = list(size = 3)
      )
    ) +
    ggplot2::scale_color_manual(values = player_diff_pal) +
    scale_x_continuous_wp_states() +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::theme(
      legend.position = 'top'
    ) +
    ggplot2::labs(
      title = 'Offensive Win Probability',
      y = 'Win probability'
    )
  
  if (is.null(feature_name)) {
    return(p)
  }
  
  p +
    ggplot2::facet_wrap(~.data[['feature']])
}


side_lab_switcher <- function(side) {
  switch(
    side,
    'o' = 'Offensive',
    'd' = 'Defensive'
  )
}

plot_round <- function(
    model,
    data,
    round_id,
    side = 'o',
    expand = FALSE,
    save = TRUE,
    path = file.path('figs', sprintf('round_id=%s&side=%s&expand=%s.png', round_id, side, tolower(expand))),
    ggsave.args = list(
      height = 10,
      width = 10,
      units = 'in'
    )
) {
  
  stopifnot(length(round_id) == 1)
  side <- rlang::arg_match(side, c('o', 'd'))
  
  filt <- data |>
    dplyr::filter(.data[['round_id']] == !!round_id, .data[['side']] == !!side)
  
  win_round <- as.character(unique(filt[['win_round']])) == 'yes'
  labels_on_bottom <- win_round
  
  last_row <- filt |> 
    dplyr::slice_max(.data[['seconds_elapsed']], n = 1, with_ties = FALSE)
  
  filt <- dplyr::add_row(
    filt,
    last_row |> 
      dplyr::mutate(
        'activity' = 'End',
        'seconds_elapsed' = .data[['seconds_elapsed']] + 1
      )
  )
  
  round_has_plant <- unique(filt$round_has_plant)
  # round_has_plant <- isTRUE(any(filt[['is_pre_plant']] == FALSE))
  
  if (isTRUE(expand)) {
    f_line <- ggplot2::geom_line
    
    seconds_elapsed_range <- range(filt[['seconds_elapsed']])
    pre_plant_seconds_elapsed_range <- filt |> 
      dplyr::filter(.data[['is_pre_plant']]) |> 
      dplyr::pull(.data[['model_seconds_elapsed']]) |> 
      range()
    
    pre_plant_seconds <- filt |> 
      dplyr::filter(.data[['is_pre_plant']]) |> 
      dplyr::pull(.data[['model_seconds_elapsed']]) |> 
      unique()
    
    pre_plant_seconds_seq <- c(
      seq(pre_plant_seconds_elapsed_range[1], pre_plant_seconds_elapsed_range[2]),
      pre_plant_seconds
    ) |> 
      unique() |> 
      sort()
    
    ## TODO: Does this work for non-integer seconds?
    grid <- tibble::tibble(
      'model_seconds_elapsed' = pre_plant_seconds_seq,
      'is_pre_plant' = rep(TRUE, length(pre_plant_seconds_seq))
    ) |> 
      dplyr::mutate('seconds_elapsed' = .data[['model_seconds_elapsed']])
    
    if (isTRUE(round_has_plant)) {
      
      post_plant_seconds_elapsed_range <- filt |> 
        dplyr::filter(!.data[['is_pre_plant']]) |> 
        dplyr::pull(.data[['model_seconds_elapsed']]) |> 
        range()
      
      post_plant_seconds <- filt |> 
        dplyr::filter(!.data[['is_pre_plant']]) |> 
        dplyr::pull(.data[['model_seconds_elapsed']]) |> 
        unique()
      
      
      post_plant_seconds_seq <- c(
        seq(pre_plant_seconds_elapsed_range[1], pre_plant_seconds_elapsed_range[2]),
        pre_plant_seconds
      ) |> 
        unique() |> 
        sort()
      
      grid <- bind_rows(
        grid,
        tibble::tibble(
          'model_seconds_elapsed' = post_plant_seconds_seq,
          'is_pre_plant' = rep(FALSE, length(post_plant_seconds_seq))
        ) |> 
          dplyr::mutate('seconds_elapsed' = pre_plant_seconds_elapsed_range[2] + .data[['model_seconds_elapsed']])
      )
    }
    
    df <- grid |> 
      dplyr::left_join(
        filt,
        by = c('model_seconds_elapsed', 'seconds_elapsed', 'is_pre_plant'),
        multiple = 'all'
      ) |> 
      tidyr::fill(
        # tidyselect::vars_select_helpers$all_of(setdiff(colnames(filt), join_cols)),
        get_all_features(is_pre_plant = TRUE, named = FALSE, method = 'lb'),
        .direction = 'down'
      )
  } else {
    f_line <- ggplot2::geom_step
    df <- filt
  }
  
  df$wp <- predict(model, df)
  
  df |> 
    select(
      seconds_elapsed,
      model_seconds_elapsed,
      is_pre_plant,
      activity,
      activity_player,
      activity_opposer,
      activity_team,
      activity_opponent,
      wp
    ) |> 
    tibble::view()
  
  if (nrow(filt) == 0) {
    stop(
      'Could not find round_id or side in data.'
    )
  }
  
  labels <- filt |> 
    # dplyr::distinct(.data[['engagement_id']], .keep_all = TRUE) |> 
    dplyr::mutate(
      'label' = dplyr::case_when(
        is.na(.data[['activity']]) ~ '???',
        .data[['activity']] == 'End' ~ 'End',
        .data[['activity']] == 'Start' ~ 'Start',
        .data[['activity']] == 'Plant' & .data[['is_pre_plant']] ~ sprintf('%s (%s) plants (pre-plant)', .data[['activity_player']], .data[['activity_team']]),
        .data[['activity']] == 'Plant' & !.data[['is_pre_plant']] ~ sprintf('%s (%s) plants (post-plant)', .data[['activity_player']], .data[['activity_team']]),
        .data[['activity']] == 'Defuse' ~ sprintf('%s (%s) defuses', .data[['activity_player']], .data[['activity_team']]),
        .data[['activity']] == 'Kill' ~ sprintf('%s (%s) kills\n%s (%s)', .data[['activity_player']], .data[['activity_team']], .data[['activity_opposer']], .data[['activity_opponent']]),
        .data[['activity']] == 'Kill Planter' ~ sprintf('%s (%s) kills\n%s %s (planting)', .data[['activity_player']], .data[['activity_team']], .data[['activity_opposer']], .data[['activity_opponent']]),
        .data[['activity']] == 'Kill Defuser' ~ sprintf('%s (%s) kills\n%s (%s) (defusing)', .data[['activity_player']], .data[['activity_team']], .data[['activity_opposer']], .data[['activity_opponent']]),
        .data[['activity']] == 'Self Kill' ~ sprintf('%s (%s) self-kills', .data[['activity_player']], .data[['activity_team']]),
        .data[['activity']] == 'Team Kill' ~ sprintf('%s (%s)\nteam-kills %s', .data[['activity_player']], .data[['activity_team']], .data[['activity_opposer']])
      )
    )
  
  df |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = .data[['seconds_elapsed']], y = .data[['wp']]) +
    ggplot2::geom_step() +
    # f_line()  +
    ggplot2::geom_point(
      data = labels
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent
    ) +
    ggplot2::labs(
      title = sprintf('%s win probability', side_lab_switcher(side)),
      y = 'Win probability',
      x = 'Seconds elapsed'
    )
  
  p <- df |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = .data[['seconds_elapsed']], y = .data[['wp']]) +
    # ggplot2::geom_step() +
    f_line() +
    ggforce::geom_mark_circle(
      data = labels |> dplyr::filter(!(.data[['activity']] %in% c('Start', 'End'))),
      label.colour = 'white',
      # label.fill = 'transparent',
      label.fill = blackish_background,
      con.colour = 'white',
      label.family = font,
      label.fontsize = 12,
      label.fontface = 'plain',
      # label.buffer = 0,
      # expand = 0,
      expand = ggplot2::unit(0.01, 'npc'),
      con.linetype = 2,
      con.border = 'none',
      con.type = 'straight',
      n = 1,
      label.buffer = ggplot2::unit(0.15, 'npc'),
      colour = 'transparent',
      ggplot2::aes(
        group = .data[['label']],
        label = .data[['label']]
      )
    ) +
    ggplot2::geom_point(
      data = labels
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent
    ) +
    ggplot2::labs(
      title = sprintf('%s win probability', side_lab_switcher(side)),
      y = 'Win probability',
      x = 'Seconds elapsed'
    )
  
  if (isTRUE(round_has_plant)) {
    p <- p +
      ggplot2::geom_vline(
        ggplot2::aes(
          x = unique(filt[['plant_second']])
        ),
        color = 'white',
        linetype = 1,
        size = 1
      )
  }
  
  if (isFALSE(save)) {
    return(p)
  }
  
  rlang::exec(
    ggplot2::ggsave,
    plot = p,
    filename = path,
    !!!ggsave.args
  )
  
  # ggplot2::ggsave(
  #   plot = p,
  #   filename = file.path('figs', sprintf('round_id=%s&side=%s.png', round_id, side)),
  #   units = 'in',
  #   height = 7,
  #   width = 7
  # )
}


autoplot.wp_model <- function(object, type = 'grid', ...) {
  type <- rlang::arg_match(type, c('grid', 'coefs', 'round'))
  
  cls <- class(object)
  if (type == 'coefs' & !any(cls == 'wp_model_lb')) {
    stop(
      sprintf(
        'Object must have class `wp_model_lb` for `type = "coefs"`. Object has class %s',
        paste0(cls, collapse = ', ')
      )
    )
  }
  switch(
    type,
    'grid' = plot_wp_grid(generate_wp_grid(object), ...),
    'coefs' = plot_coefs(object, ...),
    'round' = plot_round(object, ...)
  )
}

## lb approach ----
safely_glm <- purrr::safely(glm)
fit_window_wp_coefs <- function(data, earliest_seconds_elapsed, latest_seconds_elapsed, is_pre_plant) {
  filt <- dplyr::filter(
    data,
    .data[['model_seconds_elapsed']] >= earliest_seconds_elapsed, 
    .data[['model_seconds_elapsed']] <= latest_seconds_elapsed
  )
  
  msg <- sprintf(
    'for `model_seconds_elapsed >= %s & model_seconds_elapsed <= %s.',
    earliest_seconds_elapsed, 
    latest_seconds_elapsed
  )
  if (nrow(filt) <= 2) {
    message(sprintf('Less than 3 records %s', msg))
    return(NULL)
  }
  
  if (length(unique(filt$win_round)) == 1) {
    warning(sprintf('Only one unique value in target variable %s', msg))
  } 
  
  feature_names <- get_lb_window_feature_names(is_pre_plant = is_pre_plant)
  
  default_result <- rlang::set_names(
    rep(NA, length(feature_names)), 
    feature_names
  )
  
  form <- sprintf('%s ~ .', target_name)
  fit <- safely_glm(
    as.formula(form), 
    data = dplyr::select(
      filt, 
      .data[['win_round']], 
      tidyselect::vars_select_helpers$all_of(feature_names)
    ), 
    family = binomial(link = 'logit')
  )
  
  if (!is.null(fit$error)) {
    message(sprintf('Error %s.\n%s', msg, fit$error))
    return(default_result)
  }
  coefficients(fit$result)
}

estimate_window_coefs <- function(data, is_pre_plant) {
  
  seconds_grid <- generate_seconds_grid(is_pre_plant = is_pre_plant)
  seconds_grid |>
    dplyr::transmute(
      'model_seconds_elapsed' = .data[['min_second']],
      'coefs' = purrr::map2(
        .data[['min_second']], 
        .data[['max_second']], 
        ~fit_window_wp_coefs(
          data = data, 
          earliest_seconds_elapsed = ..1, 
          latest_seconds_elapsed = ..2,
          is_pre_plant = is_pre_plant
        )
      )
    ) |> 
    tidyr::unnest_wider(.data[['coefs']])
}

# halflife <- \(x) 1/(x^0.5)
convert_seconds_to_weights_lb <- function(x, is_pre_plant) {
  max_second <- ifelse(isTRUE(is_pre_plant), max_pre_plant_second, max_post_plant_second)
  dplyr::na_if(sqrt((max_second - x) / max_second), NaN)
}

fit_coef_model <- function(data, y, is_pre_plant) {
  wts <- convert_seconds_to_weights_lb(data[['model_seconds_elapsed']], is_pre_plant)
  loess(
    data[[y]] ~ data[['model_seconds_elapsed']], 
    weights = wts,
    span = 0.5
  )
}

fit_wp_model_lb_state <- function(data, is_pre_plant) {
  
  coefs <- estimate_window_coefs(
    data, 
    is_pre_plant = is_pre_plant
  )
  
  feature_names <- get_lb_window_feature_names(is_pre_plant = is_pre_plant)
  feature_names <- c(feature_names, '(Intercept)')
  
  coefs <- tidyr::fill(
    coefs,
    tidyselect::vars_select_helpers$all_of(feature_names), 
    .direction = 'downup'
  )
  
  models <- feature_names |> 
    rlang::set_names() |> 
    purrr::map(
      ~fit_coef_model(coefs, y = .x, is_pre_plant = is_pre_plant)
    )
  
  res <- list(
    model = models,
    coefs = coefs
  )
  class(res) <- c('wp_model_lb_state', class(res))
  res
}

predict.wp_model_lb_state <- function(object, new_data, ...) {
  model <- object$model
  nms <- names(model)
  n_row <- nrow(new_data)
  n_col <- length(nms)
  m <- matrix(rep(0L, times = n_row * n_col), nrow = n_row, ncol = n_col)
  
  colnames(m) <- nms
  for(el in nms) {
    ## was having some issues with last second predictions (even if they showed 45 exactly)
    ##   without rounding
    m[, el] <- predict(model[[el]], newdata = round(new_data$model_seconds_elapsed))
  }
  
  new_data$`(Intercept)` <- 1L
  ## Check that they are in the same order
  stopifnot(colnames(m) == colnames(new_data[, nms]))
  
  log_odds <- rowSums(m * as.matrix(new_data[, nms]))
  plogis(log_odds) ## logit
}


fit_wp_model_lb <- function(data, ...) {
  fit_wp_model_states(data = data, method = 'lb', ...)
}

add_lb_plot_caption <- function(...) {
  list(
    ...,
    ggplot2::labs(
      caption = 'Method: LOESS glm models'
    )
  )
}

plot_coefs <- function(model, ...) {
  
  df <- dplyr::bind_rows(
    dplyr::mutate(model[['pre']][['coefs']], 'is_pre_plant' = TRUE),
    dplyr::mutate(
      model[['post']][['coefs']],
      'model_seconds_elapsed' = .data[['model_seconds_elapsed']] + max_pre_plant_second + max_pre_plant_second_buffer,
      'is_pre_plant' = FALSE
    )
  ) |> 
    tidyr::pivot_longer(
      -c(.data[['model_seconds_elapsed']], .data[['is_pre_plant']]),
      names_to = 'feature',
      values_to = 'value'
    )
  
  df$weights <- convert_seconds_to_weights_lb(df[['model_seconds_elapsed']], df[['is_pre_plant']])
  
  df |> 
    ggplot2::ggplot() +
    ggplot2::aes(
      x = .data[['model_seconds_elapsed']],
      y = .data[['value']],
      group = .data[['is_pre_plant']],
      color = .data[['is_pre_plant']]
    ) +
    ggplot2::guides(color = 'none') +
    # geom_vline(
    #   color = 'white',
    #   aes(xintercept = max_pre_plant_second + max_pre_plant_second_buffer / 2)
    # ) +
    # geom_text(
    #   aes()
    # ) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(
      # method.args = list(span = 0.5, df$weights),
      method = 'loess',
      formula = y ~ x
    ) +
    scale_x_continuous_wp_states() +
    ggplot2::facet_wrap(~.data[['feature']], scales = 'free_y') +
    add_lb_plot_caption() +
    ggplot2::labs(
      title = 'Offensive win probability',
      y = 'Coefficient estimate'
    )
}


## xgb approach ----
options(yardstick.event_level = 'second') ## maybe i should just withr::with_seed in functions?
fit_wp_model_xgb_state <- function(data, is_pre_plant, tune = FALSE) {
  
  all_features <- get_all_features(
    is_pre_plant = is_pre_plant,
    named = TRUE
  )
  
  form <- sprintf('%s ~ .', target_name)
  rec <- recipes::recipe(
    as.formula(form), 
    data = data |> 
      dplyr::select(
        .data[['win_round']], 
        tidyselect::vars_select_helpers$all_of(names(all_features))
      )
  )
  
  n_features <- length(all_features)
  if (isTRUE(tune)) {
    mtry <- tune::tune()
    trees <- tune::tune()
    min_n <- tune::tune()
    # tree_depth <- tune::tune()
    # sample_size <- tune::tune()
  } else if (isTRUE(is_pre_plant)) {
    mtry <- 2L # 4L
    trees <- 50L # 500L
    min_n <- 25L # 10L
  } else if (isFALSE(is_pre_plant)) {
    mtry <- 1L # 3L
    trees <- 50L
    min_n <- 5L # 25L
  }
  
  spec <- parsnip::boost_tree(
    mtry = !!mtry, 
    trees = !!trees,
    min_n = !!min_n
  ) |> 
    parsnip::set_mode('classification') |> 
    parsnip::set_engine(
      monotone_constraints = !!unname(all_features),
      engine = 'xgboost'
    )
  
  wf <- workflows::workflow(
    preprocessor = rec, 
    spec = spec
  )
  
  if (isTRUE(tune)) {
    
    withr::local_seed(42)
    folds <- rsample::group_vfold_cv(
      data,
      v = 5,
      group = .data[['round_id']]
    )
    
    n_row <- nrow(data)
    param_grid <- tidyr::crossing(
      # tree_depth = c(1, 2, 3, 4, 8),
      # sample_size = round(n_row / 100, n_row / 50, n_row / 10),
      mtry = seq.int(n_features - 2L, n_features, by = 1L),
      trees = c(1L, 5L, 10L, 50L, 100L, 150L, 250L, 500L),
      min_n  = c(2L, 5L, 10L, 25L, 50L, 100L)
    )
    
    # ms <- yardstick::metric_set(yardstick::roc_auc)
    ms <- yardstick::metric_set(brier_score)
    
    tune_res <- tune::tune_grid(
      wf,
      resamples = folds,
      grid = param_grid,
      control = tune::control_grid(verbose = TRUE),
      metrics = ms
    )
    
    best_met <- tune::select_best(tune_res, 'brier_score')
    print(best_met) ## so that we know what to update the hard-coded values to
    wf <- tune::finalize_workflow(wf, best_met)
  }
  
  model <- parsnip::fit(wf, data)
  class(model) <- c('wp_model_xgb_state', class(model))
  model
}

## Could use `NextMethod()`, but i want a vector returned
predict.wp_model_xgb_state <- function(object, new_data, type = 'prob', ...) {
  class(object) <- setdiff(class(object), 'wp_model_xgb_state')
  ## go to `predict.workflow`
  predict(object, new_data, type = type, ...)[, 2][[1]]
}

fit_wp_model_xgb <- function(data, ...) {
  fit_wp_model_states(data = data, method = 'xgb', ...)
}

add_xgb_plot_caption <- function(...) {
  list(
    ...,
    ggplot2::labs(
      caption = 'Method: xgboost model with monotonic constraints'
    )
  )
}
