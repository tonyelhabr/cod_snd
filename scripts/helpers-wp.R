
## yardstick ----
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

## TODO: Add validation for is_pre_plant
generate_seconds_grid <- function(is_pre_plant) {
  if (isTRUE(is_pre_plant)) {
    # min_second <- c(seq(0, 30, 3), seq(30, 60, 2), seq(50, 75, 1), seq(75, 85, 1), seq(86, 90))
    # max_second <- c(seq(15, 45, 3), seq(40, 70, 2), seq(60, 85, 1), seq(80, 90, 1), rep(90, 5))
    min_second <- c(seq(0, 75, by = 3), seq(75, 85, by = 1), seq(86, 88))
    max_second <- c(seq(15, 90, by = 3), seq(80, 90, by = 1), rep(90, 3))
  } else {
    min_second <- c(seq(0, 30, 2), seq(30, 38, 1), seq(39, 45))
    max_second <- c(seq(10, 40, 2), seq(37, 45, 1), rep(45, 7))
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
  
  all_features <- c(
    'model_seconds_elapsed' = 1,
    'opponent_diff' = -1,
    'is_kill_on_attempted_clinch' = 1,
    'is_initial_bomb_carrier_killed' = -1,
    'prev_team_round_wl_diff' = 0, # -1,
    'won_prior_round' = 0
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
}

get_lb_window_feature_names <- function(is_pre_plant) {
  get_all_features(
    is_pre_plant = is_pre_plant, 
    named = FALSE,
    method = 'lb'
  )
}

validate_colnames <- function(data) {
  all_feature_names <- c(TRUE, FALSE) |> 
    purrr::map(
      ~get_all_features(
        is_pre_plant = .x, 
        named = FALSE
      )
    ) |> 
    purrr::flatten_chr() |> 
    unique()
  
  nms_diff <- setdiff(c('is_pre_plant', 'side', all_feature_names), colnames(data))
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
  
  class(models) <- c('wp_model', class(models))
  models
}


## TODO: Do less hard-coding here with features
generate_pred_grid <- function(is_pre_plant) {
  binary_feature_names <- c(
    'is_kill_on_attempted_clinch',
    'is_initial_bomb_carrier_killed',
    'won_prior_round'
  )
  
  binary_l <- vector(mode = 'list', length(binary_feature_names))
  names(binary_l) <- binary_feature_names
  for(el in names(binary_l)) {
    binary_l[[el]] <- c(0L, 1L)
  }
  
  max_second <- get_max_second(is_pre_plant)
  max_player_diff <- 3L
  max_round_diff <- 5L
  
  max_is_initial_bomb_carrier_killed <- ifelse(isTRUE(is_pre_plant), 1L, 0L)
  binary_values <- 0L:1L
  tidyr::crossing(
    'side' = c('o', 'd'),
    'is_pre_plant' = is_pre_plant,
    'model_seconds_elapsed' = 0L:(max_second - 1L),
    'opponent_diff' = -max_player_diff:max_player_diff,
    'prev_team_round_wl_diff' = -max_round_diff:max_round_diff,
    'is_kill_on_attempted_clinch' = binary_values,
    'won_prior_round' = binary_values,
    'is_initial_bomb_carrier_killed' = 0L:max_is_initial_bomb_carrier_killed
  )
}

predict.wp_model <- function(object, new_data, ...) {
  
  validate_colnames(data = new_data)
  
  pred <- ifelse(
    new_data[['is_pre_plant']],
    predict(object[['pre']], new_data, ...),
    predict(object[['post']], new_data, ...)
  )
  
  ifelse(
    new_data[['side']] == 'o',
    pred,
    1 - pred
  )
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
  wp_grid <- c(TRUE, FALSE) |> 
    purrr::map_dfr(
      ~generate_wp_state_grid(
        model,
        is_pre_plant = .x
      )
    )
  class(wp_grid) <- c('wp_grid', class(wp_grid))
  wp_grid
}

## TODO: Weight by observed frequency
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

# ggsci::pal_tron('legacy', 1)(7)
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

autoplot.wp_grid <- function(data) {
  filt <- dplyr::filter(
    data, 
    .data[['side']] == default_side,
    .data[['won_prior_round']] == 0,
    .data[['prev_team_round_wl_diff']] == 0
  )
  df <- c(
    'is_initial_bomb_carrier_killed',
    'is_kill_on_attempted_clinch' # ,
    # 'won_prior_round',
    # 'prev_team_round_wl_diff'
  ) |> 
    rlang::set_names() |> 
    purrr::map_dfr(
      ~summarize_pred_grid_across_features(
        filt, 
        .x
      ), 
      .id = 'feature'
    ) |> 
    dplyr::mutate(
      'feature_value' = 
        sprintf(
          '%s: %s', 
          .data[['feature']], 
          ifelse(.data[['value']] == 1, 'yes', 'no')
        )
    )
  
  pre <- df |> filter(is_pre_plant)
  max_pre_plant_second <- 90L
  post <- df |> 
    filter(!is_pre_plant) |> 
    mutate(
      'model_seconds_elapsed' = model_seconds_elapsed + max_pre_plant_second
    )
  
  dplyr::bind_rows(
    pre,
    post
  ) |> 
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
    # ggsci::scale_color_tron() +
    ggplot2::scale_color_manual(values = player_diff_pal) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::facet_wrap(~.data[['feature_value']], dir = 'v', nrow = 2) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::theme(
      legend.position = 'top'
    ) +
    ggplot2::labs(
      title = 'Offensive Win Probability',
      x = 'Seconds Elapsed',
      y = 'Win Probability'
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
  
  form <- sprintf('%s ~ . - 1', target_name)
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
    dplyr::mutate(
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

fit_coef_model <- function(data, y) {
  loess(
    data[[y]] ~ data[['min_second']], 
    span = 0.5
  )
}

fit_wp_model_lb_state <- function(data, is_pre_plant) {
  
  coefs <- estimate_window_coefs(
    data, 
    is_pre_plant = is_pre_plant
  )
  
  feature_names <- get_lb_window_feature_names(is_pre_plant = is_pre_plant)
  
  coefs <- tidyr::fill(
    coefs,
    tidyselect::vars_select_helpers$all_of(feature_names), 
    .direction = 'downup'
  )
  
  models <- feature_names |> 
    rlang::set_names() |> 
    purrr::map(
      ~fit_coef_model(coefs, y = .x)
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
    m[, el] <- predict(model[[el]], newdata = new_data$model_seconds_elapsed)
  }
  
  ## Check that they are in the same order
  stopifnot(colnames(m) == colnames(new_data[, nms]))
  
  log_odds <- rowSums(m * as.matrix(new_data[, nms]))
  plogis(log_odds) ## logit
}


fit_wp_model_lb <- function(data, ...) {
  fit_wp_model_states(data = data, method = 'lb', ...)
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
    mtry <- 5L
    trees <- 10L
    min_n <- 2L
  } else if (isFALSE(is_pre_plant)) {
    mtry <- n_features - 1L
    trees <- 100L
    min_n <- 25L
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
