
## generic ----
target_name <- 'win_round'
default_side <- 'o'
max_pre_plant_second <- 90L
max_post_plant_second <- 45L

## TODO: Add validation for is_pre_plant
## Add extra second so that LOESS span can handle exactly on the last second
generate_seconds_grid <- function(is_pre_plant) {
  if (isTRUE(is_pre_plant)) {
    min_second <- c(seq(0, 75, by = 3), seq(75, 85, by = 1), seq(86, 90, by = 0.5))
    max_second <- c(seq(15, 90, by = 3), seq(80, 90, by = 1), rep(90, 9))
  } else {
    min_second <- c(seq(0, 30, 2), seq(30, 38, 1), seq(39, 45, by = 0.5))
    max_second <- c(seq(10, 40, 2), seq(37, 45, 1), rep(45, 13))
  }
  tibble::tibble(
    min_second = min_second,
    max_second = max_second
  )
}

get_max_second <- function(is_pre_plant) {
  ifelse(isTRUE(is_pre_plant), max_pre_plant_second, max_post_plant_second)
}

get_all_features <- function(is_pre_plant, named = FALSE) {
  
  features <- c(
    'opponent_diff' = 'integer',
    # 'is_offense' = 'binary',
    'is_kill_on_attempted_clinch' = 'binary',
    'has_started_clinch' = 'binary',
    'is_initial_bomb_carrier_killed' = 'binary'
  )
  
  if (isFALSE(is_pre_plant)) {
    idx_to_drop <- which(names(features) == 'is_initial_bomb_carrier_killed')
    idx_to_keep <- setdiff(seq.int(1, length(features)), idx_to_drop)
    features <- features[idx_to_keep]
  }
  
  if (isTRUE(named)) {
    return(features)
  }
  
  names(features)
}

validate_colnames <- function(data, is_predict = FALSE) {
  
  base_cols <- c(
    'is_pre_plant', 
    'side'
  )
  base_cols <- if (isFALSE(is_predict)) {
    c(
      base_cols,
      'n_team_remaining',
      'n_opponent_remaining',
      'model_seconds_remaining'
    )
  } else {
    c(
      base_cols,
      'is_wp_hardcoded',
      'hardcoded_wp'
    )
  }
  
  nms_diff <- setdiff(
    base_cols,
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
    'pre' = dplyr::filter(data, .data[['is_pre_plant']]),
    'post' = dplyr::filter(data, !.data[['is_pre_plant']])
  )
}

fit_wp_model_states <- function(data, ...) {
  
  model_data <- split_model_data(data = data)
  
  ## things get messy if using imap here, so just be explicit instead
  models <- list(
    'pre' = fit_wp_model_state(model_data[['pre']], is_pre_plant = TRUE, ...),
    'post' = fit_wp_model_state(model_data[['post']], is_pre_plant = FALSE, ...)
  )
  
  class(models) <- c('wp_model', class(models))
  models
}

## TODO: Do less hard-coding here with features
generate_pred_grid <- function(is_pre_plant) {
  feature_names <- get_all_features(
    is_pre_plant = is_pre_plant, 
    named = TRUE
  )

  binary_feature_names <- feature_names |> 
    purrr::keep(~.x == 'binary') |> 
    names()
  
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
    'model_seconds_elapsed' = seq(0L, (max_second - 1L), by = 0.5),
    'n_team_remaining' = 1L:max_players,
    'n_opponent_remaining' = 1L:max_players,
    'is_kill_on_attempted_clinch' = binary_values,
    'has_started_clinch' = binary_values,
    'is_initial_bomb_carrier_killed' = is_initial_bomb_carrier_killed
  ) |> 
    dplyr::mutate(
      'is_offense' = .data[['side']] == 'o',
      'opponent_diff' = .data[['n_team_remaining']] - .data[['n_opponent_remaining']],
      'model_seconds_remaining' = ifelse(
        .data[['is_pre_plant']],
        max_pre_plant_second - .data[['model_seconds_elapsed']],
        max_post_plant_second - .data[['model_seconds_elapsed']]
      ),
      'hardcoded_wp' = NA_real_,
      'is_wp_hardcoded' = FALSE
    )
}

add_hardcoded_wp_cols <- function(df) {
  
  df |> 
    dplyr::mutate(
      
      is_1v0_post_plant = !.data[['is_pre_plant']] & (.data[['n_team_remaining']] == 1) & (.data[['n_opponent_remaining']] == 0),
      is_0v1_post_plant = !.data[['is_pre_plant']] & (.data[['n_team_remaining']] == 0) & (.data[['n_opponent_remaining']] == 1),
      is_Nv0_post_plant = !.data[['is_pre_plant']] & (.data[['n_team_remaining']] > 1) & (.data[['n_opponent_remaining']] == 0),
      is_0vN_post_plant = !.data[['is_pre_plant']] & (.data[['n_team_remaining']] == 0) & (.data[['n_opponent_remaining']] > 1),
      
      hardcoded_wp = dplyr::case_when(
        
        .data[['side']] == 'o' & .data[['activity']] == 'Defuse' ~ 0,
        .data[['side']] == 'd' & .data[['activity']] == 'Defuse' ~ 1,
        
        .data[['side']] == 'o' & !.data[['is_pre_plant']] & (.data[['n_team_remaining']] == 0) & .data[['model_seconds_remaining']] >= 7.5 & .data[['activity']] == 'Start Defuse' ~ 0,
        .data[['side']] == 'd' & !.data[['is_pre_plant']] & (.data[['n_opponent_remaining']] == 0) & .data[['model_seconds_remaining']] >= 7.5 & .data[['activity']] == 'Start Defuse' ~ 1,
        
        is_1v0_post_plant & .data[['model_seconds_remaining']] >= 15 ~ 1,
        is_1v0_post_plant & .data[['model_seconds_remaining']] >= 12.5 ~ 0.9,
        is_1v0_post_plant & .data[['model_seconds_remaining']] >= 10 ~ 0.65,
        is_1v0_post_plant & .data[['model_seconds_remaining']] >= 7.5 ~ 0.5,
        is_1v0_post_plant & .data[['model_seconds_remaining']] < 7.5  ~ 0,
        
        is_0v1_post_plant & .data[['model_seconds_remaining']] >= 15 ~ 0,
        is_0v1_post_plant & .data[['model_seconds_remaining']] >= 12.5  ~ 0.1,
        is_0v1_post_plant & .data[['model_seconds_remaining']] >= 10 ~ 0.35,
        is_0v1_post_plant & .data[['model_seconds_remaining']] >= 7.5 ~ 0.5,
        is_0v1_post_plant & .data[['model_seconds_remaining']] < 7.5  ~ 1,
        
        is_Nv0_post_plant & .data[['model_seconds_remaining']] >= 12.5 ~ 1,
        is_Nv0_post_plant & .data[['model_seconds_remaining']] >= 10 ~ 0.75,
        is_Nv0_post_plant & .data[['model_seconds_remaining']] >= 7.5 ~ 0.5,
        is_Nv0_post_plant & .data[['model_seconds_remaining']] < 7.5  ~ 0.25,
        
        is_0vN_post_plant & .data[['model_seconds_remaining']] >= 12.5 ~ 0,
        is_0vN_post_plant & .data[['model_seconds_remaining']] >= 10 ~ 0.25,
        is_0vN_post_plant & .data[['model_seconds_remaining']] >= 7.5 ~ 0.5,
        is_0vN_post_plant & .data[['model_seconds_remaining']] < 7.5  ~ 0.75,
        
        TRUE ~ NA_real_
      ),
      is_wp_hardcoded = !is.na(.data[['hardcoded_wp']])
    ) |> 
    dplyr::select(
      -tidyselect::vars_select_helpers$matches('is_[10N]v[10N]_post_plant')
    )
}

## TODO: Could fit a model for the specific situation of a post-plant 1v0 defuse, for which the defense should be given 0 WP if the seconds remaining is less than 7.5, but something between 0 and 1 if there are between 15 and 7.5 seconds remaining.
## Could even allow for 2v0, 3v0, and 4v0 predictions. These would not have a hard rule for the last 7.5 seconds since a person could be defusing while a teammate eliminates an opponent with less than 7.5 seconds remaining.
# d <- all_model_pbp |> filter(side == 'o', !is_pre_plant, n_team_pre_activity == 1, n_team_remaining == 0, n_opponent_remaining == 1)
# fit <- glm(win_round ~ model_seconds_elapsed, d, family = 'binomial')
# round(predict(fit, tibble(model_seconds_elapsed = c(0:45)), type = 'response'), 2)

predict.wp_model <- function(object, new_data, ...) {
  
  validate_colnames(data = new_data, is_predict = TRUE)
  
  pred <- ifelse(
    new_data[['is_pre_plant']],
    predict(object[['pre']], new_data),
    predict(object[['post']], new_data)
  )
  
  pred <- ifelse(
    new_data[['side']] == 'o',
    pred,
    1 - pred
  )
  
  ifelse(
    new_data[['is_wp_hardcoded']],
    new_data[['hardcoded_wp']],
    pred
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
  c(TRUE, FALSE) |> 
    purrr::map_dfr(
      ~generate_wp_state_grid(
        model,
        is_pre_plant = .x
      )
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
  
  feature_names <-   get_all_features(
    is_pre_plant = is_pre_plant, 
    named = FALSE
  )
  
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
convert_seconds_to_weights <- function(x, is_pre_plant) {
  max_second <- ifelse(isTRUE(is_pre_plant), max_pre_plant_second, max_post_plant_second)
  dplyr::na_if(sqrt((max_second - x) / max_second), NaN)
}

fit_coef_model <- function(data, y, is_pre_plant) {
  # wts <- convert_seconds_to_weights(data[['model_seconds_elapsed']], is_pre_plant)
  loess(
    data[[y]] ~ data[['model_seconds_elapsed']], 
    # weights = wts,
    span = 0.5
  )
}

fit_wp_model_state <- function(data, is_pre_plant) {
  
  coefs <- estimate_window_coefs(
    data, 
    is_pre_plant = is_pre_plant
  )
  
  feature_names <-   get_all_features(
    is_pre_plant = is_pre_plant, 
    named = FALSE
  )
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
  class(res) <- c('wp_model_state', class(res))
  res
}

predict.wp_model_state <- function(object, new_data, ...) {
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


fit_wp_model <- function(data, ...) {
  fit_wp_model_states(data = data, ...)
}
