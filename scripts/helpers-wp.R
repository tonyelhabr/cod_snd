
max_quietly <- function(...) {
  f <- quietly(max)
  f(...)$result
}

generate_seconds_grid <- function(is_pre_plant = TRUE) {
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

get_max_second <- function(is_pre_plant = TRUE) {
  ifelse(isTRUE(is_pre_plant), 90L, 45L)
}

safely_glm <- safely(glm)
library(recipes)
model_window_wp <- function(pbp_grid, seconds_elapsed1, seconds_elapsed2) {
  filt <- pbp_grid |> 
    filter(seconds_elapsed >= seconds_elapsed1, seconds_elapsed <= seconds_elapsed2)

  msg <- sprintf('for `seconds_elapsed >= %s & seconds_elapsed <= %s.', seconds_elapsed1, seconds_elapsed2)
  if (nrow(filt) == 0) {
    message(sprintf('No data %s', msg))
    return(NULL)
  }
  
  if (length(unique(filt$win_round)) == 1) {
    warning(sprintf('Only one unique value in target variable %s', msg))
  } 
  
  binary_terms <- c(
    'is_offense',
    'is_initial_bomb_carrier_killed',
    'is_during_attempted_plant',
    'is_during_attempted_defuse'
  )
  
  binary_terms_to_keep <- binary_terms |> 
    keep(
      ~{
        # has_more_than_one_value <- length(unique(filt[[.x]])) > 1L
        # if (isTRUE(has_more_than_one_value)) {
        #   return(TRUE)
        # } 
        # warning(sprintf('Only one unique value in feature `%s` %s', .x, msg))
        # return(FALSE)
        length(unique(filt[[.x]])) > 1L
      }
    )
  
  all_terms <- c(
    'prev_opponent_diff',
    binary_terms_to_keep
  )
  
  default_result <- set_names(rep(NA, length(all_terms)), all_terms)

  fit <- safely_glm(
    win_round ~ ., 
    data = filt |> select(win_round, all_of(all_terms)), 
    family = binomial(link = 'logit')
  )
  fit <- recipe(
    win_round ~ .,
    data = filt |> select(win_round, all_of(all_terms))
  ) |> 
    step_interact(everything())
  
  if (!is.null(fit$error)) {
    message(sprintf('Error %s.\n%s', msg, fit$error))
    return(default_result)
  }
  coefficients(fit$result)
}

estimate_window_coefs <- function(pbp_grid, is_pre_plant = TRUE, overwrite = FALSE) {
  
  prefix <- ifelse(is_pre_plant, 'pre', 'post')
  path <- file.path('data', sprintf('%s_plant_wp_coefs.csv', prefix))
  if (file.exists(path) & isFALSE(overwrite)) {
    return(read_csv(path))
  }
  
  seconds_grid <- generate_seconds_grid(is_pre_plant = is_pre_plant)
  
  coefs <- seconds_grid |>
    mutate(
      coefs = map2(min_second, max_second, ~model_window_wp(pbp_grid, ..1, ..2))
    ) |> 
    unnest_wider(coefs)
  write_csv(coefs, path)
  coefs
}

fit_coef_model <- function(data, y) {
  loess(
    data[[y]] ~ data[['min_second']], 
    span = 0.5
  )
}

fit_wp_model <- function(data, coefs = setdiff(colnames(data), sprintf('%s_second', c('min', 'max')))) {
  models <- coefs |> 
    set_names() |> 
    map(
      ~fit_coef_model(data, y = .x)
    )
  
  class(models) <- c('cod_wp_model', class(models))
  models
}

predict.cod_wp_model <- function(object, data, ...) {
  
  nms <- names(object)
  n_row <- nrow(data)
  n_col <- length(nms)
  m <- matrix(rep(0L, times = n_row * n_col), nrow = n_row, ncol = n_col)
  
  colnames(m) <- nms
  for(el in nms) {
    m[, el] <- predict(object[[el]], newdata = data$seconds_elapsed)
  }
  
  ## Check that they are in the same order
  stopifnot(colnames(m) == colnames(data[, nms]))
  
  log_odds <- rowSums(m * as.matrix(data[, nms]))
  plogis(log_odds) ## logit
}

generate_pred_grid <- function(model, is_pre_plant = TRUE) {
  nms <- fit$fit$fit$fit$feature_names
  binary_features <- setdiff(nms, c('seconds_elapsed', 'prev_opponent_diff'))
  
  max_second <- get_max_second(is_pre_plant)
  max_diff <- 3L
  
  binary_l <- vector(mode = 'list', length(binary_features))
  names(binary_l) <- binary_features
  for(el in names(binary_l)) {
    binary_l[[el]] <- c(0L, 1L)
  }
  
  crossing(
    !!!append(
      list(
        seconds_elapsed = 0L:(max_second - 1L),
        prev_opponent_diff = -max_diff:max_diff
      ),
      binary_l
    )
  )
}

generate_wp_grid <- function(model, is_pre_plant = TRUE) {
  nms <- names(model)
  binary_features <- setdiff(nms, c('prev_opponent_diff', '(Intercept)'))
  
  max_second <- get_max_second(is_pre_plant)
  max_diff <- 3L
  
  binary_l <- vector(mode = 'list', length(binary_features))
  names(binary_l) <- binary_features
  for(el in names(binary_l)) {
    binary_l[[el]] <- c(0L, 1L)
  }
  
  pred_grid <- crossing(
    !!!append(
      list(
        seconds_elapsed = 0L:(max_second - 1L),
        prev_opponent_diff = -max_diff:max_diff
      ),
      binary_l
    )
  )
  
  pred_grid$`(Intercept)` <- 1L
  pred_grid$wp <- predict(
    model,
    pred_grid
  )
  
  pred_grid
}

plot_wp_grid <- function(df) {
  df |> 
    mutate(
      side = ifelse(is_offense == 1L, 'Offense', 'Defense')
    ) |> 
    ggplot() +
    aes(x = seconds_elapsed, y = wp) +
    geom_hline(
      color = 'white',
      aes(yintercept = 0.5)
    ) +
    geom_step(
      size = 1.5,
      aes(color = factor(prev_opponent_diff))
    ) +
    facet_wrap(~side, scales = 'fixed') +
    guides(
      color = guide_legend(
        title = 'Net # of players', 
        override.aes = list(size = 3)
      )
    ) +
    ggsci::scale_color_tron() +
    scale_y_continuous(labels = scales::comma) +
    coord_cartesian(ylim = c(0, 1)) +
    theme(
      legend.position = 'top'
    ) +
    labs(
      x = 'Seconds Elapsed',
      y = 'Win Probability'
    )
}
