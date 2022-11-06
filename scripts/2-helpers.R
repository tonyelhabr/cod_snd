
max_quietly <- function(...) {
  f <- quietly(max)
  f(...)$result
}

predict.cod_wp_model <- function(object, data, ...) {
  
  stopifnot(sort(names(object)) == c('prev_opponent_diff', 'side_o'))
  stopifnot(all(c('seconds_elapsed', 'side') %in% names(data)))
  
  opponent_diff_pred <- predict(object[['prev_opponent_diff']], newdata = data$seconds_elapsed)
  side_pred <- predict(object[['side_o']], newdata = data$seconds_elapsed)
  side_multiplier <- ifelse(data$side == 'o', 1L, -1L)
  log_odds <- opponent_diff_pred * data$prev_opponent_diff + side_pred * side_multiplier
  
  plogis(log_odds) ## logit
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

estimate_window_coefs <- function(pbp_grid, is_pre_plant = TRUE, overwrite = FALSE) {
  
  prefix <- ifelse(is_pre_plant, 'pre', 'post')
  path <- file.path('data', sprintf('%s_plant_wp_coefs.csv', prefix))
  if (file.exists(path) & isFALSE(overwrite)) {
    return(read_csv(path))
  }
  
  seconds_grid <- generate_seconds_grid(is_pre_plant = is_pre_plant)
  
  coefs <- seconds_grid |>
    mutate(
      coefs = 
        map2(
          min_second, max_second,
          ~{
            filt <- pbp_grid |> 
              filter(seconds_elapsed >= ..1, seconds_elapsed <= ..2)
            
            default_res <- c('prev_opponent_diff' = NA, 'sideo' = NA)
            msg <- sprintf('for `seconds_elapsed >= %s & seconds_elapsed <= %s.', ..1, ..2)
            if (nrow(filt) == 0) {
              message(sprintf('No data %s', msg))
              return(default_res)
            }
            
            fit <- safely_glm(
              win_round ~ prev_opponent_diff + side - 1, 
              data = filt, 
              family = binomial(link = 'logit')
            )
            if (!is.null(fit$error)) {
              message(sprintf('Error %s.\n%s', msg, fit$error))
              return(default_res)
            }
            coefficients(fit$result)
          }
        )
    )
  
  coefs <- coefs |> 
    mutate(
      prev_opponent_diff = map_dbl(coefs, ~.x[['prev_opponent_diff']]),
      side_o = map_dbl(coefs, ~.x[['sideo']])
    ) |> 
    select(-coefs)
  write_csv(coefs, path)
  coefs
}

fit_coef_model <- function(data, y) {
  loess(
    data[[y]] ~ data[['min_second']], 
    span = 0.5
  )
}

fit_wp_model <- function(data, coefs = c('prev_opponent_diff', 'side_o')) {
  models <- coefs |> 
    set_names() |> 
    map(
      ~fit_coef_model(data, y = .x)
    )
  
  class(models) <- c('cod_wp_model', class(models))
  models
}

generate_wp_grid <- function(model, is_pre_plant = TRUE) {
  max_second <- get_max_second(is_pre_plant)
  max_diff <- 3L
  pred_grid <- crossing(
    seconds_elapsed = 0L:(max_second - 1L),
    prev_opponent_diff = -max_diff:max_diff
  ) |> 
    mutate(
      side = 'o'
    )
  
  pred_grid$wp <- predict(
    model,
    pred_grid
  )
  
  pred_grid
}

plot_wp_grid <- function(pred_grid) {
  pred_grid |> 
    ggplot() +
    aes(x = seconds_elapsed, y = wp) +
    geom_hline(aes(yintercept = 0.5)) +
    geom_step(
      aes(color = factor(prev_opponent_diff))
    ) +
    scale_y_continuous(labels = scales::comma) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
      x = 'Seconds Elapsed',
      y = 'Win Probability'
    )
}
