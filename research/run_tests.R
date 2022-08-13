
simulate_cod_round_arrangements <- function(n_sims = 10000, seed = 42) {
  n_rounds_max <- 11
  set.seed(seed)
  w <- sample(c(0, 1), size = n_rounds_max * n_sims, replace = TRUE)
  m <- matrix(w, nrow = n_sims, ncol = n_rounds_max)
  # data.frame(x)
  df <- as_tibble(m)
  names(df) <- sprintf('%d', 1:n_rounds_max)
  df$i <- 1:nrow(df)
  
  sim_rounds <- tibble(
    i = rep(1:n_sims, each = n_rounds_max),
    r = rep(1:n_sims, times = n_rounds_max),
    w = w
  ) |> 
    group_by(i) |> 
    mutate(
      cumu_w = cumsum(w),
      cumu_l = cumsum(w == 0)
    ) |> 
    ungroup() |> 
    mutate(
      cumu_wl_max = ifelse(cumu_w > cumu_l, cumu_w, cumu_l)
    ) |> 
    filter(cumu_wl_max <= 6)
  
  sim_rounds <- df |> 
    pivot_longer(
      -i,
      names_to = 'r',
      values_to = 'w'
    ) |> 
    mutate(
      across(r, as.integer)
    ) |> 
    group_by(i) |> 
    mutate(
      cumu_w = cumsum(w),
      cumu_l = cumsum(w == 0)
    ) |> 
    ungroup() |> 
    mutate(
      cumu_wl_max = ifelse(cumu_w > cumu_l, cumu_w, cumu_l)
    ) |> 
    filter(cumu_wl_max <= 6)
  
  sim_rounds_to_drop <- anti_join(
    sim_rounds |> 
      filter(cumu_wl_max == 6L),
    sim_rounds |> 
      filter(cumu_wl_max == 6L) |> 
      group_by(i) |> 
      slice_min(r, n = 1) |> 
      ungroup(), 
    by = c('i', 'r', 'w', 'cumu_w', 'cumu_l', 'cumu_wl_max')
  )
  
  sim_rounds <- sim_rounds |> 
    anti_join(
      sim_rounds_to_drop, 
      by = c('i', 'r', 'w', 'cumu_w', 'cumu_l', 'cumu_wl_max')
    )
  
  ## always from offensive perspective
  pre_expected_records <- sim_rounds |> 
    inner_join(
      sim_rounds |> 
        filter(cumu_wl_max == 6) |> 
        mutate(
          zeros_win = cumu_l == cumu_wl_max
        ) |> 
        select(i, zeros_win),
      by = 'i'
    ) |> 
    mutate(
      across(w, ~ifelse(zeros_win, abs(1 - .x), .x))
    )
  
  pre_expected_records |> 
    group_by(i) |> 
    summarize(
      cumu_w = max(cumu_w),
      cumu_l = max(cumu_l),
      ws = paste0(w, collapse = '-')
    ) |> 
    ungroup() |> 
    mutate(
      wins = ifelse(cumu_w == 6, cumu_w, cumu_l),
      losses = ifelse(cumu_w == 6, cumu_l, cumu_w)
    ) |> 
    unite(
      record, wins, losses, sep = '-'
    ) |> 
    count(record, ws, sort = TRUE) |> 
    mutate(prop = n / sum(n))
}

cod_expected_round_arrangements <- simulate_cod_round_arrangements()

cod_actual_round_arrangements <- cod_rounds |> 
  filter(win_series) |> 
  mutate(across(win_round, as.integer)) |> 
  group_by(year, event, series) |> 
  summarize(
    wins = max(cumu_w),
    losses = max(cumu_l),
    ws = paste0(win_round, collapse = '-')
  ) |> 
  ungroup() |> 
  unite(
    record, wins, losses, sep = '-'
  ) |> 
  count(record, ws, sort = TRUE) |> 
  mutate(prop = n / sum(n))

suppressWarnings(
  cod_run_tests <- cod_expected_round_arrangements |> 
    select(record, ws) |> 
    separate(ws, into = as.character(1:11), remove = FALSE) |> 
    pivot_longer(
      -c(record, ws),
      names_to = 'round',
      values_to = 'w',
      values_drop_na = TRUE
    ) |> 
    mutate(
      across(c(round, w), as.integer)
    ) |> 
    filter(record != '6-0') |> 
    select(-round) |> 
    group_by(record, ws) |> 
    summarize(
      w = list(w)
    ) |> 
    ungroup() |> 
    mutate(
      p_value = map_dbl(w, ~runs.test(.x, threshold = 0.5)$p.value),
      is_significant = p_value <= 0.05
    ) |> 
    select(record, ws, p_value, is_significant)
)

cod_expected_round_arrangements |> 
  select(-n) |> 
  rename(prop_expected = prop) |> 
  left_join(
    cod_actual_round_arrangements |> 
      select(-n) |> 
      rename(prop_actual = prop),
    by = c('record', 'ws')
  ) |> 
  inner_join(
    cod_run_tests,
    by = c('record', 'ws')
  ) |> 
  mutate(
    across(prop_actual, replace_na, 0)
  ) |> 
  count(is_significant, prop_actual > prop_expected)

