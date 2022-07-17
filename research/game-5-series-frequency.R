# https://twitter.com/AstrosInMyAfro/status/1548108679563530241?s=20&t=xWhKd8dOWQ_fhJRmgMc--A
n_rounds_max <- 5
n_sims <- 100000
set.seed(42)
w <- sample(c(0, 1), size = n_rounds_max * n_sims, replace = TRUE)

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
  filter(cumu_wl_max <= 3)

sim_rounds_to_drop <- anti_join(
  sim_rounds |> 
    filter(cumu_wl_max == 3L),
  sim_rounds |> 
    filter(cumu_wl_max == 3L) |> 
    group_by(i) |> 
    slice_min(r, n = 1) |> 
    ungroup()
)

sim_rounds <- sim_rounds |> 
  anti_join(
    sim_rounds_to_drop
  )
sim_rounds

e_series_streaks <- sim_rounds |> 
  inner_join(
    sim_rounds |> 
      filter(cumu_wl_max == 3) |> 
      mutate(
        zeros_win = cumu_l == cumu_wl_max
      ) |> 
      select(i, zeros_win)
  ) |> 
  mutate(
    across(w, ~ifelse(zeros_win, abs(1 - .x), .x))
  ) |> 
  group_by(i) |> 
  summarize(
    cumu_w = max(cumu_w),
    cumu_l = max(cumu_l),
    ws = paste0(w, collapse = '')
  ) |> 
  ungroup() |> 
  mutate(
    wins = ifelse(cumu_w == 3, cumu_w, cumu_l),
    losses = ifelse(cumu_w == 3, cumu_l, cumu_w)
  ) |> 
  unite(
    record, wins, losses, sep = '-'
  ) |> 
  count(record, ws, sort = TRUE) |> 
  mutate(prop = n / sum(n))

e_series_streaks |> 
  group_by(record) |> 
  summarize(
    across(n, sum)
  ) |> 
  mutate(prop = n / sum(n))

## What are the odds of 15 of 17 game 5 series?
n_rounds <- 17
set.seed(42)
w <- sample(c(0, 1), size = n_rounds * n_sims, prob = c(0.625, 0.375), replace = TRUE)

tibble(
  i = rep(1:n_sims, each = n_rounds),
  r = rep(1:n_sims, times = n_rounds),
  w = w
) |> 
  group_by(i) |> 
  summarize(
    across(w, sum)
  ) |> 
  count(w) |> 
  mutate(prop = n / sum(n))
