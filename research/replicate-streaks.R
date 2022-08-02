
library(tidyverse)

## http://keyonvafa.com/hot-hand/
get_post_streak_prob <- function(n, k, p = 0.5) {
  tosses <- rbinom(n, 1, p)
  runs <- rle(tosses)
  n_neg_after <- length(which(runs$values == 1 & runs$lengths >= k))
  n_pos_after <- sum(runs$lengths[which(runs$values == 1 & runs$lengths >= k)] - k)
  
  ## edge case
  if (n %in% cumsum(runs$lengths)[which(runs$values == 1 & runs$lengths >= k)]) {
    n_neg_after <- n_neg_after - 1
  }
  
  n_pos_after / (n_pos_after + n_neg_after)
}

simulate_post_streak_prob <- function(sims = 1000, ...) {
  rerun(
    sims,
    get_post_streak_prob(...)
  ) |> 
    flatten_dbl() |> 
    mean(na.rm = TRUE)
}

set.seed(42)
## 1.6 hours for 1k reruns and 100 sims
runs <- rerun(
  1000,
  crossing(
    n = 1:100,
    k = 1:4,
    p = c(0.25, 0.5, 0.75)
  ) |> 
    mutate(
      next_p = pmap_dbl(list(n, k, p), ~simulate_post_streak_prob(sims = 100, n = ..1, k = ..2, p = ..3))
    )
) |> 
  reduce(bind_rows)

agg_runs <- runs |> 
  filter(!is.nan(next_p)) |> 
  group_by(p, k, n) |> 
  summarize(
    across(next_p, mean)
  ) |> 
  ungroup() |>
  arrange(p, k)

p_streaks <- agg_runs |>
  group_by(p, k) |> 
  mutate(
    avg_next_p = slider::slide_dbl(next_p, .f = mean, .before = 5, .after = 0)
  ) |> 
  ungroup() |> 
  mutate(
    across(k, factor),
    group = sprintf('%s-%s', p, k)
  ) |> 
  ggplot() +
  theme_minimal() +
  aes(x = n, y = avg_next_p, color = k, group = group) +
  geom_line() +
  geom_hline(aes(yintercept = p)) +
  labs(x = NULL, y = NULL)
p_streaks
ggsave(p_streaks, filename = 'research/npk_replicated.png', width = 12, height = 9)
