
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

runs <- crossing(
  n = 1:100,
  k = 1:5,
  p = c(0.25, 0.5, 0.75)
) |> 
  mutate(
    next_p = pmap_dbl(list(n, k, p), ~simulate_post_streak_prob(sims = 10000, n = ..1, k = ..2, p = ..3))
  )

p_streaks <- runs |>
  arrange(n, k) |>
  mutate(
    across(k, factor),
    group = sprintf('%s-%s', p, k)
  ) |> 
  ggplot() +
  theme_minimal() +
  aes(x = n, y = next_p, color = k, group = group) +
  geom_step() +
  # facet_wrap(~p, scales = 'free_y') +
  geom_hline(aes( yintercept = p))
p_streaks
ggsave(p_streaks, filename = 'research/streaks.png', width = 12, height = 6)
