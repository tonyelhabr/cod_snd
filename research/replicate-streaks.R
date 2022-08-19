
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
runs <- rerun(
  1000,
  crossing(
    n = 1:50,
    k = 1:4,
    p = c(0.25, 0.5, 0.75)
  ) |> 
    mutate(
      next_p = pmap_dbl(list(n, k, p), ~simulate_post_streak_prob(sims = 1000, n = ..1, k = ..2, p = ..3))
    )
) |> 
  reduce(bind_rows)
qs::qsave(runs, 'data/runs_1000x1000.qs')

agg_runs <- runs |> 
  filter(!is.nan(next_p)) |> 
  group_by(p, k, n) |> 
  summarize(
    across(next_p, mean)
  ) |> 
  ungroup() |>
  arrange(p, k)

p_streaks <- agg_runs |> 
  arrange(p, k, n) |> 
  mutate(
    across(k, factor),
    group = sprintf('%s-%s', p, k)
  ) |> 
  ggplot() +
  theme_minimal() +
  aes(x = n, y = next_p, color = k, group = group) +
  geom_line() +
  # geom_smooth() +
  geom_hline(aes(yintercept = p)) +
  labs(x = NULL, y = NULL)
p_streaks
ggsave(p_streaks, filename = 'research/npk_replicated.png', width = 12, height = 9)

font <- 'Latin Modern Roman 10'
extrafont::loadfonts(quiet = TRUE)
theme_set(theme_classic())
theme_update(
  text = element_text(family = font)
)
  
p <- agg_runs |> 
  filter(p == 0.5, k >= 2, k <= 4, n <= 50) |> 
  arrange(p, k, n) |> 
  mutate(
    across(k, factor)
  ) |> 
  ggplot() +
  aes(x = n, y = next_p, color = k, group = k) +
  guides(
    color = guide_legend(
      title.hjust = 0.5,
      nrow = 1, override.aes = list(size = 3)
    )
  ) +
  geom_line() +
  geom_hline(aes(yintercept = p)) +
  labs(x = 'n', y = 'p') +
  theme(
    legend.position = c(0.5, 0.8)
  )
p

ggsave(
  p, 
  filename = 'paper/images/npkd.png', 
  width = 5.5, 
  height = 3
)
