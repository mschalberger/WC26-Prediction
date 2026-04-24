library(dplyr)
library(nnet)
library(ggplot2)

load("data/elo_all_matches.RData")

COMPETITIVE_CODES <- c("WC", "EC", "CA", "AC", "AR", "OC")

# --- Data prep ---
df <- elo_all_matches %>%
  mutate(
    elo_pre_home = elo_after_match_home - elo_change_home,
    elo_pre_away = elo_after_match_away - elo_change_away
  ) %>%
  filter(
    year >= 2000,
    tournament %in% COMPETITIVE_CODES
  ) %>%
  mutate(
    outcome = case_when(
      home_goals > away_goals ~ "home",
      home_goals < away_goals ~ "away",
      TRUE ~ "draw"
    ),
    outcome = factor(outcome, levels = c("home", "draw", "away")),
    p_h = 1 / (1 + 10^((elo_pre_away - elo_pre_home) / 400)),
    p_a = 1 - p_h
  ) %>%
  filter(!is.na(p_h), !is.na(home_goals), !is.na(away_goals))

df <- df %>%
  mutate(
    # Who is the favourite?
    home_is_fav = (p_h >= 0.5),

    # p_fav always in [0.5, 1.0]
    p_fav = ifelse(home_is_fav, p_h, 1 - p_h),

    # Goals re-oriented: fav_goals = goals of the stronger team
    fav_goals = ifelse(home_is_fav, home_goals, away_goals),
    und_goals = ifelse(home_is_fav, away_goals, home_goals),

    # Outcome from the favourite's point of view
    outcome = case_when(
      fav_goals > und_goals ~ "fav_win",
      fav_goals < und_goals ~ "und_win",
      TRUE                  ~ "draw"
    )
  )

breaks <- seq(0.5, 1.0, by = 0.05)

df <- df %>%
  mutate(
    p_bin = cut(p_fav,
                breaks       = breaks,
                include.lowest = TRUE,   # includes 0.5 in the first bin
                right          = TRUE)
  )


is_valid <- function(outcome, fav, und) {
  if (outcome == "fav_win") return(fav > und)
  if (outcome == "und_win") return(fav < und)
  if (outcome == "draw")    return(fav == und)
  FALSE
}

max_goals <- 5
sigma <- .5

# 1. Full grid
full_grid <- expand_grid(
  p_bin = unique(df$p_bin),
  outcome = unique(df$outcome),
  fav_goals = 0:max_goals,
  und_goals = 0:max_goals
)

# 2. Empirical probabilities
score_dist <- df %>%
  filter(!is.na(p_bin)) %>%
  count(p_bin, outcome, fav_goals, und_goals, name = "n") %>%
  group_by(p_bin, outcome) %>%
  mutate(prob = n / sum(n)) %>%
  ungroup()

score_full <- full_grid %>%
  left_join(score_dist, by = c("p_bin","outcome","fav_goals","und_goals")) %>%
  mutate(prob = replace_na(prob, 0)) %>%
  rowwise() %>%
  mutate(valid = is_valid(outcome, fav_goals, und_goals)) %>%
  ungroup()

smooth_scores <- function(df_bin) {

  scores <- as.matrix(df_bin[, c("fav_goals", "und_goals")])
  probs  <- df_bin$prob
  valid  <- df_bin$valid

  smoothed <- numeric(nrow(df_bin))

  for (i in seq_len(nrow(df_bin))) {

    # skip invalid targets completely
    if (!valid[i]) {
      smoothed[i] <- 0
      next
    }

    # distances only to valid neighbors
    d2 <- (scores[,1] - scores[i,1])^2 +
      (scores[,2] - scores[i,2])^2

    weights <- exp(-d2 / (2 * sigma^2)) * valid

    smoothed[i] <- sum(weights * probs)
  }

  # normalize only over valid cells
  smoothed / sum(smoothed)
}

score_smooth <- score_full %>%
  group_by(p_bin, outcome) %>%
  group_modify(~ {
    .x$prob_smooth <- smooth_scores(.x)
    .x
  }) %>%
  ungroup() %>%
  filter(valid) %>%   # drop invalid scorelines entirely
  select(p_bin, outcome, fav_goals, und_goals, prob = prob_smooth, prob_raw = prob) %>%
  mutate( p_lo  = as.numeric(sub("\\(?([0-9.]+),.*",    "\\1", p_bin)),
             p_hi  = as.numeric(sub(".*,([0-9.]+)\\]$",   "\\1", p_bin)),
             # The leftmost bin is "[0.5,0.55]" so p_lo parses correctly
             p_lo  = replace_na(p_lo, 0.5))

write.csv(score_smooth, "data/score_dist.csv", row.names=FALSE)

# --- Draw probability ---
sigma_hat <- sqrt(mean((df$p_h - 0.5)^2))
draw_p <- 1/3 * exp(-((df$p_h - 0.5)^2) / (2 * sigma_hat^2))

ggplot(df, aes(p_h, draw_p)) +
  geom_point() +
  labs(
    title = "Draw Probability vs Home Win Expectancy",
    x = "Home Win Expectancy (p_h)",
    y = "Draw Probability"
  ) +
  theme_minimal()

# --- Lambda estimation ---
goals_long <- bind_rows(
  df %>% transmute(goals = home_goals, p_self = p_h, p_opp = p_a, is_home = 1L),
  df %>% transmute(goals = away_goals, p_self = p_a, p_opp = p_h, is_home = 0L)
)

model <- lm(goals ~ p_self, data = goals_long)
summary(model)

ggplot(goals_long, aes(p_self, goals)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Goals vs Win Expectancy",
    x = "Win Expectancy (p_self)",
    y = "Goals"
  ) +
  theme_minimal()

lambda <- predict(model, newdata = data.frame(p_self = df$p_h))

