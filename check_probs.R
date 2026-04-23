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
    p_bin = cut(p_h, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE),
    p_lo = as.numeric(sub("\\((.+),.*", "\\1", p_bin)),
    p_lo = tidyr::replace_na(p_lo, 0),
    p_hi = as.numeric(sub(".*,([^]]+)\\]", "\\1", p_bin)),
    goal_diff = home_goals - away_goals
  )

# --- Score distribution by probability bin ---
score_dist <- df %>%
  group_by(p_bin, p_lo, p_hi,outcome, home_goals, away_goals) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(p_bin, outcome) %>%
  mutate(prob = n / sum(n)) %>%
  arrange(p_bin, outcome, desc(prob))

write.csv(score_dist, "data/score_dist.csv", row.names=FALSE)

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

