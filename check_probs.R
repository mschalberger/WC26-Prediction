library(dplyr)
library(nnet)
library(ggplot2)

COMPETITIVE_CODES <- c("WC", "WCQ")

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


