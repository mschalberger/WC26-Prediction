library(dplyr)
library(tidyr)
library(nnet)
library(ggplot2)
library(rjson)
library(knitr)
library(kableExtra)

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

#whats the maximum draw probability if there is little to no elo difference
df %>%
  filter(p_h >= 0.49 & p_h <= 0.51) %>%
  #filter(outcome != "draw") %>%
  summarise(n = n(),
            mean_draw = mean(outcome == "draw"))

sigma_hat <- sqrt(mean((df$p_h - 0.5)^2))
draw_p <- 1/3 * exp(-((df$p_h - 0.5)^2) / (2 * sigma_hat^2))


theoretical_probs <- data.frame(
  elo_diff = seq(-600, 600, by = 10)
) %>%
  mutate(
    p_h = 1 / (1 + 10^(-elo_diff / 400)),
    draw_p = .3 * exp(-((p_h - 0.5)^2) / (2 * .3^2)),
    home_win_p = p_h * (1 - draw_p),
    away_win_p = (1 - p_h) * (1 - draw_p)
  ) %>%
  select(elo_diff, home_win_p, draw_p, away_win_p) %>%
  pivot_longer(
    cols = -elo_diff,
    names_to = "outcome",
    values_to = "probability"
  ) %>%
  mutate(
    outcome = factor(
      outcome,
      levels = c("home_win_p", "draw_p", "away_win_p"),
      labels = c("Heimsieg", "Unentschieden", "Auswärtssieg")
    )
  )

ggplot(theoretical_probs, aes(x = elo_diff, y = probability, color = outcome)) +
  geom_line(linewidth = 1.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(
    values = c("Heimsieg" = "#CCFF00",
               "Unentschieden" = "#00A4D1",
               "Auswärtssieg" =  "#E57050")
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Theoretische Wahrscheinlichkeiten nach Elo-Differenz",
    subtitle = "Positive Werte bedeuten stärkeres Heimteam",
    x = "Elo-Differenz (Heimteam − Auswärtsteam)",
    y = "Wahrscheinlichkeit",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("Figures/theoretical_probs.png",
       width  = 6, height = 4,
       dpi    = 300,
       bg     = "white")



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

# create html table for elo ranking
elo_all_matches %>%
  group_by(id_home, team_name_home) %>%
  summarise(n = n(),
            elo = max(elo_after_match_home)) %>%
  arrange(desc(elo)) %>%
  head(20) %>%
  knitr::kable(format = "html", col.names = c("Team ID", "Team Name", "Matches", "Elo")) %>%
  kableExtra::kable_styling(full_width = FALSE, position = "left") %>%
  kableExtra::row_spec(0, bold = TRUE)

load_data <- function() {
  teams   <- read.csv("data/teams.csv", stringsAsFactors = FALSE)
  elo_raw <- read.delim("https://www.eloratings.net/World.tsv?_=1776984277329",
                        sep = "\t", header = FALSE)
  ctry    <- read.delim("https://www.eloratings.net/en.teams.tsv?_=1772102421794",
                        sep = "\t", header = FALSE)
  fifa <- fromJSON(file ="https://api.fifa.com/api/v3/fifarankings/rankings/rankingsbyschedule?rankingScheduleId=FRS_Male_Football_20260119&language=en")
  fifa_rank <- map_dfr(fifa$Results, function(x) {
    tibble(
      IdTeam = x$IdCountry,
      TeamName = x$TeamName[[1]]$Description,
      Rank = x$Rank
    )
  })


  elo <- elo_raw %>%
    left_join(ctry, by = c("V3" = "V1")) %>%
    select(country = V2.y, elo = V4) %>%
    mutate(country = case_when(
      country == "United States"                ~ "USA",
      country == "Iran"                         ~ "IR Iran",
      country == "Cape Verde"                   ~ "Cabo Verde",
      country == "Ivory Coast"                  ~ "Côte d'Ivoire",
      country == "Democratic Republic of Congo" ~ "DR Congo",
      TRUE ~ country
    ))

  teams_init <- teams %>%
    mutate(team_name = case_when(
      team_name == "Winner FIFA Playoff 1" ~ "DR Congo",
      team_name == "Winner FIFA Playoff 2" ~ "Iraq",
      team_name == "Winner UEFA Playoff B" ~ "Sweden",
      team_name == "Winner UEFA Playoff C" ~ "Turkey",
      team_name == "Winner UEFA Playoff A" ~ "Bosnia and Herzegovina",
      team_name == "Winner UEFA Playoff D" ~ "Czechia",
      TRUE ~ team_name
    )) %>%
    left_join(elo, by = c("team_name" = "country")) %>%
    mutate(fifa_code = case_when(
      fifa_code == "CUR" ~ "CUW",
      TRUE ~ fifa_code
    ))

  dc <- readRDS("dc/dc_model.rds")
  team_de <- c(
    "Mexico"                 = "Mexiko",
    "South Africa"           = "Südafrika",
    "South Korea"            = "Südkorea",
    "Czechia"                = "Tschechien",
    "Canada"                 = "Kanada",
    "Bosnia and Herzegovina" = "Bosnien und Herzegowina",
    "Qatar"                  = "Katar",
    "Switzerland"            = "Schweiz",
    "Brazil"                 = "Brasilien",
    "Morocco"                = "Marokko",
    "Haiti"                  = "Haiti",
    "Scotland"               = "Schottland",
    "USA"                    = "USA",
    "Paraguay"               = "Paraguay",
    "Australia"              = "Australien",
    "Turkey"                 = "Türkei",
    "Germany"                = "Deutschland",
    "Curaçao"                = "Curaçao",
    "Côte d'Ivoire"          = "Côte d'Ivoire",
    "Ecuador"                = "Ecuador",
    "Netherlands"            = "Niederlande",
    "Japan"                  = "Japan",
    "Sweden"                 = "Schweden",
    "Tunisia"                = "Tunesien",
    "Belgium"                = "Belgien",
    "Egypt"                  = "Ägypten",
    "IR Iran"                = "Iran",
    "New Zealand"            = "Neuseeland",
    "Spain"                  = "Spanien",
    "Cabo Verde"             = "Kap Verde",
    "Saudi Arabia"           = "Saudi-Arabien",
    "Uruguay"                = "Uruguay",
    "France"                 = "Frankreich",
    "Senegal"                = "Senegal",
    "Iraq"                   = "Irak",
    "Norway"                 = "Norwegen",
    "Argentina"              = "Argentinien",
    "Algeria"                = "Algerien",
    "Austria"                = "Österreich",
    "Jordan"                 = "Jordanien",
    "Portugal"               = "Portugal",
    "DR Congo"               = "DR Kongo",
    "Uzbekistan"             = "Usbekistan",
    "Colombia"               = "Kolumbien",
    "England"                = "England",
    "Croatia"                = "Kroatien",
    "Ghana"                  = "Ghana",
    "Panama"                 = "Panama"
  )

  cbind(dc$parameters$attack, dc$parameters$defense) %>%
     as.data.frame() %>%
     setNames(c("attack", "defense")) %>%
    tibble::rownames_to_column("team_id") %>%
    filter(team_id %in% 1:48) %>%
    mutate(team_id = as.integer(team_id)) %>%
    left_join(teams_init, by = c("team_id" = "id")) %>%
    left_join(fifa_rank, by = c("fifa_code" = "IdTeam")) %>%
    mutate(team_name = recode(team_name, !!!team_de))
}

combined_data <- load_data()

options(encoding = "UTF-8")
Sys.setlocale("LC_ALL", "en_US.UTF-8")

html_tab <- combined_data %>%
  select(team_name, Rank, elo, attack, defense) %>%
  mutate(attack = round(attack, 3),
         defense = round(defense, 3)) %>%
  arrange(Rank) %>%
  knitr::kable(format = "html", col.names = c("Team Name", "FIFA Rang","Elo", "Angriff", "Defensive")) %>%
  kableExtra::kable_styling(full_width = FALSE, position = "left") %>%
  kableExtra::row_spec(0, bold = TRUE)

writeLines(html_tab, "output/team_ranking_table.html")
