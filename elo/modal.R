library(dplyr)
library(tidyr)

# ── ELO HELPERS ───────────────────────────────────────────────────────────────

elo_exp <- function(ea, eb) 1 / (1 + 10^((eb - ea) / 400))

draw_p <- function(ph) {
  z <- ph - 0.5
  0.3 * exp(-(z^2) / (2 * 0.3^2))
}

# ── SCORE DISTRIBUTION HELPERS ───────────────────────────────────────────────
score_rows <- function(pfav, oc_code, hist_sd) {
  rows <- hist_sd %>%
    filter(outcome == oc_code, p_lo <= pfav, pfav < p_hi)
  if (nrow(rows) == 0) {
    rows <- hist_sd %>%
      filter(outcome == oc_code) %>%
      filter(p_hi == max(p_hi))
  }
  rows
}

# ── PREDICT ONE MATCH ─────────────────────────────────────────────────────────
#
# For every (outcome, score) pair compute:
#   joint_prob = P(outcome) × P(score | outcome, pfav)
# and select the pair with the highest joint_prob.
predict_match <- function(elo_home, elo_away, hist_sd) {
  ph   <- elo_exp(elo_home, elo_away)   # raw P(home wins) before draw correction
  dp   <- draw_p(ph)
  p_hw <- ph       * (1 - dp)           # P(home win)
  p_d  <-            dp                 # P(draw)
  p_aw <- (1 - ph) * (1 - dp)           # P(away win)

  home_is_fav <- ph >= 0.5
  pfav        <- ifelse(home_is_fav, ph, 1 - ph)

  candidates <- bind_rows(
    score_rows(pfav, "fav_win", hist_sd) %>%
      mutate(p_outcome = ifelse(home_is_fav, p_hw, p_aw), oc = "fav_win"),
    score_rows(pfav, "draw",    hist_sd) %>%
      mutate(p_outcome = p_d,                              oc = "draw"),
    score_rows(pfav, "und_win", hist_sd) %>%
      mutate(p_outcome = ifelse(home_is_fav, p_aw, p_hw), oc = "und_win")
  ) %>%
    mutate(joint_prob = p_outcome * prob)

  best <- candidates %>% slice_max(joint_prob, n = 1, with_ties = FALSE)

  # Map fav/und goals → home/away goals
  if (best$oc == "draw") {
    home_goals <- best$fav_goals
    away_goals <- best$und_goals
    outcome    <- "draw"
  } else if (best$oc == "fav_win") {
    home_goals <- ifelse(home_is_fav, best$fav_goals, best$und_goals)
    away_goals <- ifelse(home_is_fav, best$und_goals, best$fav_goals)
    outcome    <- ifelse(home_is_fav, "home_win", "away_win")
  } else {  # und_win
    home_goals <- ifelse(home_is_fav, best$und_goals, best$fav_goals)
    away_goals <- ifelse(home_is_fav, best$fav_goals, best$und_goals)
    outcome    <- ifelse(home_is_fav, "away_win", "home_win")
  }

  data.frame(
    home_goals      = home_goals,
    away_goals      = away_goals,
    outcome         = outcome,
    best_joint_prob = round(best$joint_prob, 5),
    p_home_win      = round(p_hw, 4),
    p_draw          = round(p_d,  4),
    p_away_win      = round(p_aw, 4)
  )
}

# ── DATA LOADING ──────────────────────────────────────────────────────────────

load_data <- function() {
  fixtures_raw <- read.delim(
    "https://www.eloratings.net/2026_World_Cup_fixtures.tsv?_=1780309462239",
    sep = "\t", header = FALSE, stringsAsFactors = FALSE
  ) %>%
    filter(V6 == "WC") %>%
    select(
      year      = V1,
      month     = V2,
      day       = V3,
      home_code = V4,
      away_code = V5,
      home_elo  = V10,
      away_elo  = V11
    )

  ctry <- read.delim(
    "https://www.eloratings.net/en.teams.tsv?_=1772102421794",
    sep = "\t", header = FALSE, stringsAsFactors = FALSE
  ) %>%
    select(code = V1, name = V2)

  code_to_name <- setNames(ctry$name, ctry$code)

  fixtures <- fixtures_raw %>%
    mutate(
      home_name = coalesce(code_to_name[home_code], home_code),
      away_name = coalesce(code_to_name[away_code], away_code),
      date      = sprintf("%04d-%02d-%02d", year, month, day),
      phase = case_when(
        date <= "2026-06-27" ~ "Group Stage",
        TRUE                 ~ "Knockout"
      )
    )

  hist_sd <- read.csv("data/score_dist.csv", stringsAsFactors = FALSE)

  list(fixtures = fixtures, hist_sd = hist_sd)
}

# ── BUILD SCORE MATRIX ────────────────────────────────────────────────────────

build_score_matrix <- function(fixtures, hist_sd) {
  results <- lapply(seq_len(nrow(fixtures)), function(i) {
    fx   <- fixtures[i, ]
    pred <- predict_match(fx$home_elo, fx$away_elo, hist_sd)

    data.frame(
      date            = fx$date,
      phase           = fx$phase,
      home_code       = fx$home_code,
      home_name       = fx$home_name,
      home_elo        = fx$home_elo,
      away_code       = fx$away_code,
      away_name       = fx$away_name,
      away_elo        = fx$away_elo,
      home_goals      = pred$home_goals,
      away_goals      = pred$away_goals,
      score           = paste0(pred$home_goals, "\u2013", pred$away_goals),
      outcome         = pred$outcome,
      best_joint_prob = pred$best_joint_prob,
      p_home_win      = pred$p_home_win,
      p_draw          = pred$p_draw,
      p_away_win      = pred$p_away_win,
      stringsAsFactors = FALSE
    )
  }) %>% bind_rows()

  results
}

# ── PRINT SUMMARY ─────────────────────────────────────────────────────────────

print_summary <- function(scores) {
  cat("\n\u2550\u2550 WC 2026 \u2014 Most Likely Scores \u2550\u2550\n")

  for (ph in unique(scores$phase)) {
    cat(sprintf("\n\u2500\u2500 %s \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n", ph))
    grp <- scores %>% filter(phase == ph)
    for (i in seq_len(nrow(grp))) {
      r <- grp[i, ]
      cat(sprintf(
        "  %s  %-22s %s  %-22s  [H:%.0f%% D:%.0f%% A:%.0f%%]  joint:%.1f%%\n",
        r$date,
        sprintf("%s (%d)", r$home_name, r$home_elo),
        r$score,
        sprintf("%s (%d)", r$away_name, r$away_elo),
        r$p_home_win      * 100,
        r$p_draw          * 100,
        r$p_away_win      * 100,
        r$best_joint_prob * 100
      ))
    }
  }
}

# ── MAIN ──────────────────────────────────────────────────────────────────────

message("Loading fixtures and score distribution...")
dat <- load_data()

message(sprintf("Computing most likely scores for %d WC fixtures...", nrow(dat$fixtures)))
scores <- build_score_matrix(dat$fixtures, dat$hist_sd)

print_summary(scores)

write.csv(scores, "output/most_likely_scores.csv", row.names = FALSE)
message("Saved \u2192 output/most_likely_scores.csv")
