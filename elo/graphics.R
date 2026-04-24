# ============================================================
# FIFA World Cup 2026 — Monte Carlo Simulation & Plots
# Run: Rscript wc2026_simulation.R
#
# install.packages(c("ggplot2","dplyr","tidyr","patchwork","scales"))
#
# Expects the same data/ folder structure as the Shiny app:
#   data/teams.csv
#   data/cache/elo.tsv, data/cache/countries.tsv
#   data/score_dist.csv
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales)

# ── CONFIG ───────────────────────────────────────────────────

N_SIMS       <- 2000
SEED         <- 42
K            <- 20
MAX_WIN_PROB <- 0.95
USE_HIST     <- TRUE
GERMANY_NAME <- "Germany"
TOP_N        <- 10

# ── LIGHT MODE PALETTE (mirrors Shiny app light-mode CSS vars) ──
#
#  --body / --panel     →  #FFFFFF
#  --border             →  #E0E0E0
#  --text               →  #000000
#  --muted              →  #888888
#  --fublue             →  #004659   (dark navy accent)
#  group-header-bg      →  #CCFF00   (lime — strip / header fill)
#  ko-round-title color →  #000000   (in light mode)
#  run-btn bg           →  #CCFF00
#  --green (light mode) →  #007a30
#  --red                →  #C8102E

BG        <- "#FFFFFF"
PANEL_BG  <- "#FFFFFF"
BORDER    <- "#E0E0E0"
TEXT      <- "#000000"
MUTED     <- "#888888"
LIME      <- "#CCFF00"
NAVY      <- "#004659"   # --fublue
GREEN_LT  <- "#007a30"
RED_LT    <- "#C8102E"
DRAW_COL  <- "#8B6914"   # warm dark ochre — readable on pale yellow

# Score matrix cell fills (pastel, coloured per outcome region)
FILL_WIN  <- "#d6f5e3"   # pale green
FILL_DRAW <- "#fdf6cc"   # pale lime/yellow
FILL_LOSS <- "#fde8e8"   # pale red

FONT <- "sans"           # swap for "Source Sans 3" if installed

# ── THEME — mirrors the Shiny app's light-mode look ──────────
#
#  - White background, black text
#  - Lime (#CCFF00) strip headers (like .group-header)
#  - E0E0E0 grid / borders (like --border)
#  - Bold uppercase strip text (like .group-header)
#  - ko-round-title left accent replicated as plot title style

theme_wc <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      text              = element_text(family = FONT, colour = TEXT),
      plot.background   = element_rect(fill = BG,       colour = NA),
      panel.background  = element_rect(fill = PANEL_BG, colour = NA),
      panel.border      = element_rect(fill = NA, colour = BORDER, linewidth = 0.8),
      panel.grid.major  = element_line(colour = BORDER,  linewidth = 0.35),
      panel.grid.minor  = element_blank(),
      axis.text         = element_text(colour = MUTED,  size = rel(0.85)),
      axis.title        = element_text(colour = TEXT,   size = rel(0.95), face = "bold"),
      plot.title        = element_text(colour = TEXT,   size = rel(1.22), face = "bold",
                                       margin = margin(b = 5)),
      plot.subtitle     = element_text(colour = MUTED,  size = rel(0.82),
                                       margin = margin(b = 10)),
      plot.caption      = element_text(colour = MUTED,  size = rel(0.70),
                                       hjust = 0, margin = margin(t = 8)),
      legend.background = element_rect(fill = PANEL_BG, colour = NA),
      legend.key        = element_rect(fill = PANEL_BG, colour = NA),
      legend.text       = element_text(colour = TEXT),
      legend.title      = element_text(colour = MUTED, face = "bold"),
      # Lime header with black text — mirrors .group-header in light mode
      strip.background  = element_rect(fill = LIME, colour = NA),
      strip.text        = element_text(colour = TEXT, face = "bold", size = rel(0.88)),
      plot.margin       = margin(16, 16, 12, 16)
    )
}

# ── DATA LOADING ─────────────────────────────────────────────

load_data <- function() {
  teams   <- read.csv("data/teams.csv", stringsAsFactors = FALSE)
  elo_raw       <- read.delim("https://www.eloratings.net/World.tsv?_=1776984277329",    sep = "\t", header = FALSE)
  ctry <- read.delim("https://www.eloratings.net/en.teams.tsv?_=1772102421794", sep = "\t", header = FALSE)


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
    left_join(elo, by = c("team_name" = "country"))

  missing <- teams_init %>% filter(is.na(elo)) %>% pull(team_name)
  if (length(missing) > 0) {
    message("⚠️  No ELO for: ", paste(missing, collapse = ", "), " — using median")
    teams_init <- teams_init %>%
      mutate(elo = ifelse(is.na(elo), median(elo, na.rm = TRUE), elo))
  }

  hist_sd <- read.csv("data/score_dist.csv", stringsAsFactors = FALSE)
  list(teams_init = teams_init, hist_sd = hist_sd)
}

# ── FLAGS ────────────────────────────────────────────────────

flag_map <- c(
  MEX="🇲🇽", RSA="🇿🇦", KOR="🇰🇷", UEPD="🇨🇿", CAN="🇨🇦", UEPA="🇧🇦", QAT="🇶🇦",
  SUI="🇨🇭", BRA="🇧🇷", MAR="🇲🇦", HAI="🇭🇹", SCO="🏴󠁧󠁢󠁳󠁣󠁴󠁿", USA="🇺🇸", PAR="🇵🇾",
  AUS="🇦🇺", UEPC="🇹🇷", GER="🇩🇪", CUR="🇨🇼", CIV="🇨🇮", ECU="🇪🇨", NED="🇳🇱",
  JPN="🇯🇵", UEPB="🇸🇪", TUN="🇹🇳", BEL="🇧🇪", EGY="🇪🇬", IRN="🇮🇷", NZL="🇳🇿",
  ESP="🇪🇸", CPV="🇨🇻", KSA="🇸🇦", URU="🇺🇾", FRA="🇫🇷", SEN="🇸🇳", FP02="🇮🇶",
  NOR="🇳🇴", ARG="🇦🇷", ALG="🇩🇿", AUT="🇦🇹", JOR="🇯🇴", POR="🇵🇹", FP01="🇨🇩",
  UZB="🇺🇿", COL="🇨🇴", ENG="🏴󠁧󠁢󠁥󠁮󠁧󠁿", CRO="🇭🇷", GHA="🇬🇭", PAN="🇵🇦"
)
get_flag <- function(code) ifelse(is.na(flag_map[code]), "🏳️", unname(flag_map[code]))

# ── ELO ENGINE ───────────────────────────────────────────────

elo_expected <- function(ea, eb) 1 / (1 + 10^((eb - ea) / 400))

elo_wdl <- function(elo_h, elo_a, max_win_prob = MAX_WIN_PROB) {
  p_h_raw <- elo_expected(elo_h, elo_a)
  p_h <- if (p_h_raw >= 0.5)
    0.5 + min(p_h_raw - 0.5, max_win_prob - 0.5)
  else
    0.5 - min(0.5 - p_h_raw, max_win_prob - 0.5)
  draw_p <- 1/3 * exp(-((p_h - .5)^2) / (2 * 0.236875^2))
  list(win = p_h * (1 - draw_p), draw = draw_p, loss = (1 - p_h) * (1 - draw_p))
}

# ── ANALYTICAL SCORE DISTRIBUTION ────────────────────────────
#
# Requires score_dist.csv built with build_score_dist.R, which
# orients every match from the FAVOURITE's perspective:
#   outcome   : "fav_win" | "draw" | "und_win"
#   p_lo/p_hi : bin edges for p_fav  (always in [0.5, 1.0])
#   fav_goals : goals of the stronger team
#   und_goals : goals of the weaker team
#
# Because the CSV is already in favourite/underdog space, no
# symmetrization is needed. We simply identify which team is
# Germany (fav or und), look up the bin, and read goals directly.

analytical_score_dist <- function(elo_ger, elo_opp, hist_sd) {

  # p_fav: always >= 0.5; identify whether Germany is the favourite
  p_raw     <- elo_expected(elo_ger, elo_opp)   # P(Germany wins)
  ger_is_fav <- (p_raw >= 0.5)
  p_fav     <- if (ger_is_fav) p_raw else (1 - p_raw)

  # Apply max-win-prob cap
  p_fav_cap <- 0.5 + min(p_fav - 0.5, MAX_WIN_PROB - 0.5)

  # ELO-derived W/D/L probabilities (from the favourite's point of view)
  draw_p  <- 1/3 * exp(-((p_fav_cap - 0.5)^2) / (2 * 0.236875^2))
  p_fav_w <- p_fav_cap * (1 - draw_p)   # P(favourite wins)
  p_und_w <- (1 - p_fav_cap) * (1 - draw_p)   # P(underdog wins)

  outcome_weights <- list(
    list(csv = "fav_win", weight = p_fav_w),
    list(csv = "draw",    weight = draw_p),
    list(csv = "und_win", weight = p_und_w)
  )

  rows <- lapply(outcome_weights, function(oc) {
    if (oc$weight == 0) return(NULL)

    bin <- hist_sd %>%
      filter(outcome == oc$csv, p_lo <= p_fav_cap, p_fav_cap < p_hi)
    if (nrow(bin) == 0)
      bin <- hist_sd %>%
      filter(outcome == oc$csv,
             p_hi == max(p_hi[outcome == oc$csv]))
    if (nrow(bin) == 0) return(NULL)

    # Map fav/und goals → ger/opp goals
    bin %>%
      mutate(
        ger_goals  = if (ger_is_fav) fav_goals else und_goals,
        opp_goals  = if (ger_is_fav) und_goals else fav_goals,
        joint_prob = prob * oc$weight
      ) %>%
      select(ger_goals, opp_goals, joint_prob)
  })

  bind_rows(rows) %>%
    group_by(ger_goals, opp_goals) %>%
    summarise(prob = sum(joint_prob), .groups = "drop") %>%
    mutate(prob = prob / sum(prob))
}

# ── SIMULATE MATCH (used only for MC) ────────────────────────

sample_hist_score <- function(p_fav, sim_outcome, hist_sd) {
  csv_out <- switch(sim_outcome, fav_win = "home", und_win = "away", "draw")
  bin <- hist_sd %>% filter(outcome == csv_out, p_lo <= p_fav, p_fav < p_hi)
  if (nrow(bin) == 0)
    bin <- hist_sd %>% filter(outcome == csv_out,
                              p_hi == max(p_hi[outcome == csv_out]))
  if (nrow(bin) == 0) return(NULL)
  idx <- sample(nrow(bin), 1, prob = bin$prob)
  list(fav_goals = bin$home_goals[idx], und_goals = bin$away_goals[idx])
}

simulate_match <- function(elo_h, elo_a, k = 20, max_win_prob = 0.95,
                           use_hist = FALSE, hist_sd = NULL) {
  p_h_raw <- elo_expected(elo_h, elo_a)
  p_h <- if (p_h_raw >= 0.5)
    0.5 + min(p_h_raw - 0.5, max_win_prob - 0.5)
  else
    0.5 - min(0.5 - p_h_raw, max_win_prob - 0.5)
  draw_p  <- 1/3 * exp(-((p_h - .5)^2) / (2 * 0.236875^2))
  ph <- p_h * (1 - draw_p); pa <- (1 - p_h) * (1 - draw_p)
  outcome <- sample(c("home","draw","away"), 1, prob = c(ph, draw_p, pa))

  home_is_fav <- (p_h >= 0.5)
  p_fav       <- if (home_is_fav) p_h else (1 - p_h)
  sim_out     <- if (outcome == "draw") "draw"
  else if ((outcome == "home") == home_is_fav) "fav_win" else "und_win"

  gh <- ga <- NA_integer_
  if (use_hist && !is.null(hist_sd)) {
    sc <- sample_hist_score(p_fav, sim_out, hist_sd)
    if (!is.null(sc)) {
      gh <- if (home_is_fav) sc$fav_goals else sc$und_goals
      ga <- if (home_is_fav) sc$und_goals else sc$fav_goals
    }
  }
  if (is.na(gh)) {
    lh <- 1.99419 * ph + 0.24629; la <- 1.99419 * pa + 0.24629
    repeat {
      gh <- rpois(1, lh); ga <- rpois(1, la)
      if (outcome == "home" && gh > ga) break
      if (outcome == "away" && ga > gh) break
      if (outcome == "draw" && gh == ga) break
    }
  }
  act_h <- ifelse(outcome == "home", 1, ifelse(outcome == "draw", 0.5, 0))
  exp_h <- elo_expected(elo_h, elo_a)
  gd    <- abs(gh - ga)
  k_adj <- k * (if (gd <= 1) 1 else if (gd == 2) 1.5
                else if (gd == 3) 1.75 else 1.75 + (gd - 3) / 8)
  list(home_goals   = gh, away_goals   = ga, outcome = outcome,
       new_elo_home = elo_h + k_adj * (act_h - exp_h),
       new_elo_away = elo_a + k_adj * ((1 - act_h) - (1 - exp_h)))
}

# ── GROUP STAGE ──────────────────────────────────────────────

run_group_stage <- function(teams_df, k, mwp, use_hist, hist_sd) {
  elo_live <- setNames(teams_df$elo, teams_df$id)
  all_st   <- data.frame()
  for (grp in sort(unique(teams_df$group_letter))) {
    ids  <- teams_df %>% filter(group_letter == grp) %>% pull(id)
    pairs <- combn(ids, 2, simplify = FALSE)
    pts <- gf <- ga <- setNames(rep(0L, 4), ids)
    for (pair in pairs) {
      h <- pair[1]; a <- pair[2]
      res <- simulate_match(elo_live[as.character(h)], elo_live[as.character(a)],
                            k = k, max_win_prob = mwp,
                            use_hist = use_hist, hist_sd = hist_sd)
      elo_live[as.character(h)] <- res$new_elo_home
      elo_live[as.character(a)] <- res$new_elo_away
      gf[as.character(h)] <- gf[as.character(h)] + res$home_goals
      ga[as.character(h)] <- ga[as.character(h)] + res$away_goals
      gf[as.character(a)] <- gf[as.character(a)] + res$away_goals
      ga[as.character(a)] <- ga[as.character(a)] + res$home_goals
      if (res$outcome == "home")       pts[as.character(h)] <- pts[as.character(h)] + 3L
      else if (res$outcome == "away")  pts[as.character(a)] <- pts[as.character(a)] + 3L
      else {
        pts[as.character(h)] <- pts[as.character(h)] + 1L
        pts[as.character(a)] <- pts[as.character(a)] + 1L
      }
    }
    st <- data.frame(id = ids, pts = as.numeric(pts),
                     gf = as.numeric(gf), ga = as.numeric(ga)) %>%
      mutate(gd = gf - ga, elo = elo_live[as.character(ids)]) %>%
      arrange(desc(pts), desc(gd), desc(gf), desc(elo)) %>%
      mutate(rank = 1:4, group = grp) %>%
      left_join(teams_df %>% select(id, team_name, fifa_code), by = "id")
    all_st <- rbind(all_st, st)
  }
  list(standings = all_st, elo_live = elo_live)
}

# ── KNOCKOUT ─────────────────────────────────────────────────

run_knockout <- function(pairs, elo_live, teams_df, k, mwp, use_hist, hist_sd) {
  winners <- c(); losers <- c()
  for (pair in pairs) {
    res <- simulate_match(elo_live[as.character(pair[1])],
                          elo_live[as.character(pair[2])],
                          k = k, max_win_prob = mwp,
                          use_hist = use_hist, hist_sd = hist_sd)
    elo_live[as.character(pair[1])] <- res$new_elo_home
    elo_live[as.character(pair[2])] <- res$new_elo_away
    winner <- if (res$outcome == "draw") {
      pa <- elo_expected(elo_live[as.character(pair[1])],
                         elo_live[as.character(pair[2])])
      ifelse(runif(1) < pa, pair[1], pair[2])
    } else if (res$outcome == "home") pair[1] else pair[2]
    winners <- c(winners, winner)
    losers  <- c(losers,  ifelse(winner == pair[1], pair[2], pair[1]))
  }
  list(winners = winners, losers = losers, elo_live = elo_live)
}

# ── SINGLE TOURNAMENT ────────────────────────────────────────

run_tournament <- function(teams_init, hist_sd, seed = NULL,
                           k = 20, mwp = 0.95, use_hist = FALSE) {
  if (!is.null(seed)) set.seed(seed)
  gs  <- run_group_stage(teams_init, k, mwp, use_hist, hist_sd)
  std <- gs$standings
  el  <- gs$elo_live

  thirds <- std %>% filter(rank == 3) %>%
    arrange(desc(pts), desc(gd), desc(gf)) %>% slice(1:8)
  gt <- function(g, r) std %>% filter(group == g, rank == r) %>% pull(id)

  r32p <- list(
    c(gt("A",2),gt("B",2)), c(gt("C",1),gt("F",2)),
    c(gt("E",1),thirds$id[1]), c(gt("F",1),gt("C",2)),
    c(gt("E",2),gt("I",2)), c(gt("I",1),thirds$id[2]),
    c(gt("A",1),thirds$id[3]), c(gt("L",1),thirds$id[4]),
    c(gt("G",1),thirds$id[5]), c(gt("D",1),thirds$id[6]),
    c(gt("H",1),gt("J",2)), c(gt("K",2),gt("L",2)),
    c(gt("B",1),thirds$id[7]), c(gt("D",2),gt("G",2)),
    c(gt("J",1),gt("H",2)), c(gt("K",1),thirds$id[8])
  )
  ko <- list(elo_live=el, teams_df=teams_init, k=k, mwp=mwp,
             use_hist=use_hist, hist_sd=hist_sd)

  r32 <- do.call(run_knockout, c(list(pairs=r32p), ko))
  ko$elo_live <- r32$elo_live
  r16 <- do.call(run_knockout, c(list(pairs=lapply(seq(1,15,2),
                                                   function(i) c(r32$winners[i],r32$winners[i+1]))), ko))
  ko$elo_live <- r16$elo_live
  qf  <- do.call(run_knockout, c(list(pairs=lapply(seq(1,7,2),
                                                   function(i) c(r16$winners[i],r16$winners[i+1]))), ko))
  ko$elo_live <- qf$elo_live
  sf  <- do.call(run_knockout, c(list(pairs=list(
    c(qf$winners[1],qf$winners[2]), c(qf$winners[3],qf$winners[4]))), ko))
  ko$elo_live <- sf$elo_live
  tp  <- do.call(run_knockout, c(list(pairs=list(sf$losers)), ko))
  ko$elo_live <- tp$elo_live
  fin <- do.call(run_knockout, c(list(pairs=list(
    c(sf$winners[1],sf$winners[2]))), ko))

  thirds_out <- setdiff(std %>% filter(rank == 3) %>% pull(id), thirds$id)

  list(group_info  = std,
       group_out   = c(std %>% filter(rank == 4) %>% pull(id), thirds_out),
       r32_losers  = r32$losers, r16_losers = r16$losers,
       qf_losers   = qf$losers,  sf_losers  = sf$losers,
       finalist    = fin$losers[1], champion = fin$winners[1])
}

# ── MONTE CARLO ──────────────────────────────────────────────

run_mc <- function(n_sims, teams_init, hist_sd,
                   k = 20, mwp = 0.95, use_hist = TRUE, seed_base = 42) {
  message(sprintf("Running %d simulations...", n_sims))
  all_ids <- teams_init$id
  stages  <- c("Group Stage","Round of 32","Round of 16",
               "Quarter-Final","Semi-Final","Final","Champion")
  elim    <- matrix(0L, nrow=length(all_ids), ncol=length(stages),
                    dimnames=list(as.character(all_ids), stages))
  grp_ltrs  <- sort(unique(teams_init$group_letter))
  gwin      <- matrix(0L, nrow=length(all_ids), ncol=length(grp_ltrs),
                      dimnames=list(as.character(all_ids), grp_ltrs))

  set.seed(seed_base); seeds <- sample.int(1e6, n_sims)
  for (sim in seq_len(n_sims)) {
    if (sim %% 500 == 0) message(sprintf("  sim %d / %d", sim, n_sims))
    r <- run_tournament(teams_init, hist_sd, seed=seeds[sim],
                        k=k, mwp=mwp, use_hist=use_hist)
    for (id in as.character(r$group_out))
      elim[id,"Group Stage"]   <- elim[id,"Group Stage"]   + 1L
    for (id in as.character(r$r32_losers))
      elim[id,"Round of 32"]   <- elim[id,"Round of 32"]   + 1L
    for (id in as.character(r$r16_losers))
      elim[id,"Round of 16"]   <- elim[id,"Round of 16"]   + 1L
    for (id in as.character(r$qf_losers))
      elim[id,"Quarter-Final"] <- elim[id,"Quarter-Final"] + 1L
    for (id in as.character(r$sf_losers))
      elim[id,"Semi-Final"]    <- elim[id,"Semi-Final"]    + 1L
    elim[as.character(r$finalist),"Final"]    <-
      elim[as.character(r$finalist),"Final"]    + 1L
    elim[as.character(r$champion),"Champion"] <-
      elim[as.character(r$champion),"Champion"] + 1L

    gi <- r$group_info %>% filter(rank == 1)
    for (j in seq_len(nrow(gi))) {
      gid <- as.character(gi$id[j]); gl <- gi$group[j]
      if (gid %in% rownames(gwin) && gl %in% colnames(gwin))
        gwin[gid, gl] <- gwin[gid, gl] + 1L
    }
  }

  # P(reach >= stage)
  rp <- matrix(0, nrow=length(all_ids), ncol=length(stages),
               dimnames=list(as.character(all_ids), stages))
  rp[,"Group Stage"]   <- 1
  rp[,"Round of 32"]   <- 1 - elim[,"Group Stage"]   / n_sims
  rp[,"Round of 16"]   <- rp[,"Round of 32"]   - elim[,"Round of 32"]   / n_sims
  rp[,"Quarter-Final"] <- rp[,"Round of 16"]   - elim[,"Round of 16"]   / n_sims
  rp[,"Semi-Final"]    <- rp[,"Quarter-Final"] - elim[,"Quarter-Final"] / n_sims
  rp[,"Final"]         <- rp[,"Semi-Final"]    - elim[,"Semi-Final"]    / n_sims
  rp[,"Champion"]      <- elim[,"Champion"] / n_sims

  reach_df <- as.data.frame(rp) %>%
    mutate(id = as.integer(rownames(rp))) %>%
    left_join(teams_init %>% select(id, team_name, fifa_code, group_letter), by="id")

  gwin_df <- as.data.frame(gwin / n_sims) %>%
    mutate(id = as.integer(rownames(gwin))) %>%
    left_join(teams_init %>% select(id, team_name, fifa_code, group_letter), by="id") %>%
    pivot_longer(cols=all_of(grp_ltrs), names_to="group_col", values_to="win_prob") %>%
    filter(group_col == group_letter) %>%
    select(id, team_name, fifa_code, group=group_letter, win_prob)

  list(reach_df=reach_df, gwin_df=gwin_df, n_sims=n_sims)
}

# ══════════════════════════════════════════════════════════════
# PLOT 1 — Germany Group Score Matrices  (ANALYTICAL)
#
# Each cell's fill = outcome region (WIN / DRAW / LOSS).
# Each cell's text = joint probability of that exact scoreline.
# No simulation — derived analytically from score_dist.csv
# weighted by ELO win/draw/loss probabilities.
# ══════════════════════════════════════════════════════════════

plot_germany_matrix <- function(teams_init, hist_sd,
                                germany_name = "Germany", max_g = 5) {

  ger   <- teams_init %>% filter(team_name == germany_name)
  ger_e <- ger$elo[1]
  ger_g <- ger$group_letter[1]

  opps  <- teams_init %>%
    filter(group_letter == ger_g, team_name != germany_name)

  plots <- lapply(seq_len(nrow(opps)), function(i) {

    opp   <- opps[i, ]
    opp_e <- opp$elo
    opp_f <- get_flag(opp$fifa_code)

    dist <- analytical_score_dist(ger_e, opp_e, hist_sd) %>%
      filter(ger_goals <= max_g, opp_goals <= max_g)

    grid <- expand.grid(ger_goals = 0:max_g, opp_goals = 0:max_g) %>%
      left_join(dist, by = c("ger_goals","opp_goals")) %>%
      mutate(
        prob   = ifelse(is.na(prob), 0, prob),
        result = case_when(
          ger_goals > opp_goals ~ "WIN",
          ger_goals < opp_goals ~ "LOSS",
          TRUE                  ~ "DRAW"
        ),
        label = ifelse(prob >= 0.005,
                       sprintf("%.1f%%", prob * 100),
                       "<.05%")
      )

    # ---- ELO comparison ----
    wdl <- elo_wdl(ger_e, opp_e)

    # ---- heatmap ----
    p_heat <- ggplot(grid, aes(x = ger_goals, y = opp_goals)) +
      geom_tile(aes(fill = prob), colour = BORDER, linewidth = 0.9) +
      geom_text(aes(label = label),
                colour = TEXT,
                size = 2.9,
                fontface = "bold",
                family = FONT) +
      scale_fill_gradient(low = FILL_DRAW, high = LIME) +
      scale_x_continuous(breaks = 0:max_g, expand = expansion(add = 0.52)) +
      scale_y_continuous(breaks = 0:max_g, expand = expansion(add = 0.52)) +
      labs(
        title    = sprintf("Germany  vs  %s", opp$team_name),
        x        = "Germany Goals",
        y        = sprintf("%s Goals", opp$team_name)
      ) +
      #theme_wc(10) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background  = element_blank(),
        plot.background   = element_blank(),
        legend.position   = "none"
      )

    # ---- bar plot (W/D/L) ----
    summary_df <- data.frame(
      WIN  = wdl$win,
      DRAW = wdl$draw,
      LOSS = wdl$loss
    )

    summary_df <- tidyr::pivot_longer(
      summary_df,
      cols = everything(),
      names_to = "result",
      values_to = "prob"
    )

    summary_df$result <- factor(summary_df$result,
                                levels = c("WIN", "DRAW", "LOSS"))
    p_bar <- ggplot(summary_df, aes(x = 1, y = prob, fill = result)) +
      geom_col(width = 0.6) +
      geom_text(
        aes(label = paste0(round(prob * 100), "%")),
        position = position_stack(vjust = 0.5),
        size = 3,
        colour = "black"
      ) +
      coord_flip() +
      scale_fill_manual(
        values = c("WIN" = LIME, "DRAW" = FILL_DRAW, "LOSS" = RED_LT),
        name = NULL
      ) +
      #theme_wc(10) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background  = element_blank(),
        plot.background   = element_blank(),
        axis.text         = element_blank(),
        axis.ticks        = element_blank(),
        axis.title        = element_blank(),
        legend.position   = "none"
      )

    # ---- combine ----
    p_heat / p_bar + plot_layout(heights = c(4, 1))
  })

  wrap_plots(plots, nrow = 1) +
    plot_annotation(
      title = "Germany — Group Stage Score Probability Matrix",
      theme = theme(
        #plot.background = element_rect(fill = BG, colour = NA),
        plot.title = element_text(colour = TEXT, size = 15, face = "bold",
                                  family = FONT,
                                  margin = margin(b = 4)),
        plot.subtitle = element_text(colour = MUTED, size = 9, family = FONT,
                                     margin = margin(b = 6)),
        plot.caption = element_text(colour = MUTED, size = 8, family = FONT,
                                    hjust = 0, margin = margin(t = 6)),
        plot.margin = margin(20, 20, 14, 20)
      )
    )
}

# ══════════════════════════════════════════════════════════════
# PLOT 2a — Championship Probability  (top N, horizontal bar)
# Style: lime bars, bold team names — mirrors app's bar aesthetic
# ══════════════════════════════════════════════════════════════

plot_champion_prob <- function(mc, top_n = 10) {
  df <- mc$reach_df %>%
    arrange(desc(Champion)) %>%
    slice(1:top_n) %>%
    mutate(
      pct       = Champion * 100,
      team_name = factor(team_name, levels = rev(team_name))
    )

  ggplot(df, aes(x = pct, y = team_name)) +
    # Main lime bar
    geom_col(fill = LIME, colour = NA, width = 0.65) +
    # Navy left-edge accent (mirrors ko-round-title left border)
    geom_col(aes(x = pmax(pct * 0.018, 0.05)), fill = NAVY,
             colour = NA, width = 0.65) +
    geom_text(aes(label = sprintf("%.1f%%", pct)),
              hjust = -0.12, colour = TEXT,
              size = 3.7, fontface = "bold", family = FONT) +
    scale_x_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.22))) +
    labs(
      title    = "World Cup Champion Probability",
      subtitle = sprintf("Top %d nations  ·  %s simulations",
                         top_n, format(mc$n_sims, big.mark = ",")),
      x = NULL, y = NULL
    ) +
    theme_wc(11) +
    theme(
      axis.text.y        = element_text(size = 10.5, colour = TEXT, face = "bold"),
      axis.text.x        = element_text(size = 9,    colour = MUTED),
      panel.grid.major.y = element_blank()
    )
}

# ══════════════════════════════════════════════════════════════
# PLOT 2b — Germany Stage Progression Probability
# ══════════════════════════════════════════════════════════════

plot_germany_stages <- function(mc, germany_name = "Germany") {
  stage_keys <- c("Round of 32","Round of 16","Quarter-Final",
                  "Semi-Final","Final","Champion")
  stage_lbls <- c("Round\nof 32","Round\nof 16","Quarter-\nFinal",
                  "Semi-\nFinal","Final","Champion")

  ger <- mc$reach_df %>% filter(team_name == germany_name)
  if (nrow(ger) == 0) stop("Germany not found in results.")

  df <- data.frame(
    stage = factor(stage_keys, levels = stage_keys),
    label = stage_lbls,
    pct   = as.numeric(ger[1, stage_keys]) * 100
  ) %>%
    mutate(lbl = sprintf("%.1f%%", pct))

  ggplot(df, aes(x = stage, y = pct)) +
    geom_col(fill = LIME, colour = NA, width = 0.65) +
    geom_col(aes(y = pmax(pct * 0.03, 0.2)), fill = NAVY,
             colour = NA, width = 0.65) +
    geom_text(aes(label = lbl), vjust = -0.45, colour = TEXT,
              size = 3.7, fontface = "bold", family = FONT) +
    scale_x_discrete(labels = stage_lbls) +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.18))) +
    labs(
      title    = "Germany — Probability of Reaching Each Stage",
      subtitle = sprintf("%s simulations", format(mc$n_sims, big.mark = ",")),
      x = NULL, y = NULL
    ) +
    theme_wc(11) +
    theme(
      axis.text.x        = element_text(size = 9, colour = TEXT, face = "bold"),
      axis.text.y        = element_text(size = 9, colour = MUTED),
      panel.grid.major.x = element_blank()
    )
}

# ══════════════════════════════════════════════════════════════
# PLOT 2c — Group Winner Probabilities (all groups, faceted)
# Strip headers match the app's lime .group-header
# ══════════════════════════════════════════════════════════════

plot_group_winners <- function(mc, teams_init) {
  df <- mc$gwin_df %>%
    filter(win_prob > 0.005) %>%
    group_by(group) %>%
    arrange(desc(win_prob)) %>%
    ungroup() %>%
    mutate(group_label = paste("GROUP", group),
           group_label = factor(group_label,
                                levels = paste("GROUP", sort(unique(group)))))

  ggplot(df, aes(x = win_prob * 100,
                 y = reorder(team_name, win_prob))) +
    geom_col(fill = LIME, colour = NA, width = 0.70) +
    geom_col(aes(x = pmax(win_prob * 100 * 0.025, 0.1)), fill = NAVY,
             colour = NA, width = 0.70) +
    geom_text(aes(label = sprintf("%.0f%%", win_prob * 100)),
              hjust = -0.15, colour = TEXT,
              size = 2.65, fontface = "bold", family = FONT) +
    facet_wrap(~ group_label, scales = "free_y", ncol = 4) +
    scale_x_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.32))) +
    labs(
      title    = "Group Winner Probabilities",
      subtitle = sprintf(
        "%s simulations  ·  Teams with > 0.5%% probability shown",
        format(mc$n_sims, big.mark = ",")),
      x = NULL, y = NULL
    ) +
    theme_wc(9) +
    theme(
      strip.text         = element_text(colour = TEXT, face = "bold", size = 8),
      strip.background   = element_rect(fill = LIME, colour = NA),
      axis.text.y        = element_text(size = 7.5, colour = TEXT),
      axis.text.x        = element_text(size = 7,   colour = MUTED),
      panel.grid.major.y = element_blank(),
      panel.spacing      = unit(0.7, "lines")
    )
}

# ══════════════════════════════════════════════════════════════
# MAIN
# ══════════════════════════════════════════════════════════════

message("Loading data...")
dat <- load_data()

# ── Page 1: Analytical score matrix ──────────────────────────
message("Building Germany score matrices (analytical, no simulation)...")
p1 <- plot_germany_matrix(dat$teams_init, dat$hist_sd,
                          germany_name = GERMANY_NAME, max_g = 5)
ggsave("wc2026_germany_matrix.png", p1,
       width = 15, height = 5.5, dpi = 180, bg = BG)
message("Saved: wc2026_germany_matrix.png")

# ── Page 2: Monte Carlo results ──────────────────────────────
message(sprintf("Running %d MC simulations (seed=%d)...", N_SIMS, SEED))
mc <- run_mc(dat$teams_init, dat$hist_sd,
             k = K, mwp = MAX_WIN_PROB,
             use_hist = USE_HIST, seed_base = SEED,
             n_sims = N_SIMS)

message("Building simulation result plots...")
p2a <- plot_champion_prob(mc, top_n = TOP_N)
p2b <- plot_germany_stages(mc, germany_name = GERMANY_NAME)
p2c <- plot_group_winners(mc, dat$teams_init)

page2 <- (p2a | p2b) / p2c +
  plot_layout(heights = c(1, 1.5)) +
  plot_annotation(
    title    = "FIFA World Cup 2026 — Monte Carlo Simulation Results",
    subtitle = sprintf(
      "%s simulations  ·  ELO model  ·  Historical score distribution  ·  K = %d  ·  Max win prob = %.0f%%",
      format(mc$n_sims, big.mark=","), K, MAX_WIN_PROB*100),
    caption  = "ELO source: eloratings.net  ·  Score model: historical World Cup & major tournament data",
    theme = theme(
      plot.background = element_rect(fill = BG, colour = NA),
      plot.title      = element_text(colour = TEXT,  size = 17, face = "bold",
                                     family = FONT, margin = margin(b = 4)),
      plot.subtitle   = element_text(colour = MUTED, size = 10, family = FONT,
                                     margin = margin(b = 6)),
      plot.caption    = element_text(colour = MUTED, size = 8,  family = FONT,
                                     hjust = 0, margin = margin(t = 8)),
      plot.margin     = margin(20, 20, 14, 20)
    )
  )

ggsave("wc2026_simulation_results.png", page2,
       width = 18, height = 14, dpi = 180, bg = BG)
message("Saved: wc2026_simulation_results.png")
message("✅ All done.")
