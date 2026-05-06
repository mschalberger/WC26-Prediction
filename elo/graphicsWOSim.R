library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales)

# ── CONFIG ───────────────────────────────────────────────────

N_SIMS       <- 20000
SEED         <- 42
K            <- 60
USE_HIST     <- TRUE
GERMANY_NAME <- "Germany"
TOP_N        <- 10


BG        <- "#FFFFFF"
PANEL_BG  <- "#FFFFFF"
BORDER    <- "#E6E6E6"
TEXT      <- "#000000"
MUTED     <- "#808080"
LIME      <- "#CCFF00"
NAVY      <- "#004659"   # --fublue
GREEN_LT  <- "#00A4D1"
RED_LT    <- "#E57050"
DRAW_COL  <- "#58756A"

# Score matrix cell fills (pastel, coloured per outcome region)
FILL_WIN  <- "#CCFF00"   # pale green
FILL_DRAW <- "#00A4D1"   # pale lime/yellow
FILL_LOSS <- "#E57050"   # pale red


FONT <- "sans"


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

flag_map <- c(
  MEX="🇲🇽", RSA="🇿🇦", KOR="🇰🇷", CZE="🇨🇿", CAN="🇨🇦", BIH="🇧🇦", QAT="🇶🇦",
  SUI="🇨🇭", BRA="🇧🇷", MAR="🇲🇦", HAI="🇭🇹", SCO="🏴󠁧󠁢󠁳󠁣󠁴󠁿", USA="🇺🇸", PAR="🇵🇾",
  AUS="🇦🇺", TUR="🇹🇷", GER="🇩🇪", CUR="🇨🇼", CIV="🇨🇮", ECU="🇪🇨", NED="🇳🇱",
  JPN="🇯🇵", SWE="🇸🇪", TUN="🇹🇳", BEL="🇧🇪", EGY="🇪🇬", IRN="🇮🇷", NZL="🇳🇿",
  ESP="🇪🇸", CPV="🇨🇻", KSA="🇸🇦", URU="🇺🇾", FRA="🇫🇷", SEN="🇸🇳", IRQ="🇮🇶",
  NOR="🇳🇴", ARG="🇦🇷", ALG="🇩🇿", AUT="🇦🇹", JOR="🇯🇴", POR="🇵🇹", COD="🇨🇩",
  UZB="🇺🇿", COL="🇨🇴", ENG="🏴󠁧󠁢󠁥󠁮󠁧󠁿", CRO="🇭🇷", GHA="🇬🇭", PAN="🇵🇦"
)
get_flag <- function(code) ifelse(is.na(flag_map[code]), "🏳️", unname(flag_map[code]))

# ── ELO ENGINE ───────────────────────────────────────────────

elo_expected <- function(ea, eb) 1 / (1 + 10^((eb - ea) / 400))

elo_wdl <- function(elo_h, elo_a) {
  p_h <- elo_expected(elo_h, elo_a)
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
  
  # ELO-derived W/D/L probabilities (from the favourite's point of view)
  draw_p  <- 1/3 * exp(-((p_fav - 0.5)^2) / (2 * 0.236875^2))
  p_fav_w <- p_fav * (1 - draw_p)   # P(favourite wins)
  p_und_w <- (1 - p_fav) * (1 - draw_p)   # P(underdog wins)
  
  outcome_weights <- list(
    list(csv = "fav_win", weight = p_fav_w),
    list(csv = "draw",    weight = draw_p),
    list(csv = "und_win", weight = p_und_w)
  )
  
  rows <- lapply(outcome_weights, function(oc) {
    if (oc$weight == 0) return(NULL)
    
    bin <- hist_sd %>%
      filter(outcome == oc$csv, p_lo <= p_fav_w, p_fav_w < p_hi)
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

# ══════════════════════════════════════════════════════════════
# PLOT 1 — Germany Group Score Matrices  (ANALYTICAL)
#
# Each cell's fill = outcome region (WIN / DRAW / LOSS).
# Each cell's text = joint probability of that exact scoreline.
# No simulation — derived analytically from score_dist.csv
# weighted by ELO win/draw/loss probabilities.
# ══════════════════════════════════════════════════════════════
plot_germany_matrix <- function(teams_init, hist_sd,
                                germany_name = "Deutschland", max_g = 5) {
  
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
        title    = sprintf("Deutschland gegen %s", opp$team_name),
        x        = "Tore Deutschland",
        y        = sprintf("Tore %s", opp$team_name)
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
      title = "Deutschland: Wahrscheinlichkeitsmatrix für die Gruppenphase",
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
# PLOT 1b — Germany Group Score Matrices  (MONTE CARLO / POISSON)
#
# Frequencies come directly from simulated scorelines stored
# during run_mc → run_tournament → run_group_stage.
# Each cell = share of simulations producing that exact score.
# ══════════════════════════════════════════════════════════════

plot_germany_matrix_mc <- function(mc, teams_init,
                                   germany_name = "Deutschland",
                                   max_g = 5,
                                   opps) {
  
  ger_g <- teams_init %>%
    filter(team_name == germany_name) %>%
    pull(group_letter)
  
  # Über alle Gegner iterieren -> Liste von Plots
  plots <- lapply(seq_len(nrow(opps)), function(i) {
    
    opp_nm <- opps$team_name[i]
    
    # ── Häufigkeitstabelle aus den MC-Simulationen ──
    raw <- mc$ger_scores_df %>%
      filter(opponent == opp_nm) %>%
      count(ger_goals, opp_goals, name = "n")
    
    n_total <- sum(raw$n)
    
    dist <- raw %>%
      mutate(prob = n / n_total) %>%
      filter(ger_goals <= max_g, opp_goals <= max_g)
    
    grid <- expand.grid(ger_goals = 0:max_g, opp_goals = 0:max_g) %>%
      left_join(dist, by = c("ger_goals", "opp_goals")) %>%
      mutate(
        prob   = ifelse(is.na(prob), 0, prob),
        result = case_when(
          ger_goals > opp_goals ~ "WIN",
          ger_goals < opp_goals ~ "LOSS",
          TRUE                  ~ "DRAW"
        ),
        label  = ifelse(prob >= 0.005,
                        sprintf("%.1f%%", prob * 100),
                        "<.05%")
      )
    
    # ── Marginale W/D/L-Wahrscheinlichkeiten ──
    wdl_df <- grid %>%
      group_by(result) %>%
      summarise(prob = sum(prob), .groups = "drop") %>%
      mutate(result = factor(result, levels = c("WIN", "DRAW", "LOSS")))
    
    # ── Farbverlauf je Region ──
    grid <- grid %>%
      mutate(
        region = case_when(
          ger_goals >  opp_goals ~ "win",
          ger_goals <  opp_goals ~ "loss",
          TRUE                   ~ "draw"
        ),
        prob_norm = if (max(prob) > 0) prob / max(prob) else 0
      )
    
    grid$fill_col <- mapply(
      function(reg, p) {
        end_col <- switch(reg,
                          win  = FILL_WIN,
                          loss = FILL_LOSS,
                          draw = FILL_DRAW)
        scales::seq_gradient_pal("white", end_col)(p)
      },
      grid$region, grid$prob_norm
    )
    
    # ── Heatmap ──
    p_heat <- ggplot(grid, aes(x = ger_goals, y = opp_goals)) +
      geom_tile(aes(fill = fill_col), colour = BORDER, linewidth = 0.9) +
      geom_text(aes(label = label),
                colour = TEXT, size = 2.9, fontface = "bold", family = FONT) +
      scale_fill_identity() +
      scale_x_continuous(breaks = 0:max_g, expand = expansion(add = 0.52)) +
      scale_y_continuous(breaks = 0:max_g, expand = expansion(add = 0.52)) +
      labs(
        x = "Tore Deutschland",
        y = sprintf("Tore %s", opp_nm)
      ) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background  = element_blank(),
        legend.position  = "none"
      )
    
    # ── W/D/L-Balken ──
    p_bar <- ggplot(wdl_df, aes(x = 1, y = prob, fill = result)) +
      geom_col(width = 0.6) +
      geom_text(
        aes(label = paste0(round(prob * 100), "%")),
        position = position_stack(vjust = 0.5),
        size = 3, colour = "black"
      ) +
      coord_flip() +
      scale_fill_manual(
        values = c("WIN" = LIME, "DRAW" = FILL_DRAW, "LOSS" = RED_LT),
        name   = NULL
      ) +
      theme(
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        panel.background  = element_blank(),
        plot.background   = element_blank(),
        axis.text         = element_blank(),
        axis.ticks        = element_blank(),
        axis.title        = element_blank(),
        legend.position   = "none"
      )
    
    # ── Patchwork zusammenbauen UND zurückgeben ──
    (p_heat / p_bar) +
      plot_layout(heights = c(4, 1)) +
      plot_annotation(
        title    = sprintf("Deutschland gegen %s", opp_nm),
        # subtitle = sprintf(
        #   "%s Simulationen  ·  Poisson + ELO Modell  ·  Häufigkeiten aus simulierten Spielständen",
        #   format(mc$n_sims, big.mark = ",")),
        theme = theme(
          plot.title    = element_text(colour = TEXT,  size = 15, face = "bold",
                                       family = FONT,  margin = margin(b = 4)),
          plot.subtitle = element_text(colour = MUTED, size = 9,  family = FONT,
                                       margin = margin(b = 6)),
          plot.margin   = margin(20, 20, 14, 20)
        )
      )
  })
  
  # Liste mit Gegner-Namen benennen
  names(plots) <- opps$team_name
  
  plots
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
    geom_text(aes(label = sprintf("%.1f%%", pct)),
              hjust = -0.12, colour = TEXT,
              size = 3.7, fontface = "bold", family = FONT) +
    scale_x_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.22))) +
    labs(
      title    = "Weltmeisterwahrscheinlichkeiten",
      subtitle = sprintf("Die Besten %d Nationen  ·  %s Simulationen",
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

plot_germany_stages <- function(mc, germany_name = "Deutschland") {
  stage_keys <- c("Round of 32","Round of 16","Quarter-Final",
                  "Semi-Final","Final","Champion")
  stage_lbls <- c("Sechzehntel-\nfinale","Achtel-\nfinale","Viertel-\nfinale",
                  "Halb-\nfinale","Finale","Weltmeister")
  
  ger <- mc$reach_df %>% filter(team_name == germany_name)
  if (nrow(ger) == 0) stop("Deutschland not found in results.")
  
  df <- data.frame(
    stage = factor(stage_keys, levels = stage_keys),
    label = stage_lbls,
    pct   = as.numeric(ger[1, stage_keys]) * 100
  ) %>%
    mutate(lbl = sprintf("%.1f%%", pct))
  
  ggplot(df, aes(x = stage, y = pct)) +
    geom_col(fill = LIME, colour = NA, width = 0.65) +
    geom_text(aes(label = lbl), vjust = -0.45, colour = TEXT,
              size = 3.7, fontface = "bold", family = FONT) +
    scale_x_discrete(labels = stage_lbls) +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.18))) +
    labs(
      title    = "Wahrscheinlichkeiten für Deutschland",
      subtitle = sprintf("%s Simulationen", format(mc$n_sims, big.mark = ",")),
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
  gadv_df <- mc$reach_df %>%
    select(id, adv_prob = `Round of 32`)
  
  df <- mc$gwin_df %>%
    left_join(gadv_df, by = "id") %>%
    filter(adv_prob > 0.01) %>%   # optional filter
    group_by(group) %>%
    arrange(desc(adv_prob)) %>%
    ungroup() %>%
    mutate(
      group_label = paste("GROUP", group),
      group_label = factor(group_label,
                           levels = paste("GROUP", sort(unique(group))))
    )
  
  ggplot(df, aes(x = win_prob * 100,
                 y = reorder(team_name, win_prob))) +
    geom_col(aes(x = adv_prob * 100),
             fill = MUTED, alpha = 0.35, width = 0.75) +
    geom_text(
      aes(x = adv_prob * 100,
          label = sprintf("%.0f%%", adv_prob * 100)),
      hjust = -0.15,
      colour = MUTED,
      size = 2.5,
      family = FONT
    ) +
    geom_col(aes(x = win_prob * 100),
             fill = LIME, width = 0.55) +
    geom_text(
      aes(x = win_prob * 100,
          label = sprintf("%.0f%%", win_prob * 100)),
      hjust = -0.15,
      colour = TEXT,
      size = 2.6,
      fontface = "bold",
      family = FONT
    ) +
    facet_wrap(~ group_label, scales = "free_y", ncol = 2) +
    scale_x_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.32))) +
    labs(
      title    = "Gruppensieger",
      subtitle = sprintf(
        "%s Simulationen  ·  Nur Mannschaften mit > 0.5%% Wahrscheinlichkeit",
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
    ) +
    xlim(0, 105)
}

# ══════════════════════════════════════════════════════════════
# MAIN
# ══════════════════════════════════════════════════════════════

message("Loading data...")
dat <- load_data()
mc <- readRDS("output/wc2026_mc_results.rds")

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
# Hilfsfunktion: alles, was nicht in der Tabelle ist, bleibt unverändert
translate <- function(x) ifelse(x %in% names(team_de), team_de[x], x)

# Auf alle relevanten Stellen anwenden
dat$teams_init$team_name    <- translate(dat$teams_init$team_name)
mc$ger_scores_df$opponent   <- translate(mc$ger_scores_df$opponent)
mc$reach_df$team_name       <- translate(mc$reach_df$team_name)
mc$gwin_df$team_name        <- translate(mc$gwin_df$team_name)


# ── Page 1: Analytical score matrix ──────────────────────────
message("Building Germany score matrices (analytical, no simulation)...")
p1 <- plot_germany_matrix(dat$teams_init, dat$hist_sd,
                          germany_name = "Deutschland", max_g = 5)
message("Saved: wc2026_germany_matrix.png")
p1


# ── Page 2: Monte Carlo results ──────────────────────────────
message(sprintf("Running %d MC simulations (seed=%d)...", N_SIMS, SEED))
# mc <- run_mc(dat$teams_init, dat$hist_sd,
#              k = K,
#              use_hist = USE_HIST, seed_base = SEED,
#              n_sims = N_SIMS)
# saveRDS(mc, "output/wc2026_mc_results.rds")

message("Building simulation result plots...")

opps_aux = dat$teams_init %>%
  filter(group_letter == "E", team_name != "Deutschland")

opps = opps_aux[1, ]

p1mc <- plot_germany_matrix_mc(mc, dat$teams_init,
                               germany_name = "Deutschland", max_g = 4,
                               opps)
p1mc
ggsave("Figures/GERCuracao.png",
       plot   = p1mc,
       width  = 6, height = 4,
       dpi    = 300,
       bg     = "white")

opps = opps_aux[2, ]
p1mc <- plot_germany_matrix_mc(mc, dat$teams_init,
                               germany_name = "Deutschland", max_g = 4,
                               opps)
p1mc
ggsave("Figures/GERCoteDIvoire.png",
       plot   = p1mc,
       width  = 6, height = 4,
       dpi    = 300,
       bg     = "white")

opps = opps_aux[3, ]
p1mc <- plot_germany_matrix_mc(mc, dat$teams_init,
                               germany_name = "Deutschland", max_g = 4,
                               opps)
p1mc
ggsave("Figures/GEREcuador.png",
       plot   = p1mc,
       width  = 6, height = 4,
       dpi    = 300,
       bg     = "white")

p2a <- plot_champion_prob(mc, top_n = TOP_N)
p2b <- plot_germany_stages(mc, germany_name = "Deutschland")
p2c <- plot_group_winners(mc, dat$teams_init)

p2a
ggsave("Figures/Winners.png",
       plot   = p2a,
       width  = 6, height = 4,
       dpi    = 300,
       bg     = "white")
p2b
ggsave("Figures/GERWeiterkommen.png",
       plot   = p2b,
       width  = 6, height = 4,
       dpi    = 300,
       bg     = "white")
p2c
ggsave("Figures/Gruppensieger.png",
       plot   = p2c,
       width  = 6, height = 12,
       dpi    = 300,
       bg     = "white")
page2 <- (p2a | p2b) / p2c +
  plot_layout(heights = c(1, 1.5)) +
  plot_annotation(
    title    = "FIFA World Cup 2026 — Monte Carlo Simulation Results",
    subtitle = sprintf(
      "%s simulations  ·  ELO model  ·  Historical score distribution  ·  K = %d",
      format(mc$n_sims, big.mark=","), K),
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
