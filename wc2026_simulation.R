# ============================================================
# FIFA World Cup 2026 — Monte Carlo Win Distribution
# Usage: source("wc2026_simulation.R")
# Packages: dplyr, ggplot2, tidyr, scales, ggtext, showtext
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(scales)
  library(ggtext)
})

# ── OPTIONAL: nicer fonts via showtext ────────────────────────
if (requireNamespace("showtext", quietly = TRUE)) {
  showtext::showtext_auto()
  sysfonts::font_add_google("Bebas Neue", "bebas")
  sysfonts::font_add_google("Inter",      "inter")
  FONT_TITLE <- "bebas"
  FONT_BODY  <- "inter"
} else {
  FONT_TITLE <- "sans"
  FONT_BODY  <- "sans"
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ── CONFIGURATION ─────────────────────────────────────────────
N_SIMS     <- 1000   # number of tournament simulations
K_FACTOR   <- 20     # ELO K-factor
SEED       <- 42     # master seed (set NULL for pure random)

# ── OUTPUT DIRECTORY ─────────────────────────────────────────
OUT_DIR <- "output"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
out <- function(f) file.path(OUT_DIR, f)

# ── DATA ──────────────────────────────────────────────────────
message("Loading data...")
matches <- read.csv("data/matches.csv")
teams   <- read.csv("data/teams.csv")

elo       <- read.delim("https://www.eloratings.net/World.tsv?_=1772102421796",    sep = "\t", header = FALSE)
countries <- read.delim("https://www.eloratings.net/en.teams.tsv?_=1772102421794", sep = "\t", header = FALSE)

elo <- elo %>%
  left_join(countries, by = c("V3" = "V1")) %>%
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
    team_name == "Winner FIFA Playoff 2" ~ "Bolivia",
    team_name == "Winner UEFA Playoff B" ~ "Poland",
    team_name == "Winner UEFA Playoff C" ~ "Turkey",
    team_name == "Winner UEFA Playoff A" ~ "Italy",
    team_name == "Winner UEFA Playoff D" ~ "Denmark",
    TRUE ~ team_name
  )) %>%
  left_join(elo, by = c("team_name" = "country"))

missing <- teams_init %>% filter(is.na(elo)) %>% pull(team_name)
if (length(missing) > 0) {
  message("⚠️  No ELO for: ", paste(missing, collapse = ", "), " — using median")
  teams_init <- teams_init %>%
    mutate(elo = ifelse(is.na(elo), median(elo, na.rm = TRUE), elo))
}

# ── ELO ENGINE ────────────────────────────────────────────────
elo_expected <- function(ea, eb) 1 / (1 + 10^((eb - ea) / 400))

simulate_match <- function(elo_home, elo_away, k = K_FACTOR) {
  p_h    <- elo_expected(elo_home, elo_away)
  draw_p <- 1/3 * exp(-((p_h - .5)^2) / (2 * 0.28^2))
  ph     <- p_h * (1 - draw_p)
  pa     <- (1 - p_h) * (1 - draw_p)
  outcome <- sample(c("home", "draw", "away"), 1, prob = c(ph, draw_p, pa))
  lh <- 1.8 * ph + 0.27
  la <- 1.8 * pa + 0.27
  repeat {
    gh <- rpois(1, lh); ga <- rpois(1, la)
    if (outcome == "home" && gh >  ga) break
    if (outcome == "away" && ga >  gh) break
    if (outcome == "draw" && gh == ga) break
  }
  act_h     <- ifelse(outcome == "home", 1, ifelse(outcome == "draw", 0.5, 0))
  exp_h     <- elo_expected(elo_home, elo_away)
  goal_diff <- abs(gh - ga)
  k_mult    <- if (goal_diff <= 1) 1
  else if (goal_diff == 2) 1.5
  else if (goal_diff == 3) 1.75
  else 1.75 + (goal_diff - 3) / 8
  k_adj <- k * k_mult
  list(home_goals = gh, away_goals = ga, outcome = outcome,
       new_elo_home = elo_home + k_adj * (act_h - exp_h),
       new_elo_away = elo_away + k_adj * ((1 - act_h) - (1 - exp_h)))
}

# ── GROUP STAGE ───────────────────────────────────────────────
run_group_stage <- function(teams_df, k = K_FACTOR) {
  elo_live      <- setNames(teams_df$elo, teams_df$id)
  all_standings <- data.frame()

  for (grp in sort(unique(teams_df$group_letter))) {
    ids   <- teams_df %>% filter(group_letter == grp) %>% pull(id)
    pairs <- combn(ids, 2, simplify = FALSE)
    pts <- gf <- ga <- setNames(rep(0, 4), ids)

    for (pair in pairs) {
      h <- pair[1]; a <- pair[2]
      res <- simulate_match(elo_live[as.character(h)], elo_live[as.character(a)], k = k)
      elo_live[as.character(h)] <- res$new_elo_home
      elo_live[as.character(a)] <- res$new_elo_away
      gf[as.character(h)] <- gf[as.character(h)] + res$home_goals
      ga[as.character(h)] <- ga[as.character(h)] + res$away_goals
      gf[as.character(a)] <- gf[as.character(a)] + res$away_goals
      ga[as.character(a)] <- ga[as.character(a)] + res$home_goals
      if      (res$outcome == "home") pts[as.character(h)] <- pts[as.character(h)] + 3
      else if (res$outcome == "away") pts[as.character(a)] <- pts[as.character(a)] + 3
      else {
        pts[as.character(h)] <- pts[as.character(h)] + 1
        pts[as.character(a)] <- pts[as.character(a)] + 1
      }
    }

    standing <- data.frame(id = ids, pts = as.numeric(pts),
                           gf = as.numeric(gf), ga = as.numeric(ga)) %>%
      mutate(gd = gf - ga, elo = elo_live[as.character(ids)]) %>%
      arrange(desc(pts), desc(gd), desc(gf), desc(elo)) %>%
      mutate(rank = 1:4, group = grp) %>%
      left_join(teams_df %>% select(id, team_name, fifa_code), by = "id")
    all_standings <- rbind(all_standings, standing)
  }
  list(standings = all_standings, elo_live = elo_live)
}

# ── KNOCKOUT ──────────────────────────────────────────────────
sim_ko_match <- function(id_a, id_b, elo_live, teams_df, k = K_FACTOR) {
  res    <- simulate_match(elo_live[as.character(id_a)], elo_live[as.character(id_b)], k = k)
  winner <- if (res$outcome == "draw") {
    pa <- elo_expected(elo_live[as.character(id_a)], elo_live[as.character(id_b)])
    ifelse(runif(1) < pa, id_a, id_b)
  } else {
    ifelse(res$outcome == "home", id_a, id_b)
  }
  loser <- ifelse(winner == id_a, id_b, id_a)
  elo_live[as.character(id_a)] <- res$new_elo_home
  elo_live[as.character(id_b)] <- res$new_elo_away
  list(winner = winner, loser = loser, elo_live = elo_live)
}

run_knockout <- function(pairs, elo_live, teams_df, k = K_FACTOR) {
  winners <- c(); losers <- c()
  for (pair in pairs) {
    res      <- sim_ko_match(pair[1], pair[2], elo_live, teams_df, k = k)
    elo_live <- res$elo_live
    winners  <- c(winners, res$winner)
    losers   <- c(losers,  res$loser)
  }
  list(winners = winners, losers = losers, elo_live = elo_live)
}

# ── FULL TOURNAMENT ───────────────────────────────────────────
run_tournament_slim <- function(seed = NULL, k = K_FACTOR) {
  if (!is.null(seed)) set.seed(seed)
  teams_df <- teams_init

  gs       <- run_group_stage(teams_df, k = k)
  elo_live <- gs$elo_live
  std      <- gs$standings

  thirds <- std %>% filter(rank == 3) %>%
    arrange(desc(pts), desc(gd), desc(gf)) %>% slice(1:8)

  get_t  <- function(grp, rnk) std %>% filter(group == grp, rank == rnk) %>% pull(id)

  r32_pairs <- list(
    c(get_t("A",2), get_t("B",2)),  c(get_t("C",1), get_t("F",2)),
    c(get_t("E",1), thirds$id[1]),  c(get_t("F",1), get_t("C",2)),
    c(get_t("E",2), get_t("I",2)),  c(get_t("I",1), thirds$id[2]),
    c(get_t("A",1), thirds$id[3]),  c(get_t("L",1), thirds$id[4]),
    c(get_t("G",1), thirds$id[5]),  c(get_t("D",1), thirds$id[6]),
    c(get_t("H",1), get_t("J",2)),  c(get_t("K",2), get_t("L",2)),
    c(get_t("B",1), thirds$id[7]),  c(get_t("D",2), get_t("G",2)),
    c(get_t("J",1), get_t("H",2)),  c(get_t("K",1), thirds$id[8])
  )

  r32 <- run_knockout(r32_pairs, elo_live, teams_df, k = k); elo_live <- r32$elo_live
  r16_pairs <- lapply(seq(1,15,2), function(i) c(r32$winners[i], r32$winners[i+1]))
  r16 <- run_knockout(r16_pairs, elo_live, teams_df, k = k); elo_live <- r16$elo_live
  qf_pairs  <- lapply(seq(1,7,2),  function(i) c(r16$winners[i], r16$winners[i+1]))
  qf  <- run_knockout(qf_pairs,  elo_live, teams_df, k = k); elo_live <- qf$elo_live
  sf_pairs  <- list(c(qf$winners[1], qf$winners[2]), c(qf$winners[3], qf$winners[4]))
  sf  <- run_knockout(sf_pairs,  elo_live, teams_df, k = k); elo_live <- sf$elo_live
  tp  <- run_knockout(list(c(sf$losers[1],  sf$losers[2])),         elo_live, teams_df, k = k)
  fin <- run_knockout(list(c(sf$winners[1], sf$winners[2])),        elo_live, teams_df, k = k)

  # Assign knockout-exit round to each team id
  team_ids   <- teams_df$id
  exit_round <- setNames(rep("Group Stage", length(team_ids)), team_ids)

  mark_exit <- function(ids, round_name) {
    for (id in ids) exit_round[as.character(id)] <<- round_name
  }
  mark_exit(r32$losers,  "Round of 32")
  mark_exit(r16$losers,  "Round of 16")
  mark_exit(qf$losers,   "Quarter-Final")
  # sf$losers all play the 3rd-place match, so skip "Semi-Final" label —
  # tp and fin will correctly assign Third Place / Fourth Place / Runner-Up / Champion
  mark_exit(tp$losers,   "Fourth Place")
  mark_exit(tp$winners,  "Third Place")
  mark_exit(fin$losers,  "Runner-Up")
  mark_exit(fin$winners, "Champion")

  data.frame(
    id         = as.integer(names(exit_round)),
    exit_round = as.character(exit_round),
    stringsAsFactors = FALSE
  )
}

# ── RUN SIMULATIONS ───────────────────────────────────────────
message(sprintf("Running %d simulations (K=%d)...", N_SIMS, K_FACTOR))
if (!is.null(SEED)) set.seed(SEED)

seeds <- sample(1:1e6, N_SIMS)

results_list <- vector("list", N_SIMS)
pb <- txtProgressBar(min = 0, max = N_SIMS, style = 3)
for (i in seq_len(N_SIMS)) {
  results_list[[i]] <- run_tournament_slim(seed = seeds[i])
  setTxtProgressBar(pb, i)
}
close(pb)

all_results <- bind_rows(results_list) %>%
  left_join(teams_init %>% select(id, team_name, fifa_code), by = "id")

# ── AGGREGATE ─────────────────────────────────────────────────
round_levels <- c("Group Stage", "Round of 32", "Round of 16",
                  "Quarter-Final", "Fourth Place",
                  "Third Place", "Runner-Up", "Champion")

summary_df <- all_results %>%
  group_by(team_name, fifa_code, exit_round) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(pct = n / N_SIMS * 100,
         exit_round = factor(exit_round, levels = round_levels))

# Champion probability for ordering
champ_prob <- summary_df %>%
  filter(exit_round == "Champion") %>%
  select(team_name, champ_pct = pct)

# Full grid (some teams never reach some rounds — fill with 0)
full_grid <- expand.grid(
  team_name  = unique(summary_df$team_name),
  exit_round = factor(round_levels, levels = round_levels),
  stringsAsFactors = FALSE
) %>%
  left_join(summary_df %>% select(team_name, exit_round, pct),
            by = c("team_name", "exit_round")) %>%
  replace_na(list(pct = 0)) %>%
  left_join(champ_prob, by = "team_name") %>%
  replace_na(list(champ_pct = 0)) %>%
  arrange(champ_pct)

team_order <- unique(full_grid$team_name)   # already sorted by champ probability

# ── PLOT 1: Stacked bar — champion probability top 20 ─────────
message("Generating plots...")

champ_top <- summary_df %>%
  filter(exit_round == "Champion") %>%
  arrange(desc(pct)) %>%
  slice(1:20) %>%
  left_join(teams_init %>% select(team_name, elo), by = "team_name")

p_champ <- ggplot(champ_top, aes(x = reorder(team_name, pct), y = pct)) +
  geom_col(aes(fill = pct), width = 0.72, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            hjust = -0.15, size = 3.2, family = FONT_BODY,
            color = "#E8E8F0", fontface = "bold") +
  scale_fill_gradient(low = "#1A5276", high = "#F5C518") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)),
                     labels = percent_format(scale = 1)) +
  coord_flip() +
  labs(
    title    = "🏆 FIFA WORLD CUP 2026",
    subtitle = sprintf("Champion Probability — %s simulations  •  K-factor = %d", scales::comma(N_SIMS), K_FACTOR),
    x        = NULL,
    y        = "Win Probability",
    caption  = "ELO-based Monte Carlo simulation"
  ) +
  theme_minimal(base_family = FONT_BODY) +
  theme(
    plot.background  = element_rect(fill = "#1A1A26", color = NA),
    panel.background = element_rect(fill = "#1A1A26", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#32324A", linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    axis.text          = element_text(color = "#A0A0B0", size = 11, family = FONT_BODY),
    axis.text.x        = element_text(color = "#6B6B80", size = 9),
    plot.title         = element_text(family = FONT_TITLE, size = 28, color = "#F5C518",
                                      letter_spacing = 4, margin = margin(b = 4)),
    plot.subtitle      = element_text(color = "#7EC820", size = 11, margin = margin(b = 16)),
    plot.caption       = element_text(color = "#6B6B80", size = 9),
    plot.margin        = margin(20, 28, 16, 20)
  )

ggsave(out("wc2026_champion_probability.png"), p_champ, width = 10, height = 8, dpi = 180, bg = "#1A1A26")
message(paste("✅ Saved:", out("wc2026_champion_probability.png")))

# ── PLOT 2: Heatmap — all rounds, all teams ────────────────────
plot_df <- full_grid %>%
  mutate(team_name = factor(team_name, levels = team_order))

# Colour palette per round
round_palette <- c(
  "Group Stage"    = "#C8102E",
  "Round of 32"    = "#E05B1A",
  "Round of 16"    = "#E09B1A",
  "Quarter-Final"  = "#D4C014",
  "Semi-Final"     = "#7EC820",
  "Fourth Place"   = "#2979FF",
  "Third Place"    = "#CD7F32",
  "Runner-Up"      = "#A8A9AD",
  "Champion"       = "#F5C518"
)

# Only show labels >= 5%
p_heat <- ggplot(plot_df, aes(x = exit_round, y = team_name, fill = pct)) +
  geom_tile(color = "#1A1A26", linewidth = 0.5) +
  geom_text(aes(label = ifelse(pct >= 5, sprintf("%.0f", pct), "")),
            size = 2.8, color = "#000000", fontface = "bold", family = FONT_BODY) +
  scale_fill_gradientn(
    colors = c("#0E0E18", "#1A3A5C", "#1A5276", "#2E86C1", "#7EC820", "#F5C518"),
    values  = scales::rescale(c(0, 5, 15, 30, 60, 100)),
    name    = "% of sims",
    limits  = c(0, 100)
  ) +
  scale_x_discrete(guide = guide_axis(angle = 30)) +
  labs(
    title    = "🌍 WORLD CUP 2026 — Full Tournament Distribution",
    subtitle = sprintf("%s Monte Carlo simulations  •  K = %d  •  Sorted by champion probability",
                       scales::comma(N_SIMS), K_FACTOR),
    x        = NULL, y = NULL,
    caption  = "Cell values shown when ≥ 5%  •  ELO-based simulation"
  ) +
  theme_minimal(base_family = FONT_BODY) +
  theme(
    plot.background   = element_rect(fill = "#1A1A26", color = NA),
    panel.background  = element_rect(fill = "#1A1A26", color = NA),
    panel.grid        = element_blank(),
    axis.text.x       = element_text(color = "#A0A0B0", size = 9.5, family = FONT_BODY),
    axis.text.y       = element_text(color = "#C8C8D8", size = 7.5, family = FONT_BODY),
    legend.background = element_rect(fill = "#1A1A26", color = NA),
    legend.text       = element_text(color = "#A0A0B0", size = 8),
    legend.title      = element_text(color = "#A0A0B0", size = 9),
    legend.key.width  = unit(1.2, "cm"),
    plot.title        = element_text(family = FONT_TITLE, size = 24, color = "#F5C518",
                                     margin = margin(b = 4)),
    plot.subtitle     = element_text(color = "#7EC820", size = 10, margin = margin(b = 12)),
    plot.caption      = element_text(color = "#6B6B80", size = 8.5),
    plot.margin       = margin(20, 24, 16, 20)
  )

ggsave(out("wc2026_full_heatmap.png"), p_heat, width = 14, height = 16, dpi = 180, bg = "#1A1A26")
message(paste("✅ Saved:", out("wc2026_full_heatmap.png")))

# ── PLOT 3: Progression stacked bar (reach at least round X) ──
# "Probability of reaching AT LEAST this round"
reach_order <- c("Round of 32", "Round of 16", "Quarter-Final",
                 "Semi-Final", "Final", "Champion")

reach_df <- all_results %>%
  mutate(
    reached_r32 = exit_round != "Group Stage",
    reached_r16 = exit_round %in% c("Round of 16","Quarter-Final","Semi-Final",
                                    "Fourth Place","Third Place","Runner-Up","Champion"),
    reached_qf  = exit_round %in% c("Quarter-Final","Semi-Final",
                                    "Fourth Place","Third Place","Runner-Up","Champion"),
    reached_sf  = exit_round %in% c("Semi-Final","Fourth Place","Third Place","Runner-Up","Champion"),
    reached_fin = exit_round %in% c("Third Place","Runner-Up","Champion"),
    champion    = exit_round == "Champion"
  ) %>%
  group_by(team_name) %>%
  summarise(
    `Round of 32`   = mean(reached_r32)  * 100,
    `Round of 16`   = mean(reached_r16)  * 100,
    `Quarter-Final` = mean(reached_qf)   * 100,
    `Semi-Final`    = mean(reached_sf)   * 100,
    `Final`         = mean(reached_fin)  * 100,
    `Champion`      = mean(champion)     * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(-team_name, names_to = "round", values_to = "pct") %>%
  mutate(round = factor(round, levels = reach_order))

# Top 24 by champion prob
top24 <- champ_prob %>% arrange(desc(champ_pct)) %>% slice(1:24) %>% pull(team_name)

reach_top <- reach_df %>%
  filter(team_name %in% top24) %>%
  left_join(champ_prob, by = "team_name") %>%
  mutate(team_name = factor(team_name, levels = top24))

p_reach <- ggplot(reach_top, aes(x = team_name, y = pct, color = round, group = round)) +
  geom_line(linewidth = 0.6, alpha = 0.5) +
  geom_point(aes(size = pct), alpha = 0.9) +
  scale_color_manual(values = c(
    "Round of 32"   = "#E05B1A",
    "Round of 16"   = "#E09B1A",
    "Quarter-Final" = "#D4C014",
    "Semi-Final"    = "#7EC820",
    "Final"         = "#2979FF",
    "Champion"      = "#F5C518"
  ), name = "Reached round") +
  scale_size_continuous(range = c(1, 5), guide = "none") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 105)) +
  scale_x_discrete(guide = guide_axis(angle = 40)) +
  labs(
    title    = "⚽ ROUND PROGRESSION — Top 24 Teams",
    subtitle = sprintf("% reaching each knockout round  •  %s sims", scales::comma(N_SIMS)),
    x = NULL, y = "Probability (%)",
    caption  = "ELO-based Monte Carlo"
  ) +
  theme_minimal(base_family = FONT_BODY) +
  theme(
    plot.background  = element_rect(fill = "#1A1A26", color = NA),
    panel.background = element_rect(fill = "#1A1A26", color = NA),
    panel.grid.major.y = element_line(color = "#32324A", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text          = element_text(color = "#A0A0B0", size = 9, family = FONT_BODY),
    axis.title.y       = element_text(color = "#6B6B80", size = 10),
    legend.background  = element_rect(fill = "#22223A", color = "#32324A"),
    legend.text        = element_text(color = "#C8C8D8", size = 9),
    legend.title       = element_text(color = "#A0A0B0", size = 9),
    plot.title         = element_text(family = FONT_TITLE, size = 24, color = "#F5C518",
                                      margin = margin(b = 4)),
    plot.subtitle      = element_text(color = "#7EC820", size = 10, margin = margin(b = 12)),
    plot.caption       = element_text(color = "#6B6B80", size = 8.5),
    plot.margin        = margin(20, 24, 16, 20)
  )

ggsave(out("wc2026_progression.png"), p_reach, width = 14, height = 7, dpi = 180, bg = "#1A1A26")
message(paste("✅ Saved:", out("wc2026_progression.png")))

# ── PRINT SUMMARY TABLE ───────────────────────────────────────
cat("\n")
cat("══════════════════════════════════════════════════════\n")
cat(sprintf("  FIFA WORLD CUP 2026 — Monte Carlo Results (%d sims)\n", N_SIMS))
cat("══════════════════════════════════════════════════════\n")

summary_wide <- summary_df %>%
  select(team_name, exit_round, pct) %>%
  pivot_wider(names_from = exit_round, values_from = pct, values_fill = 0) %>%
  left_join(champ_prob, by = "team_name") %>%
  arrange(desc(champ_pct)) %>%
  select(Team = team_name,
         any_of(c("Champion", "Runner-Up", "Third Place", "Semi-Final",
                  "Quarter-Final", "Round of 16", "Round of 32", "Group Stage")))

print(as.data.frame(summary_wide), digits = 1, row.names = FALSE)
cat("\nAll percentages = % of simulations reaching that exact exit round.\n")

# Also save as CSV
write.csv(summary_wide, out("wc2026_simulation_results.csv"), row.names = FALSE)
message(paste("✅ Saved:", out("wc2026_simulation_results.csv")))
message(sprintf("\nAll outputs saved to ./%s/", OUT_DIR))

# ── INTERACTIVE DT TABLE ──────────────────────────────────────
# install.packages(c("DT", "htmlwidgets"))
if (requireNamespace("DT", quietly = TRUE) &&
    requireNamespace("htmlwidgets", quietly = TRUE)) {

  library(DT)
  library(htmlwidgets)
  library(htmltools)

  # ── Build display table ──
  round_cols <- c("Champion", "Runner-Up", "Third Place", "Fourth Place",
                  "Quarter-Final", "Round of 16",
                  "Round of 32", "Group Stage")

  dt_df <- summary_df %>%
    select(team_name, exit_round, pct) %>%
    pivot_wider(names_from = exit_round, values_from = pct, values_fill = 0) %>%
    left_join(teams_init %>% select(team_name, elo, group_letter, fifa_code), by = "team_name") %>%
    left_join(champ_prob, by = "team_name") %>%
    replace_na(list(champ_pct = 0)) %>%
    arrange(desc(champ_pct)) %>%
    mutate(
      Rank    = row_number(),
      ELO     = round(elo),
      Group   = group_letter,
      Flag    = sapply(fifa_code, function(code) {
        flag_map <- c(MEX="🇲🇽",RSA="🇿🇦",KOR="🇰🇷",UEPD="🇩🇰",CAN="🇨🇦",UEPA="🇮🇹",
                      QAT="🇶🇦",SUI="🇨🇭",BRA="🇧🇷",MAR="🇲🇦",HAI="🇭🇹",SCO="🏴󠁧󠁢󠁳󠁣󠁴󠁿",
                      USA="🇺🇸",PAR="🇵🇾",AUS="🇦🇺",UEPC="🇹🇷",GER="🇩🇪",CUR="🇨🇼",
                      CIV="🇨🇮",ECU="🇪🇨",NED="🇳🇱",JPN="🇯🇵",UEPB="🇵🇱",TUN="🇹🇳",
                      BEL="🇧🇪",EGY="🇪🇬",IRN="🇮🇷",NZL="🇳🇿",ESP="🇪🇸",CPV="🇨🇻",
                      KSA="🇸🇦",URU="🇺🇾",FRA="🇫🇷",SEN="🇸🇳",FP02="🇧🇴",NOR="🇳🇴",
                      ARG="🇦🇷",ALG="🇩🇿",AUT="🇦🇹",JOR="🇯🇴",POR="🇵🇹",FP01="🇨🇩",
                      UZB="🇺🇿",COL="🇨🇴",ENG="🏴󠁧󠁢󠁥󠁮󠁧󠁿",CRO="🇭🇷",GHA="🇬🇭",PAN="🇵🇦")
        ifelse(is.na(flag_map[code]), "🏳️", flag_map[code])
      }),
      Team    = paste(Flag, team_name)
    ) %>%
    # ensure all round columns exist (fill missing with 0)
    { for (col in round_cols) if (!col %in% names(.)) .[[col]] <- 0; . } %>%
    select(Rank, Team, Group, ELO, all_of(round_cols)) %>%
    mutate(across(all_of(round_cols), ~ round(.x, 1)))

  # ── Colour-bar JS helper ──
  # Renders a % value as a coloured progress bar + label inside the cell
  bar_js <- function(color_hex) {
    JS(sprintf(
      "function(data, type, row, meta) {
         if (type !== 'display') return data;
         var pct = parseFloat(data) || 0;
         var bar = '<div style=\"background:%s;width:' + Math.round(pct) + '%%%%;' +
                   'height:6px;border-radius:3px;margin-top:3px;\"></div>';
         return '<div style=\"line-height:1.1;font-size:12px;font-weight:600;\">' +
                pct.toFixed(1) + '%%' + bar + '</div>';
       }", color_hex))
  }

  col_defs <- list(
    # Rank — medal emoji for top 3
    list(targets = 0, width = "40px", className = "dt-center",
         render = JS(
           "function(data, type, row) {
              if (type !== 'display') return data;
              if (data == 1) return '🥇';
              if (data == 2) return '🥈';
              if (data == 3) return '🥉';
              return data;
            }")),
    # Team name
    list(targets = 1, width = "160px"),
    # Group badge
    list(targets = 2, width = "55px", className = "dt-center",
         render = JS(
           "function(data) {
              return '<span style=\"background:rgba(126,200,32,0.18);color:#7EC820;' +
                     'border:1px solid rgba(126,200,32,0.4);border-radius:4px;' +
                     'padding:1px 7px;font-size:11px;font-weight:700;\">'+data+'</span>';
            }")),
    # ELO — coloured badge
    list(targets = 3, width = "70px", className = "dt-center",
         render = JS(
           "function(data) {
              var v = parseInt(data);
              var hue = Math.round((v - 1500) / 500 * 120);
              hue = Math.max(0, Math.min(120, hue));
              var col = 'hsl(' + hue + ',70%,45%)';
              return '<span style=\"color:' + col + ';font-weight:700;font-size:12px;\">' +
                     data + '</span>';
            }")),
    # Champion
    list(targets = 4,  render = bar_js("#F5C518")),
    # Runner-Up
    list(targets = 5,  render = bar_js("#A8A9AD")),
    # Third Place
    list(targets = 6,  render = bar_js("#CD7F32")),
    # Fourth Place
    list(targets = 7,  render = bar_js("#2979FF")),
    # Quarter-Final
    list(targets = 8,  render = bar_js("#D4C014")),
    # Round of 16
    list(targets = 9,  render = bar_js("#E09B1A")),
    # Round of 32
    list(targets = 10, render = bar_js("#E05B1A")),
    # Group Stage
    list(targets = 11, render = bar_js("#C8102E"))
  )

  # ── Champion % for row-level background colouring ──
  champ_idx <- which(names(dt_df) == "Champion") - 1L   # 0-based

  row_callback <- JS(sprintf(
    "function(row, data) {
       var pct = parseFloat(data[%d]) || 0;
       var alpha = Math.min(pct / 40, 0.55);
       $(row).css('background', 'rgba(245,197,24,' + alpha + ')');
     }", champ_idx))

  dt_widget <- datatable(
    dt_df,
    rownames    = FALSE,
    escape      = FALSE,
    filter      = "top",
    extensions  = c("Buttons", "FixedHeader", "Scroller"),
    options     = list(
      dom            = "Bfrtip",
      pageLength     = 48,
      scrollX        = TRUE,
      scrollY        = "620px",
      scroller       = TRUE,
      fixedHeader    = TRUE,
      autoWidth      = FALSE,
      order          = list(list(4, "desc")),   # sort by Champion % desc
      columnDefs     = col_defs,
      rowCallback    = row_callback,
      buttons        = list(
        list(extend = "csv",   text = "⬇ CSV",   filename = "wc2026_simulation"),
        list(extend = "excel", text = "⬇ Excel", filename = "wc2026_simulation"),
        list(extend = "copy",  text = "📋 Copy"),
        list(extend = "colvis", text = "Columns ▾")
      ),
      initComplete = JS(
        "function(settings, json) {
           $(this.api().table().header()).css({
             'background-color': '#22223A',
             'color': '#F5C518',
             'font-family': 'Inter, sans-serif',
             'font-size': '11px',
             'letter-spacing': '1px',
             'text-transform': 'uppercase'
           });
         }"
      )
    ),
    class = "display compact hover"
  ) %>%
    formatStyle(
      columns    = "Team",
      fontWeight = "600",
      fontSize   = "13px"
    ) %>%
    formatStyle(
      columns    = round_cols,
      textAlign  = "center"
    )

  # ── Build styled widget using prependContent (keeps it a true htmlwidget) ──
  styled_widget <- dt_widget %>%
    prependContent(
      tagList(
        tags$link(
          href = "https://fonts.googleapis.com/css2?family=Bebas+Neue&family=Inter:wght@300;400;600&display=swap",
          rel  = "stylesheet"),
        tags$style(HTML("
          body {
            background: #1A1A26 !important;
            color: #E8E8F0;
            font-family: 'Inter', sans-serif;
            padding: 28px 36px;
            margin: 0;
          }
          h1.wc-title {
            font-family: 'Bebas Neue', sans-serif;
            font-size: 42px; letter-spacing: 4px; color: #F5C518;
            margin: 0 0 4px;
            text-shadow: 0 0 40px rgba(126,200,32,0.3);
          }
          p.wc-subtitle {
            color: #7EC820; font-size: 12px; letter-spacing: 3px;
            text-transform: uppercase; margin-bottom: 16px;
          }
          .wc-legend { display: flex; gap: 18px; flex-wrap: wrap; margin-bottom: 16px; }
          .wc-legend-item { display: flex; align-items: center; gap: 6px; font-size: 11px; color: #6B6B80; }
          .wc-legend-dot { width: 10px; height: 10px; border-radius: 50%; flex-shrink: 0; }
          table.dataTable tbody tr             { background: #1A1A26 !important; color: #E8E8F0; }
          table.dataTable tbody tr:hover td    { background: rgba(255,255,255,0.05) !important; }
          table.dataTable tbody tr.odd  td     { background: #1E1E2E !important; }
          table.dataTable tbody tr.even td     { background: #1A1A26 !important; }
          .dataTables_wrapper .dataTables_filter input,
          .dataTables_wrapper .dataTables_length select {
            background: #22223A; border: 1px solid #32324A;
            color: #E8E8F0; border-radius: 5px; padding: 4px 8px;
          }
          .dataTables_wrapper .dataTables_info,
          .dataTables_wrapper .dataTables_paginate { color: #6B6B80; font-size: 12px; }
          .dataTables_wrapper .dataTables_paginate .paginate_button.current,
          .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
            background: #F5C518 !important; border-color: #F5C518 !important;
            color: #000 !important; border-radius: 4px;
          }
          .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
            background: rgba(245,197,24,0.15) !important;
            border-color: rgba(245,197,24,0.3) !important; color: #F5C518 !important;
          }
          .dt-buttons .btn {
            background: #22223A !important; border: 1px solid #32324A !important;
            color: #A0A0B0 !important; font-size: 12px !important;
            border-radius: 5px !important; padding: 5px 12px !important;
          }
          .dt-buttons .btn:hover {
            background: rgba(245,197,24,0.12) !important;
            border-color: rgba(245,197,24,0.4) !important; color: #F5C518 !important;
          }
          .dataTables_wrapper thead tr:last-child th input,
          .dataTables_wrapper thead tr:last-child th select {
            background: #0E0E18; border: 1px solid #32324A; color: #C8C8D8;
            border-radius: 4px; padding: 2px 5px; width: 100%;
          }
          ::-webkit-scrollbar { width: 8px; height: 8px; }
          ::-webkit-scrollbar-track { background: #1A1A26; }
          ::-webkit-scrollbar-thumb { background: #32324A; border-radius: 4px; }
          ::-webkit-scrollbar-thumb:hover { background: #F5C518; }
        ")),
        tags$h1(class = "wc-title", "\U0001f3c6 FIFA WORLD CUP 2026"),
        tags$p(class = "wc-subtitle",
               sprintf("Monte Carlo Distribution \u2014 %s simulations  \u2022  K-Factor = %d  \u2022  ELO-based",
                       formatC(N_SIMS, format = "d", big.mark = ","), K_FACTOR)),
        tags$div(class = "wc-legend",
                 tags$div(class="wc-legend-item", tags$div(class="wc-legend-dot",style="background:#F5C518"), "Champion"),
                 tags$div(class="wc-legend-item", tags$div(class="wc-legend-dot",style="background:#A8A9AD"), "Runner-Up"),
                 tags$div(class="wc-legend-item", tags$div(class="wc-legend-dot",style="background:#CD7F32"), "3rd Place"),
                 tags$div(class="wc-legend-item", tags$div(class="wc-legend-dot",style="background:#2979FF"), "4th Place"),
                 tags$div(class="wc-legend-item", tags$div(class="wc-legend-dot",style="background:#D4C014"), "QF"),
                 tags$div(class="wc-legend-item", tags$div(class="wc-legend-dot",style="background:#E09B1A"), "R16"),
                 tags$div(class="wc-legend-item", tags$div(class="wc-legend-dot",style="background:#E05B1A"), "R32"),
                 tags$div(class="wc-legend-item", tags$div(class="wc-legend-dot",style="background:#C8102E"), "Group Stage"),
                 tags$div(class="wc-legend-item", style="margin-left:auto;font-style:italic;",
                          "Row glow \u221d champion probability")
        )
      )
    )

  htmlwidgets::saveWidget(
    styled_widget,
    file          = out("wc2026_interactive_table.html"),
    selfcontained = TRUE,
    title         = "WC 2026 Simulation"
  )
  message(paste("\u2705 Saved:", out("wc2026_interactive_table.html")))

  # Also open in viewer if running interactively
  if (interactive()) print(styled_widget)


} else {
  message("ℹ️  Install DT + htmlwidgets to generate the interactive table:")
  message("   install.packages(c('DT', 'htmlwidgets'))")
}
