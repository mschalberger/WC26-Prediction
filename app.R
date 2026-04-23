# ============================================================
# FIFA World Cup 2026 — ELO Simulation Shiny App
# Install: install.packages(c("shiny", "dplyr", "DT"))
# Run:     shiny::runApp("app.R")
# ============================================================

library(shiny)
library(dplyr)
library(DT)

# ── DATA ─────────────────────────────────────────────────────

matches <- read.csv("data/matches.csv")
teams   <- read.csv("data/teams.csv")

elo_path       <- "data/cache/elo.tsv"
countries_path <- "data/cache/countries.tsv"

if (!file.exists(elo_path) || !file.exists(countries_path)) {
  stop("Cache-Dateien fehlen. Bitte update_elo.R manuell ausführen.")
}

elo       <- read.delim(elo_path,       sep="\t", header=FALSE)
countries <- read.delim(countries_path, sep="\t", header=FALSE)

elo <- elo %>%
  left_join(countries, by=c("V3"="V1")) %>%
  select(country=V2.y, elo=V4) %>%
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
  left_join(elo, by=c("team_name"="country"))

# Warn about any teams that didn't match the ELO source
missing <- teams_init %>% filter(is.na(elo)) %>% pull(team_name)
if (length(missing) > 0) {
  message("⚠️  No ELO found for: ", paste(missing, collapse=", "),
          " — substituting median ELO (", round(median(teams_init$elo, na.rm=TRUE)), ")")
  teams_init <- teams_init %>%
    mutate(elo = ifelse(is.na(elo), median(elo, na.rm=TRUE), elo))
}

# Flag emoji lookup
flag_map <- c(MEX="🇲🇽",RSA="🇿🇦",KOR="🇰🇷",UEPD="🇨🇿",CAN="🇨🇦",UEPA="🇧🇦",QAT="🇶🇦",
              SUI="🇨🇭",BRA="🇧🇷",MAR="🇲🇦",HAI="🇭🇹",SCO="🏴󠁧󠁢󠁳󠁣󠁴󠁿",USA="🇺🇸",PAR="🇵🇾",
              AUS="🇦🇺",UEPC="🇹🇷",GER="🇩🇪",CUR="🇨🇼",CIV="🇨🇮",ECU="🇪🇨",NED="🇳🇱",
              JPN="🇯🇵",UEPB="🇸🇪",TUN="🇹🇳",BEL="🇧🇪",EGY="🇪🇬",IRN="🇮🇷",NZL="🇳🇿",
              ESP="🇪🇸",CPV="🇨🇻",KSA="🇸🇦",URU="🇺🇾",FRA="🇫🇷",SEN="🇸🇳",FP02="🇮🇶",
              NOR="🇳🇴",ARG="🇦🇷",ALG="🇩🇿",AUT="🇦🇹",JOR="🇯🇴",POR="🇵🇹",FP01="🇨🇩",
              UZB="🇺🇿",COL="🇨🇴",ENG="🏴󠁧󠁢󠁥󠁮󠁧󠁿",CRO="🇭🇷",GHA="🇬🇭",PAN="🇵🇦")

get_flag <- function(code) { ifelse(is.na(flag_map[code]), "🏳️", flag_map[code]) }


hist_score_dist <- read.csv("data/score_dist.csv", stringsAsFactors = FALSE)

# Sample a score from historical distribution.
# p_fav  : favourite's ELO win probability (>= 0.5 always — caller must ensure this)
# outcome: "fav_win" | "draw" | "und_win"
# Returns list(fav_goals, und_goals)
sample_hist_score <- function(p_fav, sim_outcome) {
  if (is.null(hist_score_dist)) return(NULL)   # fallback signal

  # Map sim_outcome to the CSV's home/away convention
  # ("home" = favourite wins, "away" = underdog wins)
  csv_outcome <- switch(sim_outcome,
                        fav_win = "home",
                        und_win = "away",
                        "draw"          # draw stays draw
  )

  # Find bin — p_fav is always in [0.5, 1.0]; the top bin uses include.lowest
  bin_row <- hist_score_dist %>%
    filter(outcome == csv_outcome,
           p_lo <= p_fav, p_fav < p_hi)

  # Edge case: p_fav == 1.0 → use top bin
  if (nrow(bin_row) == 0) {
    bin_row <- hist_score_dist %>%
      filter(outcome == csv_outcome, p_hi == max(p_hi))
  }

  if (nrow(bin_row) == 0) return(NULL)   # fallback signal

  idx       <- sample(nrow(bin_row), 1, prob = bin_row$prob)
  fav_goals <- bin_row$home_goals[idx]   # "home" ≡ favourite in the CSV
  und_goals <- bin_row$away_goals[idx]
  list(fav_goals = fav_goals, und_goals = und_goals)
}

# ── ELO ENGINE ───────────────────────────────────────────────

elo_expected <- function(ea, eb) 1 / (1 + 10^((eb - ea) / 400))

simulate_match <- function(elo_home, elo_away, k = 20,
                           max_win_prob = 0.95,
                           use_historical = FALSE) {

  # Raw win probability
  p_h_raw <- elo_expected(elo_home, elo_away)

  # Apply max-win-probability cap (symmetric around 0.5)
  p_h <- if (p_h_raw > 0.5) {
    0.5 + min(p_h_raw - 0.5, max_win_prob - 0.5)
  } else {
    0.5 - min(0.5 - p_h_raw, max_win_prob - 0.5)
  }

  draw_p <- 1/3 * exp(-((p_h - .5)^2) / (2 * 0.236875^2))
  ph <- p_h * (1 - draw_p)
  pa <- (1 - p_h) * (1 - draw_p)

  outcome <- sample(c("home", "draw", "away"), 1, prob = c(ph, draw_p, pa))

  # ── Neutral-ground symmetry: work in favourite / underdog space ──
  # p_h >= 0.5  =>  home team is the favourite; otherwise away is.
  home_is_fav <- (p_h >= 0.5)
  p_fav       <- if (home_is_fav) p_h else (1 - p_h)

  # Translate raw outcome to favourite-space label used by the CSV lookup
  sim_outcome <- if (outcome == "draw") {
    "draw"
  } else if ((outcome == "home") == home_is_fav) {
    "fav_win"
  } else {
    "und_win"
  }

  used_historical <- FALSE
  if (use_historical) {
    sc <- sample_hist_score(p_fav, sim_outcome)
    if (!is.null(sc)) {
      # Map favourite/underdog goals back to the actual home/away teams
      gh <- if (home_is_fav) sc$fav_goals else sc$und_goals
      ga <- if (home_is_fav) sc$und_goals else sc$fav_goals
      used_historical <- TRUE
    }
  }

  if (!used_historical) {
    # ── Poisson score sampling (original / fallback) ───────
    lh <- 1.99419  * ph + 0.24629
    la <- 1.99419  * pa + 0.24629
    repeat {
      gh <- rpois(1, lh); ga <- rpois(1, la)
      if (outcome == "home" && gh > ga) break
      if (outcome == "away" && ga > gh) break
      if (outcome == "draw" && gh == ga) break
    }
  }

  act_h    <- ifelse(outcome == "home", 1, ifelse(outcome == "draw", 0.5, 0))
  exp_h    <- elo_expected(elo_home, elo_away)   # use raw p for ELO update
  goal_diff <- abs(gh - ga)
  k_mult   <- if (goal_diff <= 1) 1
  else if (goal_diff == 2) 1.5
  else if (goal_diff == 3) 1.75
  else 1.75 + (goal_diff - 3) / 8
  k_adj <- k * k_mult

  list(home_goals    = gh,
       away_goals    = ga,
       outcome       = outcome,
       new_elo_home  = elo_home + k_adj * (act_h - exp_h),
       new_elo_away  = elo_away + k_adj * ((1 - act_h) - (1 - exp_h)))
}

# ── GROUP STAGE ──────────────────────────────────────────────

run_group_stage <- function(teams_df, k = 20,
                            max_win_prob = 0.95,
                            use_historical = FALSE) {
  elo_start <- setNames(teams_df$elo, teams_df$id)
  elo_live  <- elo_start

  all_matches   <- data.frame()
  all_standings <- data.frame()

  for (grp in sort(unique(teams_df$group_letter))) {
    ids   <- teams_df %>% filter(group_letter == grp) %>% pull(id)
    pairs <- combn(ids, 2, simplify = FALSE)
    pts <- gf <- ga <- setNames(rep(0, 4), ids)

    for (pair in pairs) {
      h <- pair[1]; a <- pair[2]

      elo_h <- elo_live[as.character(h)]
      elo_a <- elo_live[as.character(a)]

      res <- simulate_match(elo_h, elo_a, k = k,
                            max_win_prob   = max_win_prob,
                            use_historical = use_historical)

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

      ht <- teams_df %>% filter(id == h)
      at <- teams_df %>% filter(id == a)
      all_matches <- rbind(all_matches, data.frame(
        stage  = "Group", group = grp,
        home   = paste(get_flag(ht$fifa_code), ht$team_name),
        away   = paste(get_flag(at$fifa_code), at$team_name),
        score  = paste0(res$home_goals, "-", res$away_goals),
        result = res$outcome, stringsAsFactors = FALSE))
    }

    standing <- data.frame(id = ids, pts = as.numeric(pts),
                           gf = as.numeric(gf), ga = as.numeric(ga)) %>%
      mutate(gd = gf - ga, elo = elo_live[as.character(ids)]) %>%
      arrange(desc(pts), desc(gd), desc(gf), desc(elo)) %>%
      mutate(rank = 1:4, group = grp) %>%
      left_join(teams_df %>% select(id, team_name, fifa_code), by = "id")
    all_standings <- rbind(all_standings, standing)
  }
  list(standings = all_standings, matches = all_matches, elo_live = elo_live)
}

# ── KNOCKOUT ─────────────────────────────────────────────────

sim_ko_match <- function(id_a, id_b, elo_live, teams_df,
                         round_name, k = 20,
                         max_win_prob = 0.95,
                         use_historical = FALSE) {
  elo_h <- elo_live[as.character(id_a)]
  elo_a <- elo_live[as.character(id_b)]

  res  <- simulate_match(elo_h, elo_a, k = k,
                         max_win_prob   = max_win_prob,
                         use_historical = use_historical)
  pens <- ""
  if (res$outcome == "draw") {
    pa     <- elo_expected(elo_h, elo_a)
    winner <- ifelse(runif(1) < pa, id_a, id_b)
    pens   <- " (pens)"
  } else {
    winner <- ifelse(res$outcome == "home", id_a, id_b)
  }
  loser <- ifelse(winner == id_a, id_b, id_a)

  elo_live[as.character(id_a)] <- res$new_elo_home
  elo_live[as.character(id_b)] <- res$new_elo_away

  ta <- teams_df %>% filter(id == id_a)
  tb <- teams_df %>% filter(id == id_b)
  tw <- teams_df %>% filter(id == winner)
  list(winner = winner, loser = loser, elo_live = elo_live,
       row = data.frame(stage  = round_name, group = "",
                        home   = paste(get_flag(ta$fifa_code), ta$team_name),
                        away   = paste(get_flag(tb$fifa_code), tb$team_name),
                        score  = paste0(res$home_goals, "-", res$away_goals, pens),
                        result = paste("→", get_flag(tw$fifa_code), tw$team_name),
                        stringsAsFactors = FALSE))
}

run_knockout <- function(pairs, elo_live, teams_df,
                         round_name, k = 20,
                         max_win_prob = 0.95,
                         use_historical = FALSE) {
  winners <- c(); losers <- c(); rows <- data.frame()
  for (pair in pairs) {
    res      <- sim_ko_match(pair[1], pair[2], elo_live, teams_df,
                             round_name, k = k,
                             max_win_prob   = max_win_prob,
                             use_historical = use_historical)
    elo_live <- res$elo_live
    winners  <- c(winners, res$winner)
    losers   <- c(losers,  res$loser)
    rows     <- rbind(rows, res$row)
  }
  list(winners = winners, losers = losers, elo_live = elo_live, matches = rows)
}

# ── TOURNAMENT ───────────────────────────────────────────────

run_tournament <- function(seed = NULL, k = 20,
                           max_win_prob = 0.95,
                           use_historical = FALSE) {
  if (!is.null(seed)) set.seed(seed)
  teams_df <- teams_init

  gs       <- run_group_stage(teams_df, k = k,
                              max_win_prob   = max_win_prob,
                              use_historical = use_historical)
  elo_live <- gs$elo_live
  std      <- gs$standings

  thirds <- std %>% filter(rank == 3) %>%
    arrange(desc(pts), desc(gd), desc(gf)) %>% slice(1:8)

  get_t <- function(grp, rnk) std %>% filter(group == grp, rank == rnk) %>% pull(id)

  r32_pairs <- list(
    c(get_t("A",2), get_t("B",2)),
    c(get_t("C",1), get_t("F",2)),
    c(get_t("E",1), thirds$id[1]),
    c(get_t("F",1), get_t("C",2)),
    c(get_t("E",2), get_t("I",2)),
    c(get_t("I",1), thirds$id[2]),
    c(get_t("A",1), thirds$id[3]),
    c(get_t("L",1), thirds$id[4]),
    c(get_t("G",1), thirds$id[5]),
    c(get_t("D",1), thirds$id[6]),
    c(get_t("H",1), get_t("J",2)),
    c(get_t("K",2), get_t("L",2)),
    c(get_t("B",1), thirds$id[7]),
    c(get_t("D",2), get_t("G",2)),
    c(get_t("J",1), get_t("H",2)),
    c(get_t("K",1), thirds$id[8])
  )

  ko_args <- list(elo_live = elo_live, teams_df = teams_df,
                  k = k, max_win_prob = max_win_prob,
                  use_historical = use_historical)

  r32 <- do.call(run_knockout, c(list(pairs = r32_pairs, round_name = "Round of 32"),   ko_args))
  ko_args$elo_live <- r32$elo_live
  r16_pairs <- lapply(seq(1,15,2), function(i) c(r32$winners[i], r32$winners[i+1]))
  r16 <- do.call(run_knockout, c(list(pairs = r16_pairs, round_name = "Round of 16"),   ko_args))
  ko_args$elo_live <- r16$elo_live
  qf_pairs  <- lapply(seq(1,7,2),  function(i) c(r16$winners[i], r16$winners[i+1]))
  qf  <- do.call(run_knockout, c(list(pairs = qf_pairs,  round_name = "Quarter-Final"), ko_args))
  ko_args$elo_live <- qf$elo_live
  sf_pairs  <- list(c(qf$winners[1], qf$winners[2]), c(qf$winners[3], qf$winners[4]))
  sf  <- do.call(run_knockout, c(list(pairs = sf_pairs,  round_name = "Semi-Final"),    ko_args))
  ko_args$elo_live <- sf$elo_live
  tp  <- do.call(run_knockout, c(list(pairs = list(sf$losers), round_name = "Third Place"), ko_args))
  ko_args$elo_live <- tp$elo_live
  fin <- do.call(run_knockout, c(list(pairs = list(c(sf$winners[1], sf$winners[2])),
                                      round_name = "Final"), ko_args))
  elo_live <- fin$elo_live

  champion <- teams_df %>% filter(id == fin$winners[1])
  runner   <- teams_df %>% filter(id == fin$losers[1])
  third    <- teams_df %>% filter(id == tp$winners[1])

  all_ko_matches <- bind_rows(r32$matches, r16$matches, qf$matches,
                              sf$matches, tp$matches, fin$matches)

  final_elo <- data.frame(
    id        = as.integer(names(elo_live)),
    final_elo = as.numeric(elo_live)
  ) %>%
    left_join(teams_df %>% select(id, team_name, fifa_code, elo), by = "id") %>%
    mutate(
      change    = round(final_elo - elo),
      final_elo = round(final_elo),
      start_elo = round(elo),
      flag      = sapply(fifa_code, get_flag)
    ) %>%
    arrange(desc(final_elo)) %>%
    mutate(rank = row_number())

  list(standings = std, group_matches = gs$matches, ko_matches = all_ko_matches,
       champion = champion, runner_up = runner, third = third,
       final_elo = final_elo)
}


# ── UI ───────────────────────────────────────────────────────

ui <- fluidPage(
  tags$head(
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
    tags$link(href="https://fonts.googleapis.com/css2?family=Source+Sans+3:wght@300;400;600;700&display=swap", rel="stylesheet"),
    tags$style(HTML("
      /* ══ DARK MODE (default) ══ */
      :root {
        --fublue:  #004659;
        --fugreen: #CCFF00;
        --fublack: #000000;
        --fuwhite: #FFFFFF;
        --body:    #000000;
        --gold:    #CCFF00;
        --lime:    #CCFF00;
        --navy:    #222222;
        --red:     #C8102E;
        --dark:    #000000;
        --panel:   #111111;
        --border:  #333333;
        --text:    #FFFFFF;
        --muted:   #888888;
        --green:   #00C853;
        --blue:    #2979FF;
        --input-bg:          #111111;
        --group-card-bg:     #111111;
        --group-header-bg:   #CCFF00;
        --qualify-bg:        transparent;
        --qualify3-bg:       transparent;
        --ko-border:         rgba(255,255,255,0.08);
        --elo-bar-bg:        rgba(255,255,255,0.08);
        --rank4-bg:          transparent;
        --rank3-bg:          transparent;
      }
      /* ══ LIGHT MODE ══ */
      body.light-mode {
        --body:            #FFFFFF;
        --dark:            #FFFFFF;
        --panel:           #FFFFFF;
        --border:          #E0E0E0;
        --text:            #000000;
        --muted:           #888888;
        --gold:            #000000;
        --lime:            #000000;
        --input-bg:        #F8F8F8;
        --group-card-bg:   #FFFFFF;
        --group-header-bg: rgba(0,0,0,0.04);
        --qualify-bg:      transparent;
        --qualify3-bg:     transparent;
        --ko-border:       rgba(0,0,0,0.06);
        --elo-bar-bg:      rgba(0,0,0,0.07);
        --rank4-bg:        transparent;
        --rank3-bg:        transparent;
      }

      * { box-sizing: border-box; }
      body {
        background: var(--dark); color: var(--text);
        font-family: 'Source Sans 3', Arial, sans-serif; font-size: 14px; margin: 0; padding: 0;
        transition: background 0.3s, color 0.3s;
      }

      /* ── HEADER ── */
      .wc-header {
        background: #000000;
        border-bottom: 3px solid #CCFF00;
        padding: 20px 40px; position: relative; overflow: hidden;
      }
      body.light-mode .wc-header {
        background: #FFFFFF;
        border-bottom: 3px solid #CCFF00;
      }
      .wc-header::before {
        content: ''; position: absolute; inset: 0;
        background: repeating-linear-gradient(
          45deg, transparent, transparent 40px,
          rgba(245,197,24,0.03) 40px, rgba(245,197,24,0.03) 41px);
        pointer-events: none;
      }
      .wc-title {
        font-family: 'Source Sans 3', Arial, sans-serif; font-size: 52px; font-weight: 700;
        letter-spacing: 4px; color: var(--fuwhite); line-height: 1; margin: 0;
        text-shadow: 0 0 40px rgba(126,200,32,0.35);
      }
      .wc-subtitle {
        color: #FFFFFF; font-size: 12px; font-weight: 300;
        letter-spacing: 3px; text-transform: uppercase; margin-top: 4px;
      }
      body.light-mode .wc-title { color: #000000; text-shadow: none; }
      body.light-mode .wc-subtitle { color: #000000; }
      .header-inner {
        display: flex; align-items: center; justify-content: space-between;
        flex-wrap: wrap; gap: 16px;
      }
      .header-branding { display: flex; flex-direction: column; }
      .fustat-logo {
        height: 44px; opacity: 0.92;
        filter: drop-shadow(0 0 8px rgba(126,200,32,0.3));
        transition: opacity 0.2s, filter 0.2s;
      }
      .fustat-logo:hover { opacity: 1; filter: drop-shadow(0 0 14px rgba(126,200,32,0.55)); }

      /* ── THEME TOGGLE ── */
      .theme-toggle {
        display: flex; align-items: center; gap: 10px;
        background: #CCFF00; border: 1px solid #CCFF00;
        border-radius: 24px; padding: 6px 14px; cursor: pointer;
        transition: background 0.2s, border-color 0.2s;
        user-select: none;
      }
      body.light-mode .theme-toggle {
        display: flex; align-items: center; gap: 10px;
        background: #FFFFFF; border: 1px solid rgba(255,255,255,0.15);
        border-radius: 24px; padding: 6px 14px; cursor: pointer;
        transition: background 0.2s, border-color 0.2s;
        user-select: none;
      }
      .toggle-label {
        font-family: 'Source Sans 3', Arial, sans-serif; font-size: 14px; font-weight: 700;
        letter-spacing: 2px; color: #000000;
      }
      .toggle-track {
        position: relative; width: 42px; height: 22px;
        background: #32324A; border-radius: 11px;
        transition: background 0.3s;
        flex-shrink: 0;
      }
      body.light-mode .toggle-track { background: #CCFF00; border: 1.5px solid #CCFF00; }
      body.light-mode .toggle-track.on { background: #CCFF00; border: 1.5px solid #CCFF00; }
      .toggle-track.on { background: var(--gold); }
      .toggle-thumb {
        position: absolute; top: 3px; left: 3px;
        width: 16px; height: 16px; border-radius: 50%;
        background: #fff; transition: transform 0.3s;
        box-shadow: 0 1px 4px rgba(0,0,0,0.3);
      }
      body.light-mode .toggle-thumb {
        position: absolute; top: 3px; left: 3px;
        width: 16px; height: 16px; border-radius: 50%;
        background: #000000; transition: transform 0.3s;
        box-shadow: 0 1px 4px rgba(0,0,0,0.3);
      }
      .toggle-track.on .toggle-thumb { transform: translateX(20px); }
      .toggle-icon { font-size: 16px; }

      /* ── LAYOUT ── */
      .wc-body { padding: 24px 32px; }

      /* ── CONTROLS ── */
      .control-bar {
        display: flex; align-items: flex-end; gap: 28px; flex-wrap: wrap;
        background: var(--panel); border: 1px solid var(--border);
        border-radius: 10px; padding: 16px 20px; margin-bottom: 24px;
        transition: background 0.3s, border-color 0.3s;
      }
      .control-group { display: flex; flex-direction: column; gap: 6px; }
      .control-label {
        color: var(--muted); font-size: 10px; letter-spacing: 1.5px;
        text-transform: uppercase; font-weight: 400;
      }
      body:not(.light-mode) .control-label { color: #FFFFFF; }
      #seed {
        background: var(--input-bg); border: 1px solid var(--border); color: var(--text);
        border-radius: 6px; padding: 8px 12px; width: 120px;
        font-family: monospace; font-size: 14px;
        transition: background 0.3s, border-color 0.3s, color 0.3s;
      }
      body:not(.light-mode) #seed { background: #000000; color: #FFFFFF; }
      body:not(.light-mode) #seed::placeholder { color: #FFFFFF; }
      #run_btn {
        background: var(--gold); color: #000; border: none;
        font-family: 'Source Sans 3', Arial, sans-serif; font-weight: 700; font-size: 18px; letter-spacing: 2px;
        padding: 10px 28px; border-radius: 6px; cursor: pointer;
        transition: all 0.2s; box-shadow: 0 0 20px rgba(245,197,24,0.3);
      }
      #run_btn:hover { background: #FFFFFF; color: #000000; box-shadow: none; transform: translateY(-1px); }
      body.light-mode #run_btn { background: #CCFF00; color: #000000; box-shadow: none; border: none; }
      body.light-mode #run_btn:hover { background: #000000; color: #CCFF00; box-shadow: none; transform: translateY(-1px); }

      /* ── K SLIDER (Shiny) ── */
      #k_slider.js-range-slider { background: transparent; }
      .irs--shiny .irs-single { background: transparent; border: none; color: #FFFFFF; font-size: 12px; padding: 2px 4px; top: -2px; }
      .irs--shiny .irs-bar { background: #CCFF00; border: none; }
      .irs--shiny .irs-handle { background: #CCFF00; border: 2px solid #CCFF00; box-shadow: none; }
      .irs--shiny .irs-handle:hover { background: #CCFF00; }
      .irs--shiny .irs-bar--single { border-left: 1px solid var(--gold); }
      .irs--shiny .irs-line { background: var(--border); border: none; }
      body.light-mode .irs--shiny .irs-single { background: #FFFFFF; border: none; color: #000000; font-size: 12px; padding: 2px 4px; top: -2px; }
      body.light-mode .irs--shiny .irs-bar { background: #CCFF00; border: none; }
      body.light-mode .irs--shiny .irs-handle { background: #000000; border: 2px solid #000000; box-shadow: none; }
      body.light-mode .irs--shiny .irs-handle:hover { background: #000000; }
      .irs--shiny .irs-min, .irs--shiny .irs-max { background: var(--panel); color: var(--muted); font-size: 11px; }
      .irs-with-grid { margin-bottom: 0 !important; }
      .form-group { margin-bottom: 0 !important; }

      /* ── SCORE MODE TOGGLE ── */
      .score-mode-wrap {
        display: flex; flex-direction: column; gap: 6px;
      }
      .score-mode-toggle {
        display: flex; align-items: center; gap: 10px; cursor: pointer; user-select: none;
      }
      .score-mode-track {
        position: relative; width: 42px; height: 22px;
        background: #32324A; border-radius: 11px; transition: background 0.3s; flex-shrink: 0;
      }
      .score-mode-track.on { background: var(--gold); }
      body.light-mode .score-mode-track { background: #CCCC00; }
      body.light-mode .score-mode-track.on { background: #000000; }
      .score-mode-thumb {
        position: absolute; top: 3px; left: 3px;
        width: 16px; height: 16px; border-radius: 50%;
        background: #fff; transition: transform 0.3s;
        box-shadow: 0 1px 4px rgba(0,0,0,0.3);
      }
      body.light-mode .score-mode-thumb { background: #fff; }
      .score-mode-track.on .score-mode-thumb { transform: translateX(20px); }
      .score-mode-label {
        font-family: 'Source Sans 3', Arial, sans-serif; font-size: 13px; font-weight: 600;
        letter-spacing: 1px; color: var(--text); min-width: 80px;
      }
      /* badge shown next to current mode */
      .mode-badge {
        font-size: 10px; font-weight: 700; letter-spacing: 1.5px;
        padding: 2px 7px; border-radius: 4px; text-transform: uppercase;
        background: var(--gold); color: #000;
      }
      body.light-mode .mode-badge { background: #CCFF00; color: #000; }

      /* ── PODIUM ── */
      .podium { display: flex; gap: 12px; margin-bottom: 24px; }
      .podium-card {
        flex: 1; background: var(--panel); border: 2px solid var(--border);
        border-radius: 12px; padding: 20px; text-align: center;
        transition: background 0.3s, border-color 0.3s;
      }
      .podium-card.first  { border-color: #D4AF37; box-shadow: 0 0 30px rgba(245,197,24,0.15); }
      .podium-card.second { border-color: #C0C0C0; }
      .podium-card.third  { border-color: #B08D57; }
      .podium-label {
        font-family: 'Source Sans 3', Arial, sans-serif; font-weight: 700; font-size: 13px;
        letter-spacing: 3px; color: var(--muted); margin-bottom: 8px;
      }
      .podium-flag  { font-size: 40px; display: block; margin-bottom: 6px; }
      .podium-team  { font-family: 'Source Sans 3', Arial, sans-serif; font-weight: 700; font-size: 22px; letter-spacing: 1px; }
      .podium-card.first  .podium-team { color: #D4AF37; }
      .podium-card.second .podium-team { color: #C0C0C0; }
      .podium-card.third  .podium-team { color: #B08D57; }
      .trophy { font-size: 28px; }

      /* ── TABS ── */
      .nav-tabs { border-bottom: 2px solid var(--border); margin-bottom: 0; }
      .nav-tabs > li > a {
        background: transparent; border: none; color: var(--muted);
        font-family: 'Source Sans 3', Arial, sans-serif; font-weight: 700; font-size: 14px; letter-spacing: 1px;
        padding: 10px 20px; border-radius: 0; transition: color 0.2s; text-transform: uppercase;
      }
      .nav-tabs > li > a:hover { color: var(--text); background: transparent; border: none; }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background: transparent; border: none;
        border-bottom: 3px solid var(--gold) !important;
        color: var(--gold); margin-bottom: -2px;
      }
      body.light-mode .nav-tabs > li.active > a,
      body.light-mode .nav-tabs > li.active > a:hover,
      body.light-mode .nav-tabs > li.active > a:focus {
        color: #000000;
        border-bottom: 3px solid #CCFF00 !important;
      }
      .tab-content {
        background: var(--panel); border: 1px solid var(--border);
        border-top: none; border-radius: 0 0 10px 10px; padding: 20px;
        transition: background 0.3s, border-color 0.3s;
      }

      /* ── GROUPS ── */
      .groups-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(320px,1fr)); gap: 16px; }
      .group-card  { background: var(--group-card-bg); border: 1px solid var(--border); border-radius: 10px; overflow: hidden; transition: background 0.3s; }
      body.light-mode .group-card { border: 1.5px solid #000000; }
      .group-header {
        background: var(--group-header-bg);
        border-bottom: 1px solid var(--border); padding: 10px 16px;
        font-family: 'Source Sans 3', Arial, sans-serif; font-weight: 700; font-size: 16px; letter-spacing: 2px; color: var(--lime);
        text-transform: uppercase;
      }
      body:not(.light-mode) .group-header { color: #000000; }
      body.light-mode .group-header { color: #000000; background: #CCFF00; border-bottom: 1px solid #000000; }
      .group-table { width: 100%; border-collapse: collapse; }
      .group-table th {
        color: var(--muted); font-size: 10px; letter-spacing: 1px; text-transform: uppercase;
        padding: 6px 12px; text-align: right; font-weight: 400;
      }
      body:not(.light-mode) .group-table th { color: #F8F8F8; }
      .group-table th:first-child { text-align: left; }
      .group-table td { padding: 8px 12px; border-top: 1px solid var(--ko-border); text-align: right; }
      .group-table td:first-child { text-align: left; }
      .group-table tr:first-child td { border-top: none; }
      .qualify     { background: var(--qualify-bg); }
      .qualify-3rd { background: var(--qualify3-bg); }
      .rank-badge {
        display: inline-flex; align-items: center; justify-content: center;
        width: 20px; height: 20px; border-radius: 50%; font-size: 11px; font-weight: 600; margin-right: 6px;
      }
      .rank-1 { background: #FFD700; color: #000000; }
      .rank-2 { background: #C0C0C0; color: #000000; }
      .rank-3 { background: #CD7F32; color: #000000; border: none; }
      .rank-4 { background: #222222; color: #888888; }
      body.light-mode .rank-1 { background: #FFD700; color: #000000; }
      body.light-mode .rank-2 { background: #C0C0C0; color: #000000; }
      body.light-mode .rank-3 { background: #CD7F32; color: #000000; border: none; }
      body.light-mode .rank-4 { background: #F0F0F0; color: #555555; }
      .pts-cell { font-weight: 600; color: var(--gold); }
      body.light-mode .pts-cell { color: #000000; }
      .gd-pos { color: var(--green); }
      .gd-neg { color: var(--red); }
      body.light-mode .gd-pos { color: #007a30; }

      /* ── KO ── */
      .ko-section { margin-bottom: 28px; }
      .ko-round-title {
        font-family: 'Source Sans 3', Arial, sans-serif; font-weight: 700; font-size: 18px; letter-spacing: 2px; color: var(--lime);
        border-left: 4px solid var(--lime); padding-left: 12px; margin-bottom: 12px; text-transform: uppercase;
      }
      body.light-mode .ko-round-title { color: #000000; border-color: #CCFF00; }
      .ko-table { width: 100%; border-collapse: collapse; }
      .ko-table th {
        color: var(--muted); font-size: 10px; letter-spacing: 1px; text-transform: uppercase;
        padding: 8px 14px; text-align: left; border-bottom: 1px solid var(--border); font-weight: 400;
      }
      .ko-table td { padding: 9px 14px; border-bottom: 1px solid var(--ko-border); }
      .ko-table tr:last-child td { border-bottom: none; }
      .ko-score  { font-family: monospace; font-size: 15px; font-weight: 600; color: var(--gold); text-align: center; }
      body.light-mode .ko-score { color: #000000; }
      .ko-winner { color: #CCFF00; font-weight: 600; }
      body.light-mode .ko-winner { color: #007a30; }

      /* ── ELO ── */
      .elo-bar-wrap { background: var(--elo-bar-bg); border-radius: 3px; height: 6px; width: 120px; }
      .elo-bar { background: linear-gradient(90deg, var(--navy), var(--lime)); height: 6px; border-radius: 3px; }
      .elo-up { color: var(--green); }
      body.light-mode .elo-up { color: #007a30; }
      .elo-dn { color: var(--red); }

      /* ── SPINNER ── */
      .loading-overlay {
        display: none; position: fixed; inset: 0; background: rgba(10,10,15,0.85);
        z-index: 9999; align-items: center; justify-content: center; flex-direction: column; gap: 16px;
      }
      .loading-overlay.active { display: flex; }
      .spinner {
        width: 48px; height: 48px; border: 4px solid rgba(245,197,24,0.2);
        border-top-color: var(--gold); border-radius: 50%; animation: spin 0.8s linear infinite;
      }
      @keyframes spin { to { transform: rotate(360deg); } }
      .loading-text { font-family: 'Source Sans 3', Arial, sans-serif; font-weight: 700; font-size: 22px; letter-spacing: 4px; color: var(--gold); }

      /* ── MISC ── */
      select.form-control { background: var(--input-bg); border: 1px solid var(--border); color: var(--text); border-radius: 6px; }
      .shiny-output-error { color: var(--red); }

      /* ── MOBILE ── */
      @media (max-width: 600px) {
        .wc-header { padding: 12px 16px; }
        .header-inner { flex-direction: column; align-items: flex-start; gap: 10px; }
        .wc-title { font-size: 28px; letter-spacing: 2px; }
        .wc-subtitle { font-size: 10px; }
        .fustat-logo { height: 28px; }
        .Alumni-logo { height: 28px; }
        div[style*='gap:20px'] { flex-wrap: wrap; gap: 8px !important; }
        .wc-body { padding: 12px; }
        .control-bar { flex-direction: column; align-items: stretch; gap: 14px; }
        #run_btn { width: 100%; }
        #seed { width: 100%; }
        .podium { flex-direction: column; gap: 8px; }
        .groups-grid { grid-template-columns: 1fr; }
        .nav-tabs > li > a { font-size: 11px; padding: 8px 8px; letter-spacing: 0; }
        .group-table td, .group-table th { padding: 5px 6px; font-size: 12px; }
        .ko-table td, .ko-table th { padding: 5px 8px; font-size: 12px; }
        .elo-bar-wrap { display: none; }
        .tab-content { padding: 12px; }
        .podium { flex-direction: column; gap: 8px; }
        .podium-card.first  { order: 1; }
        .podium-card.second { order: 2; }
        .podium-card.third  { order: 3; }
      }
    "))
  ),

  div(class="loading-overlay", id="loader",
      div(class="spinner"),
      div(class="loading-text", "Simulating Tournament...")
  ),

  div(class="wc-header",
      div(class="header-inner",
          div(class="header-branding",
              p(class="wc-subtitle", "ELO-Based Monte Carlo Simulator"),
              h1(class="wc-title", "🏆 FIFA World Cup 2026")
          ),
          div(style="display:flex; align-items:center; gap:20px;",
              div(class="theme-toggle", id="theme_toggle", onclick="toggleTheme()",
                  span(class="toggle-icon", id="theme_icon", "🌙"),
                  div(class="toggle-track", id="toggle_track",
                      div(class="toggle-thumb")
                  ),
                  span(class="toggle-label", id="theme_label", "LIGHT MODE")
              ),
              tags$img(src="FUStatBlueOnGreen.png",    id="fustat-logo", class="fustat-logo", alt="FUSTAT",  style="height:55px;"),
              tags$img(src="WiWiss-Alumni.png",                          class="Alumni-logo", alt="Alumni",  style="height:55px;"),
              tags$img(src="StuKoLogoLight_Trans.png", id="stuko-logo",  class="fustat-logo", alt="StuKo",   style="height:55px;")
          )
      )
  ),

  div(class="wc-body",

      div(class="control-bar",

          # Seed
          div(class="control-group",
              div(class="control-label", "Random Seed"),
              tags$input(id="seed", type="number", value="", min="1", max="99999",
                         class="form-control", placeholder="random")
          ),

          # K-Factor slider
          div(class="control-group",
              div(class="control-label", "K-Factor"),
              sliderInput("k_slider", label=NULL, min=0, max=60, value=20, step=5,
                          ticks=FALSE, width="200px")
          ),

          # ── NEW: Max Win Probability slider ──────────────────────
          div(class="control-group",
              div(class="control-label", "Max Win Probability"),
              sliderInput("max_win_prob", label=NULL,
                          min=0.50, max=1.00, value=0.95, step=0.01,
                          ticks=FALSE, width="220px")
          ),

          # ── NEW: Score mode toggle (Poisson / Historical) ─────────
          div(class="control-group score-mode-wrap",
              div(class="control-label", "Score Model"),
              div(class="score-mode-toggle", id="score_mode_toggle",
                  onclick="toggleScoreMode()",
                  div(class="score-mode-track", id="score_mode_track",
                      div(class="score-mode-thumb")
                  ),
                  span(class="score-mode-label", id="score_mode_label", "Poisson"),
                  span(class="mode-badge",        id="score_mode_badge", "ACTIVE")
              ),
              # Hidden input read by Shiny
              tags$input(type="hidden", id="use_historical", value="0")
          ),

          actionButton("run_btn", "▶  SIMULATE", class="btn")
      ),

      uiOutput("podium_ui"),

      tabsetPanel(id="main_tabs",
                  tabPanel("🏅 Group Standings", div(style="margin-top:16px;", uiOutput("groups_ui"))),
                  tabPanel("⚽ Group Matches",   div(style="margin-top:16px;", uiOutput("group_matches_ui"))),
                  tabPanel("⚔️ Knockout Stage",  div(style="margin-top:16px;", uiOutput("ko_ui"))),
                  tabPanel("📊 ELO Rankings",    div(style="margin-top:16px;", uiOutput("elo_ui")))
      )
  ),

  tags$script(HTML("
    /* ── Theme ── */
    var lightMode = true;
    document.addEventListener('DOMContentLoaded', function() {
      document.body.classList.add('light-mode');
      document.getElementById('toggle_track').classList.add('on');
      document.getElementById('theme_icon').textContent  = '☀️';
      document.getElementById('theme_label').textContent = '';
    });

    function toggleTheme() {
      lightMode = !lightMode;
      var body  = document.body;
      var track = document.getElementById('toggle_track');
      var icon  = document.getElementById('theme_icon');
      var label = document.getElementById('theme_label');
      if (lightMode) {
        body.classList.add('light-mode');
        track.classList.add('on');
        icon.textContent  = '☀️';
        label.textContent = '';
        document.getElementById('fustat-logo').src = 'FUStatBlueOnGreen.png';
        document.getElementById('stuko-logo').src  = 'StuKoLogoLight_Trans.png';
      } else {
        body.classList.remove('light-mode');
        track.classList.remove('on');
        icon.textContent  = '🌙';
        label.textContent = '';
        document.getElementById('fustat-logo').src = 'FUStatBlueOnGreen_DARK.png';
        document.getElementById('stuko-logo').src  = 'StuKoLogoDark_Trans.png';
      }
    }

    /* ── Score mode ── */
    var useHistorical = false;
    function toggleScoreMode() {
      useHistorical = !useHistorical;
      var track = document.getElementById('score_mode_track');
      var label = document.getElementById('score_mode_label');
      var badge = document.getElementById('score_mode_badge');
      var input = document.getElementById('use_historical');
      if (useHistorical) {
        track.classList.add('on');
        label.textContent = 'Historical';
        badge.textContent = 'ACTIVE';
        input.value = '1';
      } else {
        track.classList.remove('on');
        label.textContent = 'Poisson';
        badge.textContent = 'ACTIVE';
        input.value = '0';
      }
      // notify Shiny
      Shiny.setInputValue('use_historical', input.value, {priority: 'event'});
    }

    /* ── Spinner ── */
    $(document).on('click', '#run_btn', function() {
      $('#loader').addClass('active');
    });
    $(document).on('shiny:idle', function() {
      $('#loader').removeClass('active');
    });
  "))
)

# ── SERVER ───────────────────────────────────────────────────

server <- function(input, output, session) {
  result <- reactiveVal(NULL)

  # Run on startup with defaults
  observe({ result(run_tournament(seed=123, k=20, max_win_prob=0.95, use_historical=FALSE)) })

  observeEvent(input$run_btn, {
    seed <- suppressWarnings(as.integer(input$seed))
    if (is.na(seed)) seed <- sample(1:99999, 1)

    k_val        <- as.integer(input$k_slider %||% 20)
    mwp          <- as.numeric(input$max_win_prob %||% 0.95)
    use_hist     <- isTRUE(input$use_historical == "1")

    result(run_tournament(seed           = seed,
                          k              = k_val,
                          max_win_prob   = mwp,
                          use_historical = use_hist))
  })

  # ── Podium ──
  output$podium_ui <- renderUI({
    r <- result(); req(r)
    champ  <- r$champion; runner <- r$runner_up; third <- r$third

    make_card <- function(team, cls, label, trophy) {
      elo_val <- r$final_elo %>% filter(id == team$id) %>% pull(final_elo)
      div(class=paste("podium-card", cls),
          div(class="podium-label", label),
          tags$span(class="trophy", trophy),
          tags$span(class="podium-flag", get_flag(team$fifa_code)),
          div(class="podium-team", team$team_name),
          div(style="color:var(--muted);font-size:11px;margin-top:4px;",
              paste("ELO:", round(elo_val)))
      )
    }

    tagList(div(class="podium",
                make_card(runner, "second", "Runner-Up",   "🥈"),
                make_card(champ,  "first",  "Champion",    "🏆"),
                make_card(third,  "third",  "Third Place", "🥉")
    ))
  })

  # ── Group Standings ──
  output$groups_ui <- renderUI({
    r <- result(); req(r)
    std <- r$standings
    cards <- lapply(sort(unique(std$group)), function(grp) {
      gd <- std %>% filter(group == grp) %>% arrange(rank)
      rows <- lapply(1:nrow(gd), function(i) {
        row    <- gd[i,]
        cls    <- if (i <= 2) "qualify" else if (i == 3) "qualify-3rd" else ""
        gd_val <- row$gd
        gd_cls <- if (gd_val > 0) "gd-pos" else if (gd_val < 0) "gd-neg" else ""
        gd_str <- if (gd_val > 0) paste0("+", gd_val) else as.character(gd_val)
        tags$tr(class=cls,
                tags$td(tags$span(class=paste("rank-badge", paste0("rank-", i)), i),
                        get_flag(row$fifa_code), " ", row$team_name),
                tags$td(class="pts-cell", row$pts),
                tags$td(row$gf), tags$td(row$ga),
                tags$td(class=gd_cls, gd_str),
                tags$td(style="color:var(--muted);font-size:12px;", round(row$elo))
        )
      })
      div(class="group-card",
          div(class="group-header", paste("GROUP", grp)),
          tags$table(class="group-table",
                     tags$thead(tags$tr(
                       tags$th("Team"), tags$th("Pts"), tags$th("GF"),
                       tags$th("GA"), tags$th("GD"), tags$th("ELO")
                     )),
                     tags$tbody(rows)
          )
      )
    })
    div(class="groups-grid", cards)
  })

  # ── Group Matches ──
  output$group_matches_ui <- renderUI({
    r <- result(); req(r)
    gm <- r$group_matches
    sections <- lapply(sort(unique(gm$group)), function(grp) {
      ms   <- gm %>% filter(group == grp)
      rows <- lapply(1:nrow(ms), function(i) {
        m <- ms[i,]
        tags$tr(tags$td(m$home), tags$td(class="ko-score", m$score), tags$td(m$away))
      })
      div(class="ko-section",
          div(class="ko-round-title", paste("Group", grp)),
          tags$table(class="ko-table",
                     tags$thead(tags$tr(
                       tags$th("Home"), tags$th(style="text-align:center","Score"), tags$th("Away")
                     )),
                     tags$tbody(rows)
          )
      )
    })
    div(sections)
  })

  # ── Knockout Stage ──
  output$ko_ui <- renderUI({
    r <- result(); req(r)
    ko     <- r$ko_matches
    rounds <- c("Round of 32","Round of 16","Quarter-Final","Semi-Final","Third Place","Final")
    sections <- lapply(rounds, function(rnd) {
      ms <- ko %>% filter(stage == rnd)
      if (nrow(ms) == 0) return(NULL)
      rows <- lapply(1:nrow(ms), function(i) {
        m <- ms[i,]
        tags$tr(tags$td(m$home), tags$td(class="ko-score", m$score),
                tags$td(m$away), tags$td(class="ko-winner", m$result))
      })
      div(class="ko-section",
          div(class="ko-round-title", rnd),
          tags$table(class="ko-table",
                     tags$thead(tags$tr(
                       tags$th("Home / Team A"), tags$th(style="text-align:center","Score"),
                       tags$th("Away / Team B"), tags$th("Winner")
                     )),
                     tags$tbody(rows)
          )
      )
    })
    div(sections)
  })

  # ── ELO Rankings ──
  output$elo_ui <- renderUI({
    r <- result(); req(r)
    fe      <- r$final_elo
    max_elo <- max(fe$final_elo)

    rows <- lapply(1:nrow(fe), function(i) {
      row     <- fe[i,]
      chg     <- row$change
      chg_cls <- if (chg > 0) "elo-up" else if (chg < 0) "elo-dn" else ""
      chg_str <- if (chg > 0) paste0("+", chg) else as.character(chg)
      bar_pct <- round(100 * row$final_elo / max_elo)
      medal   <- if (i == 1) "🥇" else if (i == 2) "🥈" else if (i == 3) "🥉" else as.character(i)
      tags$tr(
        tags$td(medal),
        tags$td(paste(row$flag, row$team_name)),
        tags$td(style="font-family:monospace;font-weight:600;color:var(--gold);", row$final_elo),
        tags$td(class=chg_cls, style="font-family:monospace;", chg_str),
        tags$td(style="font-family:monospace;color:var(--muted);", round(row$start_elo)),
        tags$td(div(class="elo-bar-wrap",
                    div(class="elo-bar", style=paste0("width:", bar_pct, "%"))))
      )
    })

    tags$table(class="ko-table",
               tags$thead(tags$tr(
                 tags$th("#"), tags$th("Team"), tags$th("Final ELO"),
                 tags$th("Change"), tags$th("Start ELO"), tags$th("Strength")
               )),
               tags$tbody(rows)
    )
  })
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

shinyApp(ui, server)
