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

elo       <- read.delim("https://www.eloratings.net/World.tsv?_=1772102421796",      sep="\t", header=FALSE)
countries <- read.delim("https://www.eloratings.net/en.teams.tsv?_=1772102421794",   sep="\t", header=FALSE)

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
    team_name == "Winner FIFA Playoff 2" ~ "Bolivia",
    team_name == "Winner UEFA Playoff B" ~ "Poland",
    team_name == "Winner UEFA Playoff C" ~ "Turkey",
    team_name == "Winner UEFA Playoff A" ~ "Italy",
    team_name == "Winner UEFA Playoff D" ~ "Denmark",
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
flag_map <- c(MEX="🇲🇽",RSA="🇿🇦",KOR="🇰🇷",UEPD="🇩🇰",CAN="🇨🇦",UEPA="🇮🇹",QAT="🇶🇦",
              SUI="🇨🇭",BRA="🇧🇷",MAR="🇲🇦",HAI="🇭🇹",SCO="🏴󠁧󠁢󠁳󠁣󠁴󠁿",USA="🇺🇸",PAR="🇵🇾",
              AUS="🇦🇺",UEPC="🇹🇷",GER="🇩🇪",CUR="🇨🇼",CIV="🇨🇮",ECU="🇪🇨",NED="🇳🇱",
              JPN="🇯🇵",UEPB="🇵🇱",TUN="🇹🇳",BEL="🇧🇪",EGY="🇪🇬",IRN="🇮🇷",NZL="🇳🇿",
              ESP="🇪🇸",CPV="🇨🇻",KSA="🇸🇦",URU="🇺🇾",FRA="🇫🇷",SEN="🇸🇳",FP02="🇧🇴",
              NOR="🇳🇴",ARG="🇦🇷",ALG="🇩🇿",AUT="🇦🇹",JOR="🇯🇴",POR="🇵🇹",FP01="🇨🇩",
              UZB="🇺🇿",COL="🇨🇴",ENG="🏴󠁧󠁢󠁥󠁮󠁧󠁿",CRO="🇭🇷",GHA="🇬🇭",PAN="🇵🇦")

get_flag <- function(code) { ifelse(is.na(flag_map[code]), "🏳️", flag_map[code]) }

# ── ELO ENGINE ───────────────────────────────────────────────

elo_expected <- function(ea, eb) 1 / (1 + 10^((eb - ea) / 400))

simulate_match <- function(elo_home, elo_away, k=20) {
  p_h    <- elo_expected(elo_home, elo_away)
  draw_p <- 1/3 * exp(-((p_h - .5)^2) / (2 * 0.28^2))
  ph <- p_h * (1 - draw_p); pa <- (1 - p_h) * (1 - draw_p)
  outcome <- sample(c("home","draw","away"), 1, prob=c(ph, draw_p, pa))
  lh <- 1.8 * ph + 0.27
  la <- 1.8 * pa + 0.27
  repeat {
    gh <- rpois(1, lh); ga <- rpois(1, la)
    if (outcome=="home" && gh > ga) break
    if (outcome=="away" && ga > gh) break
    if (outcome=="draw" && gh == ga) break
  }
  act_h <- ifelse(outcome=="home", 1, ifelse(outcome=="draw", 0.5, 0))
  exp_h <- elo_expected(elo_home, elo_away)
  goal_diff <- abs(gh - ga)
  k_mult <- if (goal_diff <= 1) 1
  else if (goal_diff == 2) 1.5
  else if (goal_diff == 3) 1.75
  else 1.75 + (goal_diff - 3) / 8
  k_adj <- k * k_mult
  list(home_goals=gh, away_goals=ga, outcome=outcome,
       new_elo_home=elo_home + k_adj * (act_h - exp_h),
       new_elo_away=elo_away + k_adj * ((1 - act_h) - (1 - exp_h)))
}

# ── GROUP STAGE ──────────────────────────────────────────────

run_group_stage <- function(teams_df, k=20) {
  elo_start <- setNames(teams_df$elo, teams_df$id)
  elo_live  <- elo_start

  all_matches   <- data.frame()
  all_standings <- data.frame()

  for (grp in sort(unique(teams_df$group_letter))) {
    ids   <- teams_df %>% filter(group_letter==grp) %>% pull(id)
    pairs <- combn(ids, 2, simplify=FALSE)
    pts <- gf <- ga <- setNames(rep(0, 4), ids)

    for (pair in pairs) {
      h <- pair[1]; a <- pair[2]

      elo_h <- elo_live[as.character(h)]
      elo_a <- elo_live[as.character(a)]

      res <- simulate_match(elo_h, elo_a, k=k)

      elo_live[as.character(h)] <- res$new_elo_home
      elo_live[as.character(a)] <- res$new_elo_away

      gf[as.character(h)] <- gf[as.character(h)] + res$home_goals
      ga[as.character(h)] <- ga[as.character(h)] + res$away_goals
      gf[as.character(a)] <- gf[as.character(a)] + res$away_goals
      ga[as.character(a)] <- ga[as.character(a)] + res$home_goals

      if      (res$outcome=="home") pts[as.character(h)] <- pts[as.character(h)] + 3
      else if (res$outcome=="away") pts[as.character(a)] <- pts[as.character(a)] + 3
      else {
        pts[as.character(h)] <- pts[as.character(h)] + 1
        pts[as.character(a)] <- pts[as.character(a)] + 1
      }

      ht <- teams_df %>% filter(id==h)
      at <- teams_df %>% filter(id==a)
      all_matches <- rbind(all_matches, data.frame(
        stage="Group", group=grp,
        home=paste(get_flag(ht$fifa_code), ht$team_name),
        away=paste(get_flag(at$fifa_code), at$team_name),
        score=paste0(res$home_goals, "-", res$away_goals),
        result=res$outcome, stringsAsFactors=FALSE))
    }

    standing <- data.frame(id=ids, pts=as.numeric(pts),
                           gf=as.numeric(gf), ga=as.numeric(ga)) %>%
      mutate(gd=gf-ga, elo=elo_live[as.character(ids)]) %>%
      arrange(desc(pts), desc(gd), desc(gf), desc(elo)) %>%
      mutate(rank=1:4, group=grp) %>%
      left_join(teams_df %>% select(id, team_name, fifa_code), by="id")
    all_standings <- rbind(all_standings, standing)
  }
  list(standings=all_standings, matches=all_matches,
       elo_live=elo_live)
}

# ── KNOCKOUT ─────────────────────────────────────────────────

sim_ko_match <- function(id_a, id_b, elo_live, teams_df,
                         round_name, k=20) {
  elo_h <- elo_live[as.character(id_a)]
  elo_a <- elo_live[as.character(id_b)]

  res  <- simulate_match(elo_h, elo_a, k=k)
  pens <- ""
  if (res$outcome=="draw") {
    pa     <- elo_expected(elo_h, elo_a)
    winner <- ifelse(runif(1) < pa, id_a, id_b)
    pens   <- " (pens)"
  } else {
    winner <- ifelse(res$outcome=="home", id_a, id_b)
  }
  loser <- ifelse(winner==id_a, id_b, id_a)

  elo_live[as.character(id_a)] <- res$new_elo_home
  elo_live[as.character(id_b)] <- res$new_elo_away

  ta <- teams_df %>% filter(id==id_a)
  tb <- teams_df %>% filter(id==id_b)
  tw <- teams_df %>% filter(id==winner)
  list(winner=winner, loser=loser, elo_live=elo_live,
       row=data.frame(stage=round_name, group="",
                      home=paste(get_flag(ta$fifa_code), ta$team_name),
                      away=paste(get_flag(tb$fifa_code), tb$team_name),
                      score=paste0(res$home_goals, "-", res$away_goals, pens),
                      result=paste("→", get_flag(tw$fifa_code), tw$team_name),
                      stringsAsFactors=FALSE))
}

run_knockout <- function(pairs, elo_live, teams_df,
                         round_name, k=20) {
  winners <- c(); losers <- c(); rows <- data.frame()
  for (pair in pairs) {
    res      <- sim_ko_match(pair[1], pair[2], elo_live, teams_df, round_name, k=k)
    elo_live <- res$elo_live
    winners  <- c(winners, res$winner)
    losers   <- c(losers,  res$loser)
    rows     <- rbind(rows, res$row)
  }
  list(winners=winners, losers=losers, elo_live=elo_live, matches=rows)
}

# ── TOURNAMENT ───────────────────────────────────────────────

run_tournament <- function(seed=NULL, k=20) {
  if (!is.null(seed)) set.seed(seed)
  teams_df <- teams_init

  gs        <- run_group_stage(teams_df, k=k)
  elo_live  <- gs$elo_live
  std       <- gs$standings

  thirds <- std %>% filter(rank==3) %>%
    arrange(desc(pts), desc(gd), desc(gf)) %>% slice(1:8)

  get_t <- function(grp, rnk) std %>% filter(group==grp, rank==rnk) %>% pull(id)

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

  r32 <- run_knockout(r32_pairs, elo_live, teams_df, "Round of 32",   k=k)
  elo_live <- r32$elo_live
  r16_pairs <- lapply(seq(1,15,2), function(i) c(r32$winners[i], r32$winners[i+1]))
  r16 <- run_knockout(r16_pairs, elo_live, teams_df, "Round of 16",   k=k)
  elo_live <- r16$elo_live
  qf_pairs  <- lapply(seq(1,7,2),  function(i) c(r16$winners[i], r16$winners[i+1]))
  qf  <- run_knockout(qf_pairs,  elo_live, teams_df, "Quarter-Final", k=k)
  elo_live <- qf$elo_live
  sf_pairs  <- list(c(qf$winners[1], qf$winners[2]), c(qf$winners[3], qf$winners[4]))
  sf  <- run_knockout(sf_pairs,  elo_live, teams_df, "Semi-Final",    k=k)
  elo_live <- sf$elo_live
  tp  <- run_knockout(list(sf$losers), elo_live, teams_df, "Third Place",   k=k)
  elo_live <- tp$elo_live
  fin <- run_knockout(list(c(sf$winners[1], sf$winners[2])), elo_live, teams_df, "Final", k=k)
  elo_live <- fin$elo_live

  champion <- teams_df %>% filter(id==fin$winners[1])
  runner   <- teams_df %>% filter(id==fin$losers[1])
  third    <- teams_df %>% filter(id==tp$winners[1])

  all_ko_matches <- bind_rows(r32$matches, r16$matches, qf$matches,
                              sf$matches, tp$matches, fin$matches)

  final_elo <- data.frame(
    id        = as.integer(names(elo_live)),
    final_elo = as.numeric(elo_live)
  ) %>%
    left_join(teams_df %>% select(id, team_name, fifa_code, elo), by="id") %>%
    mutate(
      change    = round(final_elo - elo),
      final_elo = round(final_elo),
      start_elo = round(elo),
      flag      = sapply(fifa_code, get_flag)
    ) %>%
    arrange(desc(final_elo)) %>%
    mutate(rank=row_number())

  list(standings=std, group_matches=gs$matches, ko_matches=all_ko_matches,
       champion=champion, runner_up=runner, third=third,
       final_elo=final_elo)
}


# ── UI ───────────────────────────────────────────────────────

ui <- fluidPage(
  tags$head(
    tags$link(href="https://fonts.googleapis.com/css2?family=Bebas+Neue&family=Inter:wght@300;400;600&display=swap", rel="stylesheet"),
    tags$style(HTML("
      /* ══ DARK MODE (default) ══ */
      :root {
        --fublue: #004659;
        --fugreen: #CCFF00;
        --fublack: #000000;
        --fuwhite: #FFFFFF;
        # --gold:   #F5C518;
        --gold:   #CCFF00;
        --lime:   #7EC820;
        --navy:   #1A5276;
        --red:    #C8102E;
        # --dark:   #1A1A26;
        --dark:   #000000;
        # --panel:  #22223A;
        --panel:  #004659;
        --border: #32324A;
        --text:   #E8E8F0;
        --muted:  #6B6B80;
        --green:  #00C853;
        --blue:   #2979FF;
        --input-bg: #1C1C2A;
        --group-card-bg: #0E0E18;
        --group-header-bg: rgba(26,82,118,0.35);
        --qualify-bg: rgba(0,200,83,0.08);
        --qualify3-bg: rgba(41,121,255,0.06);
        --ko-border: rgba(255,255,255,0.04);
        --elo-bar-bg: rgba(255,255,255,0.05);
        --rank4-bg: rgba(255,255,255,0.05);
        --rank3-bg: rgba(41,121,255,0.3);
      }

      /* ══ LIGHT MODE ══ */
      body.light-mode {
        # --dark:   #F0F2F5;
        --dark:   #CCFF00;
        # --panel:  #FFFFFF;
        --panel:  #FFFFFF;
        # --gold:   #004659;
        --border: #D0D4DC;
        --text:   #1A1A2E;
        --muted:  #6B7280;
        --input-bg: #F8F9FB;
        --group-card-bg: #F8F9FB;
        --group-header-bg: rgba(26,82,118,0.08);
        --qualify-bg: rgba(0,150,60,0.07);
        --qualify3-bg: rgba(41,121,255,0.07);
        --ko-border: rgba(0,0,0,0.06);
        --elo-bar-bg: rgba(0,0,0,0.07);
        --rank4-bg: rgba(0,0,0,0.06);
        --rank3-bg: rgba(41,121,255,0.15);
      }

      * { box-sizing: border-box; }
      body {
        background: var(--dark); color: var(--text);
        font-family: 'Inter', sans-serif; font-size: 14px; margin: 0; padding: 0;
        transition: background 0.3s, color 0.3s;
      }

      /* ── HEADER ── */
      .wc-header {
        background: linear-gradient(135deg, #1A2235 0%, #1C2D4A 50%, #1A2235 100%);
        border-bottom: 3px solid var(--lime);
        padding: 20px 40px; position: relative; overflow: hidden;
      }
      body.light-mode .wc-header {
        # background: linear-gradient(135deg, #1C2D4A 0%, #203860 50%, #1C2D4A 100%);
        # background: linear-gradient(135deg, #FFFFFF 0%, #FFFFFF 50%, #FFFFFF 100%);
      }
      .wc-header::before {
        content: ''; position: absolute; inset: 0;
        background: repeating-linear-gradient(
          45deg, transparent, transparent 40px,
          rgba(245,197,24,0.03) 40px, rgba(245,197,24,0.03) 41px);
        pointer-events: none;
      }
      .wc-title {
        font-family: 'Bebas Neue', sans-serif; font-size: 52px;
        letter-spacing: 4px; color: var(--gold); line-height: 1; margin: 0;
        text-shadow: 0 0 40px rgba(126,200,32,0.35);
      }
      .wc-subtitle {
        color: rgba(200,200,220,0.7); font-size: 12px; font-weight: 300;
        letter-spacing: 3px; text-transform: uppercase; margin-top: 4px;
      }
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
        background: rgba(255,255,255,0.08); border: 1px solid rgba(255,255,255,0.15);
        border-radius: 24px; padding: 6px 14px; cursor: pointer;
        transition: background 0.2s, border-color 0.2s;
        user-select: none;
      }
      .theme-toggle:hover { background: rgba(255,255,255,0.14); }
      .toggle-label {
        font-family: 'Bebas Neue', sans-serif; font-size: 14px;
        letter-spacing: 2px; color: rgba(220,220,240,0.85);
      }
      .toggle-track {
        position: relative; width: 42px; height: 22px;
        background: #32324A; border-radius: 11px;
        transition: background 0.3s;
        flex-shrink: 0;
      }
      .toggle-track.on { background: var(--gold); }
      .toggle-thumb {
        position: absolute; top: 3px; left: 3px;
        width: 16px; height: 16px; border-radius: 50%;
        background: #fff; transition: transform 0.3s;
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
      #seed {
        background: var(--input-bg); border: 1px solid var(--border); color: var(--text);
        border-radius: 6px; padding: 8px 12px; width: 120px;
        font-family: monospace; font-size: 14px;
        transition: background 0.3s, border-color 0.3s, color 0.3s;
      }
      #run_btn {
        background: var(--gold); color: #000; border: none;
        font-family: 'Bebas Neue', sans-serif; font-size: 18px; letter-spacing: 2px;
        padding: 10px 28px; border-radius: 6px; cursor: pointer;
        transition: all 0.2s; box-shadow: 0 0 20px rgba(245,197,24,0.3);
      }
      #run_btn:hover { background: #FFD740; box-shadow: 0 0 30px rgba(245,197,24,0.5); transform: translateY(-1px); }

      /* ── K SLIDER (Shiny) ── */
      #k_slider.js-range-slider { background: transparent; }
      .irs--shiny .irs-bar { background: var(--gold); border-top: 1px solid var(--gold); border-bottom: 1px solid var(--gold); }
      .irs--shiny .irs-bar--single { border-left: 1px solid var(--gold); }
      .irs--shiny .irs-line { background: var(--border); border: none; }
      .irs--shiny .irs-handle { background: var(--gold); border: 2px solid var(--gold); box-shadow: 0 0 6px rgba(245,197,24,0.5); }
      .irs--shiny .irs-handle:hover, .irs--shiny .irs-handle:active { background: #FFD740; }
      .irs--shiny .irs-single { background: var(--panel); border: 1px solid var(--gold); color: var(--gold); font-family: 'Bebas Neue', sans-serif; font-size: 14px; letter-spacing: 1px; }
      .irs--shiny .irs-min, .irs--shiny .irs-max { background: var(--panel); color: var(--muted); font-size: 11px; }
      .irs-with-grid { margin-bottom: 0 !important; }
      .form-group { margin-bottom: 0 !important; }

      /* ── PODIUM ── */
      .podium { display: flex; gap: 12px; margin-bottom: 24px; }
      .podium-card {
        flex: 1; background: var(--panel); border: 1px solid var(--border);
        border-radius: 12px; padding: 20px; text-align: center;
        transition: background 0.3s, border-color 0.3s;
      }
      .podium-card.first  { border-color: var(--gold); box-shadow: 0 0 30px rgba(245,197,24,0.15); }
      .podium-card.second { border-color: #A8A9AD; }
      .podium-card.third  { border-color: #CD7F32; }
      .podium-label {
        font-family: 'Bebas Neue', sans-serif; font-size: 13px;
        letter-spacing: 3px; color: var(--muted); margin-bottom: 8px;
      }
      .podium-flag  { font-size: 40px; display: block; margin-bottom: 6px; }
      .podium-team  { font-family: 'Bebas Neue', sans-serif; font-size: 22px; letter-spacing: 1px; }
      .podium-card.first  .podium-team { color: var(--gold); }
      .podium-card.second .podium-team { color: #A8A9AD; }
      .podium-card.third  .podium-team { color: #CD7F32; }
      .trophy { font-size: 28px; }

      /* ── TABS ── */
      .nav-tabs { border-bottom: 2px solid var(--border); margin-bottom: 0; }
      .nav-tabs > li > a {
        background: transparent; border: none; color: var(--muted);
        font-family: 'Bebas Neue', sans-serif; font-size: 16px; letter-spacing: 2px;
        padding: 10px 20px; border-radius: 0; transition: color 0.2s;
      }
      .nav-tabs > li > a:hover { color: var(--text); background: transparent; border: none; }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background: transparent; border: none;
        border-bottom: 3px solid var(--gold) !important;
        color: var(--gold); margin-bottom: -2px;
      }
      .tab-content {
        background: var(--panel); border: 1px solid var(--border);
        border-top: none; border-radius: 0 0 10px 10px; padding: 20px;
        transition: background 0.3s, border-color 0.3s;
      }

      /* ── GROUPS ── */
      .groups-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(320px,1fr)); gap: 16px; }
      .group-card  { background: var(--group-card-bg); border: 1px solid var(--border); border-radius: 10px; overflow: hidden; transition: background 0.3s; }
      .group-header {
        background: var(--group-header-bg);
        border-bottom: 1px solid var(--border); padding: 10px 16px;
        font-family: 'Bebas Neue', sans-serif; font-size: 18px; letter-spacing: 3px; color: var(--lime);
      }
      body.light-mode .group-header { color: #2a7a00; }
      .group-table { width: 100%; border-collapse: collapse; }
      .group-table th {
        color: var(--muted); font-size: 10px; letter-spacing: 1px; text-transform: uppercase;
        padding: 6px 12px; text-align: right; font-weight: 400;
      }
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
      .rank-1 { background: var(--gold); color: #000; }
      .rank-2 { background: #A8A9AD; color: #000; }
      .rank-3 { background: var(--rank3-bg); color: var(--blue); border: 1px solid var(--blue); }
      .rank-4 { background: var(--rank4-bg); color: var(--muted); }
      .pts-cell { font-weight: 600; color: var(--gold); }
      body.light-mode .pts-cell { color: #b8860b; }
      .gd-pos { color: var(--green); }
      body.light-mode .gd-pos { color: #007a30; }
      .gd-neg { color: var(--red); }

      /* ── KO ── */
      .ko-section { margin-bottom: 28px; }
      .ko-round-title {
        font-family: 'Bebas Neue', sans-serif; font-size: 20px; letter-spacing: 3px; color: var(--lime);
        border-left: 4px solid var(--lime); padding-left: 12px; margin-bottom: 12px;
      }
      body.light-mode .ko-round-title { color: #2a7a00; border-color: #2a7a00; }
      .ko-table { width: 100%; border-collapse: collapse; }
      .ko-table th {
        color: var(--muted); font-size: 10px; letter-spacing: 1px; text-transform: uppercase;
        padding: 8px 14px; text-align: left; border-bottom: 1px solid var(--border); font-weight: 400;
      }
      .ko-table td { padding: 9px 14px; border-bottom: 1px solid var(--ko-border); }
      .ko-table tr:last-child td { border-bottom: none; }
      .ko-score  { font-family: monospace; font-size: 15px; font-weight: 600; color: var(--gold); text-align: center; }
      body.light-mode .ko-score { color: #b8860b; }
      .ko-winner { color: var(--green); font-weight: 600; }
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
      .loading-text { font-family: 'Bebas Neue', sans-serif; font-size: 22px; letter-spacing: 4px; color: var(--gold); }

      /* ── MISC ── */
      select.form-control { background: var(--input-bg); border: 1px solid var(--border); color: var(--text); border-radius: 6px; }
      .shiny-output-error { color: var(--red); }
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
              # ── LIGHT MODE TOGGLE ──
              div(class="theme-toggle", id="theme_toggle", onclick="toggleTheme()",
                  span(class="toggle-icon", id="theme_icon", "🌙"),
                  div(class="toggle-track", id="toggle_track",
                      div(class="toggle-thumb")
                  ),
                  span(class="toggle-label", id="theme_label", "LIGHT MODE")
              ),
              tags$img(src="FUStatWhiteOnGreen.svg",
                       class="fustat-logo", alt="FUSTAT",
                       style="height:55px;"),
              tags$img(src="WiWiss-Alumni.png",
                       class="Alumni-logo", alt="Alumni",
                       style="height:55px;"),
              tags$img(src="StuKoLogoDark_Trans.png",
                       class="fustat-logo", alt="StuKo",
                       style="height:55px;")
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

          # K slider
          div(class="control-group",
              div(class="control-label", "K-Factor"),
              sliderInput("k_slider", label=NULL, min=0, max=60, value=20, step=5, ticks=FALSE, width="200px")
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
    var lightMode = false;

    function toggleTheme() {
      lightMode = !lightMode;
      var body       = document.body;
      var track      = document.getElementById('toggle_track');
      var icon       = document.getElementById('theme_icon');
      var label      = document.getElementById('theme_label');

      if (lightMode) {
        body.classList.add('light-mode');
        track.classList.add('on');
        icon.textContent  = '☀️';
        label.textContent = 'DARK MODE';
      } else {
        body.classList.remove('light-mode');
        track.classList.remove('on');
        icon.textContent  = '🌙';
        label.textContent = 'LIGHT MODE';
      }
    }

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

  observe({ result(run_tournament(seed=123, k=20)) })

  observeEvent(input$run_btn, {
    seed  <- suppressWarnings(as.integer(input$seed))
    if (is.na(seed)) seed <- sample(1:99999, 1)
    k_val <- as.integer(input$k_slider %||% 20)
    result(run_tournament(seed=seed, k=k_val))
  })

  # ── Podium ──
  output$podium_ui <- renderUI({
    r <- result(); req(r)
    champ  <- r$champion; runner <- r$runner_up; third <- r$third

    make_card <- function(team, cls, label, trophy) {
      elo_val <- r$final_elo %>% filter(id==team$id) %>% pull(final_elo)
      div(class=paste("podium-card", cls),
          div(class="podium-label", label),
          tags$span(class="trophy", trophy),
          tags$span(class="podium-flag", get_flag(team$fifa_code)),
          div(class="podium-team", team$team_name),
          div(style="color:var(--muted);font-size:11px;margin-top:4px;",
              paste("ELO:", round(elo_val)))
      )
    }

    tagList(
      div(class="podium",
          make_card(runner, "second", "Runner-Up",   "🥈"),
          make_card(champ,  "first",  "Champion",    "🏆"),
          make_card(third,  "third",  "Third Place", "🥉")
      )
    )
  })

  # ── Group Standings ──
  output$groups_ui <- renderUI({
    r <- result(); req(r)
    std <- r$standings
    cards <- lapply(sort(unique(std$group)), function(grp) {
      gd <- std %>% filter(group==grp) %>% arrange(rank)
      rows <- lapply(1:nrow(gd), function(i) {
        row    <- gd[i,]
        cls    <- if (i<=2) "qualify" else if (i==3) "qualify-3rd" else ""
        gd_val <- row$gd
        gd_cls <- if (gd_val>0) "gd-pos" else if (gd_val<0) "gd-neg" else ""
        gd_str <- if (gd_val>0) paste0("+",gd_val) else as.character(gd_val)
        tags$tr(class=cls,
                tags$td(tags$span(class=paste("rank-badge",paste0("rank-",i)), i),
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
      ms   <- gm %>% filter(group==grp)
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
      ms <- ko %>% filter(stage==rnd)
      if (nrow(ms)==0) return(NULL)
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
    elo_col <- "Start ELO"

    rows <- lapply(1:nrow(fe), function(i) {
      row     <- fe[i,]
      chg     <- row$change
      chg_cls <- if (chg>0) "elo-up" else if (chg<0) "elo-dn" else ""
      chg_str <- if (chg>0) paste0("+",chg) else as.character(chg)
      bar_pct <- round(100 * row$final_elo / max_elo)
      medal   <- if (i==1) "🥇" else if (i==2) "🥈" else if (i==3) "🥉" else as.character(i)
      tags$tr(
        tags$td(medal),
        tags$td(paste(row$flag, row$team_name)),
        tags$td(style="font-family:monospace;font-weight:600;color:var(--gold);", row$final_elo),
        tags$td(class=chg_cls, style="font-family:monospace;", chg_str),
        tags$td(style="font-family:monospace;color:var(--muted);", round(row$start_elo)),
        tags$td(div(class="elo-bar-wrap",
                    div(class="elo-bar", style=paste0("width:",bar_pct,"%"))))
      )
    })

    tags$table(class="ko-table",
               tags$thead(tags$tr(
                 tags$th("#"), tags$th("Team"), tags$th("Final ELO"),
                 tags$th("Change"), tags$th(elo_col), tags$th("Strength")
               )),
               tags$tbody(rows)
    )
  })
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

shinyApp(ui, server)
