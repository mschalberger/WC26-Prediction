# ============================================================
# FIFA World Cup 2026 — ELO Simulation Shiny App
# Install: install.packages(c("shiny", "dplyr", "DT"))
# Run:     shiny::runApp("app.R")
# ============================================================

library(shiny)
library(dplyr)
library(DT)
library(future)
library(promises)

# Worker-Anzahl: Auf dem Produktions-Server (erkannt am /srv/shiny-server-Pfad)
# hardcoded 12 — systemd-Environment wird durch su --login geschluckt,
# .Renviron ist fehleranfällig im Pfad. ENV-Variable hat trotzdem Vorrang,
# falls man später ohne Code-Änderung umstellen will.
# Lokal: dynamisch (detectCores - 1), damit Entwickler-Maschinen
# nicht ihre gesamte CPU für die App verbrennen.
n_workers <- local({
  env_val <- Sys.getenv("WM2026_WORKERS", unset = "")
  if (nzchar(env_val)) {
    as.integer(env_val)
  } else if (dir.exists("/srv/shiny-server")) {
    12L   # Produktions-Server
  } else {
    max(1L, parallel::detectCores() - 1L)   # Lokale Entwicklung
  }
})
future::plan(future::multisession, workers = n_workers)
message("future plan: multisession mit ", n_workers, " Worker(n)")


# Null-coalescing helper: gibt a zurück, wenn nicht-NULL und nicht-leer,
# sonst b. Muss VOR der ersten Verwendung definiert sein (wird sowohl
# beim UI-Aufbau als auch im Server-Code benutzt).
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ── SESSION-TRACKING ─────────────────────────────────────────
# Loggt Start- und Endzeitpunkt jeder Shiny-Session als TSV-Datei.
# Aus den beiden Events (event = "start" / "end") lassen sich später
# sowohl die Gesamtzahl der Besuche als auch Concurrency-Kurven
# (gleichzeitige Nutzer:innen über die Zeit) rekonstruieren.
# Fehler beim Loggen werden abgefangen, damit die App auch bei
# fehlender Schreibberechtigung / vollem Volume normal weiterläuft.
session_log_file <- "../data/logs/sessions.tsv"
# Log-Verzeichnis idempotent anlegen (lokal nicht vorhanden, Server schon).
# Fehlschlag (z.B. read-only-FS) wird stillschweigend ignoriert.
tryCatch(
  dir.create(dirname(session_log_file), recursive = TRUE, showWarnings = FALSE),
  error = function(e) invisible(NULL)
)
tryCatch({
  if (!dir.exists(dirname(session_log_file))) {
    dir.create(dirname(session_log_file), recursive = TRUE, showWarnings = FALSE)
  }
  if (!file.exists(session_log_file)) {
    writeLines("timestamp\tevent\tsession_token", session_log_file)
  }
}, error = function(e) {
  message("Session-Log konnte nicht initialisiert werden: ", conditionMessage(e))
})

log_session_event <- function(event, token) {
  tryCatch({
    line <- paste(
      format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"),
      event,
      token,
      sep = "\t"
    )
    cat(line, "\n", file = session_log_file, append = TRUE, sep = "")
  }, error = function(e) {
    message("Session-Log-Fehler: ", conditionMessage(e))
  })
}

# ── DATA ─────────────────────────────────────────────────────

matches <- read.csv("../data/matches.csv")
teams   <- read.csv("../data/teams.csv")

elo_path       <- "../data/cache/elo.tsv"
countries_path <- "../data/cache/countries.tsv"

if (!file.exists(elo_path) || !file.exists(countries_path)) {
  stop("Cache-Dateien fehlen. Bitte update_elo.R manuell ausführen.")
}

elo       <- read.delim(elo_path,       sep="\t", header=FALSE)
countries <- read.delim(countries_path, sep="\t", header=FALSE)

##### PLAYED MATCHES (fixed results) #####
results_path <- "../data/cache/results.tsv"

load_played_results <- function() {
  if (file.exists(results_path)) {
    read.delim(results_path, sep="\t", header=TRUE, stringsAsFactors=FALSE)
  } else {
    data.frame(stage=character(), home_id=integer(), away_id=integer(),
               home_goals=integer(), away_goals=integer(),
               pens_winner_id=integer(), stringsAsFactors=FALSE)
  }
}
played_results <- load_played_results()
# Order-insensitive lookup: matches (h,a) and (a,h) the same way.
lookup_result <- function(stage_name, id_a, id_b) {
  hit <- played_results[
    played_results$stage == stage_name &
      ((played_results$home_id == id_a & played_results$away_id == id_b) |
         (played_results$home_id == id_b & played_results$away_id == id_a)), ]
  if (nrow(hit) == 0) return(NULL)
  hit[1, ]
}

##### End of check of existing results #####


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
  left_join(elo, by=c("team_name"="country"))

# Warn about any teams that didn't match the ELO source
missing <- teams_init %>% filter(is.na(elo)) %>% pull(team_name)
if (length(missing) > 0) {
  message("⚠️  No ELO found for: ", paste(missing, collapse=", "),
          " — substituting median ELO (", round(median(teams_init$elo, na.rm=TRUE)), ")")
  teams_init <- teams_init %>%
    mutate(elo = ifelse(is.na(elo), median(elo, na.rm=TRUE), elo))
}

# ── Deutsche Ländernamen (Mapping per FIFA-Code) ────────────
# Wird ausschließlich für die Anzeige in der UI verwendet.
# Die englische team_name-Spalte bleibt bestehen, weil sie für
# den ELO-Join (matching gegen World Football Elo Ratings) nötig ist.
team_name_de_map <- c(
  MEX="Mexiko", RSA="Südafrika", KOR="Südkorea", CZE="Tschechien",
  CAN="Kanada", BIH="Bosnien-Herzegowina", QAT="Katar", SUI="Schweiz",
  BRA="Brasilien", MAR="Marokko", HAI="Haiti", SCO="Schottland",
  USA="USA", PAR="Paraguay", AUS="Australien", TUR="Türkei",
  GER="Deutschland", CUR="Curaçao", CIV="Elfenbeinküste", ECU="Ecuador",
  NED="Niederlande", JPN="Japan", SWE="Schweden", TUN="Tunesien",
  BEL="Belgien", EGY="Ägypten", IRN="Iran", NZL="Neuseeland",
  ESP="Spanien", CPV="Kap Verde", KSA="Saudi-Arabien", URU="Uruguay",
  FRA="Frankreich", SEN="Senegal", IRQ="Irak", NOR="Norwegen",
  ARG="Argentinien", ALG="Algerien", AUT="Österreich", JOR="Jordanien",
  POR="Portugal", COD="DR Kongo", UZB="Usbekistan", COL="Kolumbien",
  ENG="England", CRO="Kroatien", GHA="Ghana", PAN="Panama"
)

teams_init <- teams_init %>%
  mutate(team_name_de = ifelse(fifa_code %in% names(team_name_de_map),
                               team_name_de_map[fifa_code],
                               team_name))   # fallback: englischer Name

# ── KO-Runden auf Deutsch (für UI-Anzeige) ──────────────────
# Interne stage-Strings bleiben englisch, weil results.tsv
# darauf basiert; übersetzt wird nur beim Rendern.
stage_de_map <- c(
  "Group"         = "Gruppe",
  "Round of 32"   = "Sechzehntelfinale",
  "Round of 16"   = "Achtelfinale",
  "Quarter-Final" = "Viertelfinale",
  "Semi-Final"    = "Halbfinale",
  "Third Place"   = "Spiel um Platz 3",
  "Final"         = "Finale"
)

# Flag emoji lookup
flag_map <- c(MEX="🇲🇽",RSA="🇿🇦",KOR="🇰🇷",CZE="🇨🇿",CAN="🇨🇦",BIH="🇧🇦",QAT="🇶🇦",
              SUI="🇨🇭",BRA="🇧🇷",MAR="🇲🇦",HAI="🇭🇹",SCO="🏴󠁧󠁢󠁳󠁣󠁴󠁿",USA="🇺🇸",PAR="🇵🇾",
              AUS="🇦🇺",TUR="🇹🇷",GER="🇩🇪",CUR="🇨🇼",CIV="🇨🇮",ECU="🇪🇨",NED="🇳🇱",
              JPN="🇯🇵",SWE="🇸🇪",TUN="🇹🇳",BEL="🇧🇪",EGY="🇪🇬",IRN="🇮🇷",NZL="🇳🇿",
              ESP="🇪🇸",CPV="🇨🇻",KSA="🇸🇦",URU="🇺🇾",FRA="🇫🇷",SEN="🇸🇳",IRQ="🇮🇶",
              NOR="🇳🇴",ARG="🇦🇷",ALG="🇩🇿",AUT="🇦🇹",JOR="🇯🇴",POR="🇵🇹",COD="🇨🇩",
              UZB="🇺🇿",COL="🇨🇴",ENG="🏴󠁧󠁢󠁥󠁮󠁧󠁿",CRO="🇭🇷",GHA="🇬🇭",PAN="🇵🇦")

get_flag <- function(code) { ifelse(is.na(flag_map[code]), "🏳️", flag_map[code]) }


hist_score_dist <- read.csv("../data/score_dist.csv", stringsAsFactors = FALSE)

# Sample a score from historical distribution.
# p_fav  : favourite's ELO win probability (>= 0.5 always — caller must ensure this)
# outcome: "fav_win" | "draw" | "und_win"
# Returns list(fav_goals, und_goals)
sample_hist_score <- function(p_fav, sim_outcome) {
  # Find bin — p_fav is always in [0.5, 1.0]; the top bin uses include.lowest
  bin_row <- hist_score_dist %>%
    filter(outcome == sim_outcome,
           p_lo <= p_fav, p_fav < p_hi)

  # Edge case: p_fav == 1.0 → use top bin
  if (nrow(bin_row) == 0) {
    bin_row <- hist_score_dist %>%
      filter(outcome == csv_outcome, p_hi == max(p_hi))
  }

  if (nrow(bin_row) == 0) return(NULL)   # fallback signal

  idx       <- sample(nrow(bin_row), 1, prob = bin_row$prob)
  fav_goals <- bin_row$fav_goals[idx]   # "home" ≡ favourite in the CSV
  und_goals <- bin_row$und_goals[idx]
  list(fav_goals = fav_goals, und_goals = und_goals)
}

# ── SIMULATION PARAMETERS ────────────────────────────────────
# Zentraler Container für alle Tuning-Parameter der Simulation.
# Defaults = aktuelles Verhalten der App (vor Einführung der Schalter).
# Wird durch die ganze Aufrufkette gereicht, damit keine Funktion
# eine lange Argumentliste braucht.
default_params <- list(
  k                 = 60,    # ELO-Lerngeschwindigkeit
  use_historical    = FALSE, # FALSE = Poisson, TRUE = empirische Tor-Verteilung
  home_advantage    = 0,     # ELO-Bonus für USA/CAN/MEX (0 = kein Heimvorteil)
  team_boost_id     = NA,    # team$id der Mannschaft, die den Bonus erhält
  team_boost_value  = 0,     # ELO-Bonus für das gewählte Team
  team_adjustments  = NULL,  # named numeric vector (id → ELO-Offset/Differenz) aus der ELO-Rangliste
  goal_scale        = 1.0,   # Multiplikator auf Poisson-λ (1 = original)
  draw_max          = 1/3,   # max. Wahrscheinlichkeit eines Unentschieden bei p_h = 0.5 (ELO-Gleichstand)
  upset_factor      = 1.0,   # interner Multiplikator: 1 = neutral, <1 = Außenseiter begünstigt, >1 = Favoriten begünstigt
                             # UI zeigt diesen Wert als (1 - upset_factor); Slider-Bereich -3 bis +3 entspricht intern 4 bis -2
  prediction_model        = "elo", # "elo" = bisherige ELO-Simulation, "dixon_coles" = DC-Tormatrizen
  dc_host_factor          = 1.0,   # Multiplikator auf den host-Koeffizienten des DC-Modells (1 = original, 0 = aus)
  dc_intercept_delta      = 0,     # Δ auf den Intercept (globales Torniveau, log-Skala)
  dc_attack_values        = NULL,  # named numeric (id → absoluter Slider-Wert 0–2.5, entspricht exp(attack_log))
  dc_defense_values       = NULL   # named numeric (id → absoluter Slider-Wert 0–2.5, entspricht exp(defense_log))
)

# Wendet die ELO-Modifier (Heimvorteil + Team-Boost + Per-Team-Adjustments)
# auf den Mannschaftsdataframe an. Wird einmal pro Turnier vor Anpfiff gerufen.
apply_elo_modifiers <- function(teams_df, params) {
  if (params$home_advantage != 0) {
    hosts <- c("USA", "CAN", "MEX")
    teams_df <- teams_df %>%
      mutate(elo = ifelse(fifa_code %in% hosts,
                          elo + params$home_advantage, elo))
  }
  if (!is.na(params$team_boost_id) && params$team_boost_value != 0) {
    teams_df <- teams_df %>%
      mutate(elo = ifelse(id == params$team_boost_id,
                          elo + params$team_boost_value, elo))
  }
  # Per-Team-Adjustments aus der ELO-Rangliste (additiv zu den anderen Modifiern).
  # Werte sind Differenzen (Slider-Wert minus Original-Start-ELO).
  if (!is.null(params$team_adjustments) && length(params$team_adjustments) > 0) {
    adj <- params$team_adjustments
    adj <- adj[!is.na(adj) & adj != 0]
    if (length(adj) > 0) {
      adj_df <- data.frame(id = as.integer(names(adj)),
                           .adj = as.numeric(adj),
                           stringsAsFactors = FALSE)
      teams_df <- teams_df %>%
        left_join(adj_df, by = "id") %>%
        mutate(elo = elo + ifelse(is.na(.adj), 0, .adj)) %>%
        select(-.adj)
    }
  }
  teams_df
}

# ── DIXON-COLES ENGINE ───────────────────────────────────────

dc_model_path <- "../dc/dc_model.rds"
dc_model_fit  <- NULL
dc_prob_cache <- new.env(parent = emptyenv())

# Aktives DC-Modell mit aktuell angewendeten User-Overrides.
# Wird zu Beginn jedes run_tournament-Aufrufs gesetzt (siehe dort).
# Bleibt NULL, solange keine Simulation läuft (UI nutzt dann load_dc_model()).
dc_active_model <- NULL

dc_available <- function() {
  file.exists(dc_model_path) && requireNamespace("goalmodel", quietly = TRUE)
}

load_dc_model <- function() {
  if (!dc_available()) {
    stop("Dixon-Coles-Modell nicht verfügbar. Bitte goalmodel installieren und ../dc/dc_model.rds bereitstellen.")
  }
  if (is.null(dc_model_fit)) {
    dc_model_fit <<- readRDS(dc_model_path)
  }
  dc_model_fit
}

# Wendet die User-Slider-Werte als additive Offsets (log-Skala) auf eine
# Kopie des gefitteten Modells an. Globale Parameter (hfa, rho, intercept)
# und per-Team-Parameter (attack, defense) werden separat behandelt.
# Rückgabewert: identische Struktur wie das Original; nur $parameters wird ersetzt.
apply_dc_overrides <- function(model_fit, params) {
  m <- model_fit
  p <- m$parameters
  
  if (!is.null(params$dc_host_factor) && !is.null(p$beta) && "host" %in% names(p$beta)) {
    f <- as.numeric(params$dc_host_factor)
    if (!is.na(f) && f != 1) {
      p$beta[["host"]] <- p$beta[["host"]] * f
    }
  }
  if (!is.null(params$dc_intercept_delta) && isTRUE(params$dc_intercept_delta != 0) && !is.null(p$intercept)) {
    p$intercept <- p$intercept + as.numeric(params$dc_intercept_delta)
  }
  
  # Per-Team-Slider liefern direkt die log-skalierten Modellparameter
  # (attack bzw. defense). Range −5 bis +5. Slider = Originalwert ⇒
  # Modell unverändert. Höheres attack = offensiver; höheres defense =
  # bessere Abwehr (goalmodel-Konvention, NICHT invertiert).
  apply_abs <- function(param_vec, abs_values) {
    if (is.null(abs_values) || length(abs_values) == 0) return(param_vec)
    abs_values <- abs_values[!is.na(abs_values)]
    if (length(abs_values) == 0) return(param_vec)
    nm_match <- intersect(names(abs_values), names(param_vec))
    if (length(nm_match) == 0) return(param_vec)
    for (k in nm_match) {
      param_vec[k] <- as.numeric(abs_values[[k]])
    }
    param_vec
  }
  
  if (!is.null(p$attack))  p$attack  <- apply_abs(p$attack,  params$dc_attack_values)
  if (!is.null(p$defense)) p$defense <- apply_abs(p$defense, params$dc_defense_values)
  
  m$parameters <- p
  m
}

get_dc_prob_matrix <- function(id_home, id_away) {
  if (is.null(id_home) || is.null(id_away) || is.na(id_home) || is.na(id_away)) {
    stop("Dixon-Coles benötigt gültige Team-IDs.")
  }
  
  key <- paste0(id_home, "_", id_away)
  if (exists(key, envir = dc_prob_cache, inherits = FALSE)) {
    return(get(key, envir = dc_prob_cache, inherits = FALSE))
  }
  
  # Während einer Turnier-Simulation: das (ggf. modifizierte) aktive Modell nutzen.
  # Außerhalb (z.B. wenn die DC-Funktionen für UI-Vorschauen direkt gerufen werden):
  # auf das Originalmodell zurückfallen.
  model_fit <- if (!is.null(dc_active_model)) dc_active_model else load_dc_model()
  host_ids  <- c(1L, 5L, 13L) # USA, Kanada, Mexiko
  x1 <- matrix(c(0, as.integer(id_home %in% host_ids), 0), ncol = 3)
  x2 <- matrix(c(as.integer(id_away %in% host_ids), 0), ncol = 2)
  colnames(x1) <- c("home", "host", "friendly")
  colnames(x2) <- c("host", "friendly")

  mat <- goalmodel::predict_goals(
    model_fit,
    team1 = id_home,
    team2 = id_away,
    maxgoal = 10,
    return_df = FALSE,
    x1 = x1,
    x2 = x2
  )
  if (is.list(mat)) mat <- mat[[1]]
  if (!is.matrix(mat) || any(is.na(mat))) {
    stop("Dixon-Coles konnte keine gültige Tormatrix berechnen für ", id_home, " vs ", id_away, ".")
  }
  # Extreme ρ-Werte können einzelne Zellen der Tau-Korrektur negativ werden lassen.
  # Vor der Normalisierung clampen, sonst füttert sample.int() negative
  # Wahrscheinlichkeiten und löscht den Worker.
  mat[mat < 0] <- 0
  s <- sum(mat)
  if (s <= 0) {
    stop("Dixon-Coles konnte keine gültige Tormatrix berechnen für ", id_home, " vs ", id_away, ".")
  }
  mat <- mat / s
  assign(key, mat, envir = dc_prob_cache)
  mat
}

sample_dc_score <- function(id_home, id_away) {
  mat <- get_dc_prob_matrix(id_home, id_away)
  probs <- as.vector(mat)
  idx <- sample.int(length(probs), 1, prob = probs)
  n_home <- nrow(mat)
  home_goals <- (idx - 1) %% n_home
  away_goals <- (idx - 1) %/% n_home
  list(home_goals = home_goals, away_goals = away_goals, matrix = mat)
}

simulate_match_dc <- function(id_home, id_away, elo_home, elo_away) {
  sc <- sample_dc_score(id_home, id_away)
  outcome <- if (sc$home_goals > sc$away_goals) "home"
  else if (sc$home_goals < sc$away_goals) "away"
  else "draw"

  mat <- sc$matrix
  home_win_prob <- sum(mat[row(mat) > col(mat)])

  list(home_goals    = sc$home_goals,
       away_goals    = sc$away_goals,
       outcome       = outcome,
       new_elo_home  = elo_home,
       new_elo_away  = elo_away,
       pen_home_prob = home_win_prob)
}

dc_home_win_probability <- function(id_home, id_away) {
  mat <- get_dc_prob_matrix(id_home, id_away)
  sum(mat[row(mat) > col(mat)])
}

# ── ELO ENGINE ───────────────────────────────────────────────

elo_expected <- function(ea, eb) 1 / (1 + 10^((eb - ea) / 400))

simulate_match_elo <- function(elo_home, elo_away,
                               params = default_params,
                               force_gh = NULL, force_ga = NULL) {

  # ── Fixed result (real, already-played match): bypass simulation ──
  # ELO is NOT updated here — update_elo.R already reflects this match
  # in the cache, so updating again would double-count.
  if (!is.null(force_gh) && !is.null(force_ga)) {
    outcome <- if (force_gh > force_ga) "home"
    else if (force_gh < force_ga) "away"
    else "draw"
    return(list(home_goals    = force_gh,
                away_goals    = force_ga,
                outcome       = outcome,
                new_elo_home  = elo_home,
                new_elo_away  = elo_away))
  }

  # Raw win probability (für ELO-Update später unverändert nötig)
  p_raw <- elo_expected(elo_home, elo_away)

  # ── Underdog-Faktor: skaliert die Abweichung von 50/50 ──
  # 1.0 = original, 0 = alle Spiele 50/50, 2 = doppelt so deutliche Favoriten
  p_h <- 0.5 + (p_raw - 0.5) * params$upset_factor
  p_h <- min(max(p_h, 0.001), 0.999)

  # ── Unentschieden-Häufigkeit ──
  # Glockenkurve um p_h = 0.5 (ELO-Gleichstand). params$draw_max ist die
  # maximale Wahrscheinlichkeit für ein Unentschieden bei p_h = 0.5; mit
  # zunehmender Abweichung (eindeutigerer Favorit) fällt die Draw-WK gauss-
  # förmig gegen 0. Sigma steuert die Breite der Glocke und bleibt fix.
  sigma  <- .3#0.236875
  draw_p <- params$draw_max * exp(-((p_h - 0.5)^2) / (2 * sigma^2))
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
  if (params$use_historical) {
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
    # Torreichtum: Multiplikator auf Lambda (1 = original WM-Niveau)
    lh <- (1.99419 * p_h + 0.24629) * params$goal_scale
    la <- (1.99419 * (1-p_h) + 0.24629) * params$goal_scale
    repeat {
      gh <- rpois(1, lh); ga <- rpois(1, la)
      if (outcome == "home" && gh > ga) break
      if (outcome == "away" && ga > gh) break
      if (outcome == "draw" && gh == ga) break
    }
  }

  act_h    <- ifelse(outcome == "home", 1, ifelse(outcome == "draw", 0.5, 0))
  exp_h    <- p_raw                                # ELO-Update mit Roh-Probability
  goal_diff <- abs(gh - ga)
  k_mult   <- if (goal_diff <= 1) 1
  else if (goal_diff == 2) 1.5
  else if (goal_diff == 3) 1.75
  else 1.75 + (goal_diff - 3) / 8
  k_adj <- params$k * k_mult

  list(home_goals    = gh,
       away_goals    = ga,
       outcome       = outcome,
       new_elo_home  = elo_home + k_adj * (act_h - exp_h),
       new_elo_away  = elo_away + k_adj * ((1 - act_h) - (1 - exp_h)))
}

simulate_match <- function(elo_home, elo_away,
                           params = default_params,
                           force_gh = NULL, force_ga = NULL,
                           id_home = NULL, id_away = NULL) {

  # Bereits gespielte Partien bleiben modellunabhängig fix.
  if (!is.null(force_gh) && !is.null(force_ga)) {
    return(simulate_match_elo(elo_home, elo_away, params = params,
                              force_gh = force_gh, force_ga = force_ga))
  }

  if (identical(params$prediction_model, "dixon_coles")) {
    return(simulate_match_dc(id_home, id_away, elo_home, elo_away))
  }

  simulate_match_elo(elo_home, elo_away, params = params)
}

# ── FIFA TIEBREAKER ──────────────────────────────────────────
# FIFA group-stage tiebreakers in order:
#   1. Points (overall)
#   2. Goal difference (overall)
#   3. Goals scored (overall)
#   4. Points in matches between tied teams (head-to-head)
#   5. Goal difference in head-to-head matches
#   6. Goals scored in head-to-head matches
# Final fallback (substitute for FIFA's "fair play" / "drawing
# of lots", which we cannot simulate): ELO.
rank_group_fifa <- function(standing, raw_matches) {
  standing <- standing %>%
    arrange(desc(pts), desc(gd), desc(gf)) %>%
    mutate(tie_key = paste(pts, gd, gf, sep = "_"))

  resolved <- lapply(unique(standing$tie_key), function(tk) {
    sub <- standing %>% filter(tie_key == tk)
    if (nrow(sub) == 1) return(sub)

    tied_ids <- sub$id
    h2h <- raw_matches %>%
      filter(home_id %in% tied_ids, away_id %in% tied_ids)

    h2h_pts <- h2h_gf <- h2h_ga <-
      setNames(rep(0, length(tied_ids)), as.character(tied_ids))

    for (i in seq_len(nrow(h2h))) {
      h  <- as.character(h2h$home_id[i])
      a  <- as.character(h2h$away_id[i])
      gh <- h2h$home_goals[i]
      ga_ <- h2h$away_goals[i]
      h2h_gf[h] <- h2h_gf[h] + gh
      h2h_ga[h] <- h2h_ga[h] + ga_
      h2h_gf[a] <- h2h_gf[a] + ga_
      h2h_ga[a] <- h2h_ga[a] + gh
      if      (gh >  ga_) h2h_pts[h] <- h2h_pts[h] + 3
      else if (gh <  ga_) h2h_pts[a] <- h2h_pts[a] + 3
      else { h2h_pts[h] <- h2h_pts[h] + 1; h2h_pts[a] <- h2h_pts[a] + 1 }
    }

    sub %>%
      mutate(h2h_pts = as.numeric(h2h_pts[as.character(id)]),
             h2h_gd  = as.numeric(h2h_gf[as.character(id)] -
                                    h2h_ga[as.character(id)]),
             h2h_gf  = as.numeric(h2h_gf[as.character(id)])) %>%
      arrange(desc(h2h_pts), desc(h2h_gd), desc(h2h_gf), desc(elo)) %>%
      select(-h2h_pts, -h2h_gd, -h2h_gf)
  })

  bind_rows(resolved) %>%
    select(-tie_key) %>%
    mutate(rank = row_number())
}

# ── GROUP STAGE ──────────────────────────────────────────────

run_group_stage <- function(teams_df, params = default_params) {
  elo_start <- setNames(teams_df$elo, teams_df$id)
  elo_live  <- elo_start

  all_matches   <- data.frame()
  all_standings <- data.frame()

  for (grp in sort(unique(teams_df$group_letter))) {
    ids   <- teams_df %>% filter(group_letter == grp) %>% pull(id)
    pairs <- combn(ids, 2, simplify = FALSE)
    pts <- gf <- ga <- setNames(rep(0, 4), ids)
    raw_grp_matches <- data.frame(home_id    = integer(),
                                  away_id    = integer(),
                                  home_goals = integer(),
                                  away_goals = integer())

    for (pair in pairs) {
      h <- pair[1]; a <- pair[2]

      elo_h <- elo_live[as.character(h)]
      elo_a <- elo_live[as.character(a)]

      # Already played? Use the real result instead of simulating.
      fixed <- lookup_result("Group", h, a)
      if (!is.null(fixed)) {
        if (fixed$home_id == h) {
          fgh <- fixed$home_goals; fga <- fixed$away_goals
        } else {
          fgh <- fixed$away_goals; fga <- fixed$home_goals
        }
        res <- simulate_match(elo_h, elo_a, params = params,
                              force_gh = fgh, force_ga = fga,
                              id_home = h, id_away = a)
      } else {
        res <- simulate_match(elo_h, elo_a, params = params,
                              id_home = h, id_away = a)
      }
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

      raw_grp_matches <- rbind(raw_grp_matches, data.frame(
        home_id = h, away_id = a,
        home_goals = res$home_goals, away_goals = res$away_goals))

      ht <- teams_df %>% filter(id == h)
      at <- teams_df %>% filter(id == a)
      all_matches <- rbind(all_matches, data.frame(
        stage  = "Group", group = grp,
        home   = paste(get_flag(ht$fifa_code), ht$team_name_de),
        away   = paste(get_flag(at$fifa_code), at$team_name_de),
        score  = paste0(res$home_goals, "-", res$away_goals),
        result = res$outcome, stringsAsFactors = FALSE))
    }

    standing <- data.frame(id = ids, pts = as.numeric(pts),
                           gf = as.numeric(gf), ga = as.numeric(ga)) %>%
      mutate(gd = gf - ga, elo = elo_live[as.character(ids)]) %>%
      arrange(desc(pts), desc(gd), desc(gf), desc(elo)) %>%
      mutate(rank = 1:4, group = grp) %>%
      left_join(teams_df %>% select(id, team_name_de, fifa_code), by = "id")
    all_standings <- rbind(all_standings, standing)
  }
  list(standings = all_standings, matches = all_matches, elo_live = elo_live)
}

# ── KNOCKOUT ─────────────────────────────────────────────────

sim_ko_match <- function(id_a, id_b, elo_live, teams_df,
                         round_name, params = default_params) {
  elo_h <- elo_live[as.character(id_a)]
  elo_a <- elo_live[as.character(id_b)]

  # Already played? Use the real result instead of simulating.
  fixed <- lookup_result(round_name, id_a, id_b)
  if (!is.null(fixed)) {
    if (fixed$home_id == id_a) {
      fgh <- fixed$home_goals; fga <- fixed$away_goals
    } else {
      fgh <- fixed$away_goals; fga <- fixed$home_goals
    }
    res <- simulate_match(elo_h, elo_a, params = params,
                          force_gh = fgh, force_ga = fga,
                          id_home = id_a, id_away = id_b)
  } else {
    res <- simulate_match(elo_h, elo_a, params = params,
                          id_home = id_a, id_away = id_b)
  }

  pens <- ""
  if (res$outcome == "draw") {
    pens <- " (i. E.)"
    if (!is.null(fixed) && !is.na(fixed$pens_winner_id)) {
      winner <- fixed$pens_winner_id
    } else {
      pa <- if (identical(params$prediction_model, "dixon_coles")) {
        res$pen_home_prob %||% dc_home_win_probability(id_a, id_b)
      } else {
        elo_expected(elo_h, elo_a)
      }
      winner <- ifelse(runif(1) < pa, id_a, id_b)
    }
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
                        home   = paste(get_flag(ta$fifa_code), ta$team_name_de),
                        away   = paste(get_flag(tb$fifa_code), tb$team_name_de),
                        score  = paste0(res$home_goals, "-", res$away_goals, pens),
                        result = paste("→", get_flag(tw$fifa_code), tw$team_name_de),
                        stringsAsFactors = FALSE))
}

run_knockout <- function(pairs, elo_live, teams_df,
                         round_name, params = default_params) {
  winners <- c(); losers <- c(); rows <- data.frame()
  for (pair in pairs) {
    res      <- sim_ko_match(pair[1], pair[2], elo_live, teams_df,
                             round_name, params = params)
    elo_live <- res$elo_live
    winners  <- c(winners, res$winner)
    losers   <- c(losers,  res$loser)
    rows     <- rbind(rows, res$row)
  }
  list(winners = winners, losers = losers, elo_live = elo_live, matches = rows)
}

# ── TOURNAMENT ───────────────────────────────────────────────

run_tournament <- function(seed = NULL, params = default_params) {
  if (!is.null(seed)) set.seed(seed)
  played_results <<- load_played_results()  # refresh from disk

  # Heimvorteil + Team-Boost + Per-Team-Adjustments VOR Turnierstart aufschlagen
  teams_df <- apply_elo_modifiers(teams_init, params)
  
  # Dixon-Coles: User-Overrides auf das gefittete Modell anwenden und Cache leeren.
  # Außerhalb des DC-Modus bleibt dc_active_model NULL und das Original wird benutzt.
  rm(list = ls(dc_prob_cache, all.names = TRUE), envir = dc_prob_cache)
  dc_active_model <<- if (identical(params$prediction_model, "dixon_coles") && dc_available()) {
    apply_dc_overrides(load_dc_model(), params)
  } else {
    NULL
  }
  
  gs       <- run_group_stage(teams_df, params = params)
  elo_live <- gs$elo_live
  std      <- gs$standings

  thirds <- std %>% filter(rank == 3) %>%
    arrange(desc(pts), desc(gd), desc(gf), desc(elo)) %>% slice(1:8)

  get_t <- function(grp, rnk) std %>% filter(group == grp, rank == rnk) %>% pull(id)

  # ── FIFA WM-2026 Annex C: Zuordnung der Gruppendritten im Sechzehntelfinale ──
  # Welcher Gruppensieger gegen den Dritten WELCHER Gruppe spielt, haengt NICHT
  # vom Rang der Dritten ab, sondern von der KOMBINATION der acht Gruppen, aus
  # denen ein qualifizierter Dritter stammt (C(12,8) = 495 Faelle, Annex C des
  # offiziellen FIFA-Reglements). Schluessel = sortierte 8-Buchstaben-Kombination
  # dieser Gruppen. Wert = 8 Zeichen = Quell-Gruppe des jeweiligen Dritten fuer
  # die Gruppensieger 1A,1B,1D,1E,1G,1I,1K,1L (genau in dieser Reihenfolge).
  # Schliesst ein Wiedersehen aus derselben Gruppe konstruktiv aus.
  annexC <- c(
    "ABCDEFGH"="HGBCAFDE", "ABCDEFGI"="CGBDAFEI", "ABCDEFGJ"="CGBDAFEJ", "ABCDEFGK"="CGBDAFEK", "ABCDEFGL"="CGBDAFLE",
    "ABCDEFHI"="HEBCAFDI", "ABCDEFHJ"="HJBCAFDE", "ABCDEFHK"="HEBCAFDK", "ABCDEFHL"="HFBCADLE", "ABCDEFIJ"="CJBDAFEI",
    "ABCDEFIK"="CEBDAFIK", "ABCDEFIL"="CEBDAFLI", "ABCDEFJK"="CJBDAFEK", "ABCDEFJL"="CJBDAFLE", "ABCDEFKL"="CEBDAFLK",
    "ABCDEGHI"="HGBCADEI", "ABCDEGHJ"="HGBCADEJ", "ABCDEGHK"="HGBCADEK", "ABCDEGHL"="HGBCADLE", "ABCDEGIJ"="EGBCADIJ",
    "ABCDEGIK"="EGBCADIK", "ABCDEGIL"="EGBCADLI", "ABCDEGJK"="EGBCADJK", "ABCDEGJL"="EGBCADLJ", "ABCDEGKL"="EGBCADLK",
    "ABCDEHIJ"="HJBCADEI", "ABCDEHIK"="HEBCADIK", "ABCDEHIL"="HEBCADLI", "ABCDEHJK"="HJBCADEK", "ABCDEHJL"="HJBCADLE",
    "ABCDEHKL"="HEBCADLK", "ABCDEIJK"="EJBCADIK", "ABCDEIJL"="EJBCADLI", "ABCDEIKL"="EIBCADLK", "ABCDEJKL"="EJBCADLK",
    "ABCDFGHI"="HGBCAFDI", "ABCDFGHJ"="HGBCAFDJ", "ABCDFGHK"="HGBCAFDK", "ABCDFGHL"="CGBDAFLH", "ABCDFGIJ"="CGBDAFIJ",
    "ABCDFGIK"="CGBDAFIK", "ABCDFGIL"="CGBDAFLI", "ABCDFGJK"="CGBDAFJK", "ABCDFGJL"="CGBDAFLJ", "ABCDFGKL"="CGBDAFLK",
    "ABCDFHIJ"="HJBCAFDI", "ABCDFHIK"="HFBCADIK", "ABCDFHIL"="HFBCADLI", "ABCDFHJK"="HJBCAFDK", "ABCDFHJL"="CJBDAFLH",
    "ABCDFHKL"="HFBCADLK", "ABCDFIJK"="CJBDAFIK", "ABCDFIJL"="CJBDAFLI", "ABCDFIKL"="CIBDAFLK", "ABCDFJKL"="CJBDAFLK",
    "ABCDGHIJ"="HGBCADIJ", "ABCDGHIK"="HGBCADIK", "ABCDGHIL"="HGBCADLI", "ABCDGHJK"="HGBCADJK", "ABCDGHJL"="HGBCADLJ",
    "ABCDGHKL"="HGBCADLK", "ABCDGIJK"="CJBDAGIK", "ABCDGIJL"="CJBDAGLI", "ABCDGIKL"="IGBCADLK", "ABCDGJKL"="CJBDAGLK",
    "ABCDHIJK"="HJBCADIK", "ABCDHIJL"="HJBCADLI", "ABCDHIKL"="HIBCADLK", "ABCDHJKL"="HJBCADLK", "ABCDIJKL"="IJBCADLK",
    "ABCEFGHI"="HGBCAFEI", "ABCEFGHJ"="HGBCAFEJ", "ABCEFGHK"="HGBCAFEK", "ABCEFGHL"="HGBCAFLE", "ABCEFGIJ"="EGBCAFIJ",
    "ABCEFGIK"="EGBCAFIK", "ABCEFGIL"="EGBCAFLI", "ABCEFGJK"="EGBCAFJK", "ABCEFGJL"="EGBCAFLJ", "ABCEFGKL"="EGBCAFLK",
    "ABCEFHIJ"="HJBCAFEI", "ABCEFHIK"="HEBCAFIK", "ABCEFHIL"="HEBCAFLI", "ABCEFHJK"="HJBCAFEK", "ABCEFHJL"="HJBCAFLE",
    "ABCEFHKL"="HEBCAFLK", "ABCEFIJK"="EJBCAFIK", "ABCEFIJL"="EJBCAFLI", "ABCEFIKL"="EIBCAFLK", "ABCEFJKL"="EJBCAFLK",
    "ABCEGHIJ"="HJBCAGEI", "ABCEGHIK"="EGBCAHIK", "ABCEGHIL"="EGBCAHLI", "ABCEGHJK"="HJBCAGEK", "ABCEGHJL"="HJBCAGLE",
    "ABCEGHKL"="EGBCAHLK", "ABCEGIJK"="EJBCAGIK", "ABCEGIJL"="EJBCAGLI", "ABCEGIKL"="EGBAICLK", "ABCEGJKL"="EJBCAGLK",
    "ABCEHIJK"="EJBCAHIK", "ABCEHIJL"="EJBCAHLI", "ABCEHIKL"="EIBCAHLK", "ABCEHJKL"="EJBCAHLK", "ABCEIJKL"="EJBAICLK",
    "ABCFGHIJ"="HGBCAFIJ", "ABCFGHIK"="HGBCAFIK", "ABCFGHIL"="HGBCAFLI", "ABCFGHJK"="HGBCAFJK", "ABCFGHJL"="HGBCAFLJ",
    "ABCFGHKL"="HGBCAFLK", "ABCFGIJK"="CJBFAGIK", "ABCFGIJL"="CJBFAGLI", "ABCFGIKL"="IGBCAFLK", "ABCFGJKL"="CJBFAGLK",
    "ABCFHIJK"="HJBCAFIK", "ABCFHIJL"="HJBCAFLI", "ABCFHIKL"="HIBCAFLK", "ABCFHJKL"="HJBCAFLK", "ABCFIJKL"="IJBCAFLK",
    "ABCGHIJK"="HJBCAGIK", "ABCGHIJL"="HJBCAGLI", "ABCGHIKL"="IGBCAHLK", "ABCGHJKL"="HJBCAGLK", "ABCGIJKL"="IJBCAGLK",
    "ABCHIJKL"="IJBCAHLK", "ABDEFGHI"="HGBDAFEI", "ABDEFGHJ"="HGBDAFEJ", "ABDEFGHK"="HGBDAFEK", "ABDEFGHL"="HGBDAFLE",
    "ABDEFGIJ"="EGBDAFIJ", "ABDEFGIK"="EGBDAFIK", "ABDEFGIL"="EGBDAFLI", "ABDEFGJK"="EGBDAFJK", "ABDEFGJL"="EGBDAFLJ",
    "ABDEFGKL"="EGBDAFLK", "ABDEFHIJ"="HJBDAFEI", "ABDEFHIK"="HEBDAFIK", "ABDEFHIL"="HEBDAFLI", "ABDEFHJK"="HJBDAFEK",
    "ABDEFHJL"="HJBDAFLE", "ABDEFHKL"="HEBDAFLK", "ABDEFIJK"="EJBDAFIK", "ABDEFIJL"="EJBDAFLI", "ABDEFIKL"="EIBDAFLK",
    "ABDEFJKL"="EJBDAFLK", "ABDEGHIJ"="HJBDAGEI", "ABDEGHIK"="EGBDAHIK", "ABDEGHIL"="EGBDAHLI", "ABDEGHJK"="HJBDAGEK",
    "ABDEGHJL"="HJBDAGLE", "ABDEGHKL"="EGBDAHLK", "ABDEGIJK"="EJBDAGIK", "ABDEGIJL"="EJBDAGLI", "ABDEGIKL"="EGBAIDLK",
    "ABDEGJKL"="EJBDAGLK", "ABDEHIJK"="EJBDAHIK", "ABDEHIJL"="EJBDAHLI", "ABDEHIKL"="EIBDAHLK", "ABDEHJKL"="EJBDAHLK",
    "ABDEIJKL"="EJBAIDLK", "ABDFGHIJ"="HGBDAFIJ", "ABDFGHIK"="HGBDAFIK", "ABDFGHIL"="HGBDAFLI", "ABDFGHJK"="HGBDAFJK",
    "ABDFGHJL"="HGBDAFLJ", "ABDFGHKL"="HGBDAFLK", "ABDFGIJK"="FJBDAGIK", "ABDFGIJL"="FJBDAGLI", "ABDFGIKL"="IGBDAFLK",
    "ABDFGJKL"="FJBDAGLK", "ABDFHIJK"="HJBDAFIK", "ABDFHIJL"="HJBDAFLI", "ABDFHIKL"="HIBDAFLK", "ABDFHJKL"="HJBDAFLK",
    "ABDFIJKL"="IJBDAFLK", "ABDGHIJK"="HJBDAGIK", "ABDGHIJL"="HJBDAGLI", "ABDGHIKL"="IGBDAHLK", "ABDGHJKL"="HJBDAGLK",
    "ABDGIJKL"="IJBDAGLK", "ABDHIJKL"="IJBDAHLK", "ABEFGHIJ"="HJBFAGEI", "ABEFGHIK"="EGBFAHIK", "ABEFGHIL"="EGBFAHLI",
    "ABEFGHJK"="HJBFAGEK", "ABEFGHJL"="HJBFAGLE", "ABEFGHKL"="EGBFAHLK", "ABEFGIJK"="EJBFAGIK", "ABEFGIJL"="EJBFAGLI",
    "ABEFGIKL"="EGBAIFLK", "ABEFGJKL"="EJBFAGLK", "ABEFHIJK"="EJBFAHIK", "ABEFHIJL"="EJBFAHLI", "ABEFHIKL"="EIBFAHLK",
    "ABEFHJKL"="EJBFAHLK", "ABEFIJKL"="EJBAIFLK", "ABEGHIJK"="EJBAHGIK", "ABEGHIJL"="EJBAHGLI", "ABEGHIKL"="EGBAIHLK",
    "ABEGHJKL"="EJBAHGLK", "ABEGIJKL"="EJBAIGLK", "ABEHIJKL"="EJBAIHLK", "ABFGHIJK"="HJBFAGIK", "ABFGHIJL"="HJBFAGLI",
    "ABFGHIKL"="HGBAIFLK", "ABFGHJKL"="HJBFAGLK", "ABFGIJKL"="IJBFAGLK", "ABFHIJKL"="HJBAIFLK", "ABGHIJKL"="HJBAIGLK",
    "ACDEFGHI"="HGECAFDI", "ACDEFGHJ"="HGJCAFDE", "ACDEFGHK"="HGECAFDK", "ACDEFGHL"="HGFCADLE", "ACDEFGIJ"="CGJDAFEI",
    "ACDEFGIK"="CGEDAFIK", "ACDEFGIL"="CGEDAFLI", "ACDEFGJK"="CGJDAFEK", "ACDEFGJL"="CGJDAFLE", "ACDEFGKL"="CGEDAFLK",
    "ACDEFHIJ"="HJECAFDI", "ACDEFHIK"="HEFCADIK", "ACDEFHIL"="HEFCADLI", "ACDEFHJK"="HJECAFDK", "ACDEFHJL"="HJFCADLE",
    "ACDEFHKL"="HEFCADLK", "ACDEFIJK"="CJEDAFIK", "ACDEFIJL"="CJEDAFLI", "ACDEFIKL"="CEIDAFLK", "ACDEFJKL"="CJEDAFLK",
    "ACDEGHIJ"="HGJCADEI", "ACDEGHIK"="HGECADIK", "ACDEGHIL"="HGECADLI", "ACDEGHJK"="HGJCADEK", "ACDEGHJL"="HGJCADLE",
    "ACDEGHKL"="HGECADLK", "ACDEGIJK"="EGJCADIK", "ACDEGIJL"="EGJCADLI", "ACDEGIKL"="EGICADLK", "ACDEGJKL"="EGJCADLK",
    "ACDEHIJK"="HJECADIK", "ACDEHIJL"="HJECADLI", "ACDEHIKL"="HEICADLK", "ACDEHJKL"="HJECADLK", "ACDEIJKL"="EJICADLK",
    "ACDFGHIJ"="HGJCAFDI", "ACDFGHIK"="HGFCADIK", "ACDFGHIL"="HGFCADLI", "ACDFGHJK"="HGJCAFDK", "ACDFGHJL"="CGJDAFLH",
    "ACDFGHKL"="HGFCADLK", "ACDFGIJK"="CGJDAFIK", "ACDFGIJL"="CGJDAFLI", "ACDFGIKL"="CGIDAFLK", "ACDFGJKL"="CGJDAFLK",
    "ACDFHIJK"="HJFCADIK", "ACDFHIJL"="HJFCADLI", "ACDFHIKL"="HFICADLK", "ACDFHJKL"="HJFCADLK", "ACDFIJKL"="CJIDAFLK",
    "ACDGHIJK"="HGJCADIK", "ACDGHIJL"="HGJCADLI", "ACDGHIKL"="HGICADLK", "ACDGHJKL"="HGJCADLK", "ACDGIJKL"="IGJCADLK",
    "ACDHIJKL"="HJICADLK", "ACEFGHIJ"="HGJCAFEI", "ACEFGHIK"="HGECAFIK", "ACEFGHIL"="HGECAFLI", "ACEFGHJK"="HGJCAFEK",
    "ACEFGHJL"="HGJCAFLE", "ACEFGHKL"="HGECAFLK", "ACEFGIJK"="EGJCAFIK", "ACEFGIJL"="EGJCAFLI", "ACEFGIKL"="EGICAFLK",
    "ACEFGJKL"="EGJCAFLK", "ACEFHIJK"="HJECAFIK", "ACEFHIJL"="HJECAFLI", "ACEFHIKL"="HEICAFLK", "ACEFHJKL"="HJECAFLK",
    "ACEFIJKL"="EJICAFLK", "ACEGHIJK"="EGJCAHIK", "ACEGHIJL"="EGJCAHLI", "ACEGHIKL"="EGICAHLK", "ACEGHJKL"="EGJCAHLK",
    "ACEGIJKL"="EJICAGLK", "ACEHIJKL"="EJICAHLK", "ACFGHIJK"="HGJCAFIK", "ACFGHIJL"="HGJCAFLI", "ACFGHIKL"="HGICAFLK",
    "ACFGHJKL"="HGJCAFLK", "ACFGIJKL"="IGJCAFLK", "ACFHIJKL"="HJICAFLK", "ACGHIJKL"="HJICAGLK", "ADEFGHIJ"="HGJDAFEI",
    "ADEFGHIK"="HGEDAFIK", "ADEFGHIL"="HGEDAFLI", "ADEFGHJK"="HGJDAFEK", "ADEFGHJL"="HGJDAFLE", "ADEFGHKL"="HGEDAFLK",
    "ADEFGIJK"="EGJDAFIK", "ADEFGIJL"="EGJDAFLI", "ADEFGIKL"="EGIDAFLK", "ADEFGJKL"="EGJDAFLK", "ADEFHIJK"="HJEDAFIK",
    "ADEFHIJL"="HJEDAFLI", "ADEFHIKL"="HEIDAFLK", "ADEFHJKL"="HJEDAFLK", "ADEFIJKL"="EJIDAFLK", "ADEGHIJK"="EGJDAHIK",
    "ADEGHIJL"="EGJDAHLI", "ADEGHIKL"="EGIDAHLK", "ADEGHJKL"="EGJDAHLK", "ADEGIJKL"="EJIDAGLK", "ADEHIJKL"="EJIDAHLK",
    "ADFGHIJK"="HGJDAFIK", "ADFGHIJL"="HGJDAFLI", "ADFGHIKL"="HGIDAFLK", "ADFGHJKL"="HGJDAFLK", "ADFGIJKL"="IGJDAFLK",
    "ADFHIJKL"="HJIDAFLK", "ADGHIJKL"="HJIDAGLK", "AEFGHIJK"="EGJFAHIK", "AEFGHIJL"="EGJFAHLI", "AEFGHIKL"="EGIFAHLK",
    "AEFGHJKL"="EGJFAHLK", "AEFGIJKL"="EJIFAGLK", "AEFHIJKL"="EJIFAHLK", "AEGHIJKL"="EJIAHGLK", "AFGHIJKL"="HJIFAGLK",
    "BCDEFGHI"="CGBDHFEI", "BCDEFGHJ"="HGBCJFDE", "BCDEFGHK"="CGBDHFEK", "BCDEFGHL"="CGBDHFLE", "BCDEFGIJ"="CGBDJFEI",
    "BCDEFGIK"="CGBDEFIK", "BCDEFGIL"="CGBDEFLI", "BCDEFGJK"="CGBDJFEK", "BCDEFGJL"="CGBDJFLE", "BCDEFGKL"="CGBDEFLK",
    "BCDEFHIJ"="CJBDHFEI", "BCDEFHIK"="CEBDHFIK", "BCDEFHIL"="CEBDHFLI", "BCDEFHJK"="CJBDHFEK", "BCDEFHJL"="CJBDHFLE",
    "BCDEFHKL"="CEBDHFLK", "BCDEFIJK"="CJBDEFIK", "BCDEFIJL"="CJBDEFLI", "BCDEFIKL"="CEBDIFLK", "BCDEFJKL"="CJBDEFLK",
    "BCDEGHIJ"="HGBCJDEI", "BCDEGHIK"="EGBCHDIK", "BCDEGHIL"="EGBCHDLI", "BCDEGHJK"="HGBCJDEK", "BCDEGHJL"="HGBCJDLE",
    "BCDEGHKL"="EGBCHDLK", "BCDEGIJK"="EGBCJDIK", "BCDEGIJL"="EGBCJDLI", "BCDEGIKL"="EGBCIDLK", "BCDEGJKL"="EGBCJDLK",
    "BCDEHIJK"="EJBCHDIK", "BCDEHIJL"="EJBCHDLI", "BCDEHIKL"="EIBCHDLK", "BCDEHJKL"="EJBCHDLK", "BCDEIJKL"="EJBCIDLK",
    "BCDFGHIJ"="HGBCJFDI", "BCDFGHIK"="CGBDHFIK", "BCDFGHIL"="CGBDHFLI", "BCDFGHJK"="HGBCJFDK", "BCDFGHJL"="CGBDHFLJ",
    "BCDFGHKL"="CGBDHFLK", "BCDFGIJK"="CGBDJFIK", "BCDFGIJL"="CGBDJFLI", "BCDFGIKL"="CGBDIFLK", "BCDFGJKL"="CGBDJFLK",
    "BCDFHIJK"="CJBDHFIK", "BCDFHIJL"="CJBDHFLI", "BCDFHIKL"="CIBDHFLK", "BCDFHJKL"="CJBDHFLK", "BCDFIJKL"="CJBDIFLK",
    "BCDGHIJK"="HGBCJDIK", "BCDGHIJL"="HGBCJDLI", "BCDGHIKL"="HGBCIDLK", "BCDGHJKL"="HGBCJDLK", "BCDGIJKL"="IGBCJDLK",
    "BCDHIJKL"="HJBCIDLK", "BCEFGHIJ"="HGBCJFEI", "BCEFGHIK"="EGBCHFIK", "BCEFGHIL"="EGBCHFLI", "BCEFGHJK"="HGBCJFEK",
    "BCEFGHJL"="HGBCJFLE", "BCEFGHKL"="EGBCHFLK", "BCEFGIJK"="EGBCJFIK", "BCEFGIJL"="EGBCJFLI", "BCEFGIKL"="EGBCIFLK",
    "BCEFGJKL"="EGBCJFLK", "BCEFHIJK"="EJBCHFIK", "BCEFHIJL"="EJBCHFLI", "BCEFHIKL"="EIBCHFLK", "BCEFHJKL"="EJBCHFLK",
    "BCEFIJKL"="EJBCIFLK", "BCEGHIJK"="EJBCHGIK", "BCEGHIJL"="EJBCHGLI", "BCEGHIKL"="EGBCIHLK", "BCEGHJKL"="EJBCHGLK",
    "BCEGIJKL"="EJBCIGLK", "BCEHIJKL"="EJBCIHLK", "BCFGHIJK"="HGBCJFIK", "BCFGHIJL"="HGBCJFLI", "BCFGHIKL"="HGBCIFLK",
    "BCFGHJKL"="HGBCJFLK", "BCFGIJKL"="IGBCJFLK", "BCFHIJKL"="HJBCIFLK", "BCGHIJKL"="HJBCIGLK", "BDEFGHIJ"="HGBDJFEI",
    "BDEFGHIK"="EGBDHFIK", "BDEFGHIL"="EGBDHFLI", "BDEFGHJK"="HGBDJFEK", "BDEFGHJL"="HGBDJFLE", "BDEFGHKL"="EGBDHFLK",
    "BDEFGIJK"="EGBDJFIK", "BDEFGIJL"="EGBDJFLI", "BDEFGIKL"="EGBDIFLK", "BDEFGJKL"="EGBDJFLK", "BDEFHIJK"="EJBDHFIK",
    "BDEFHIJL"="EJBDHFLI", "BDEFHIKL"="EIBDHFLK", "BDEFHJKL"="EJBDHFLK", "BDEFIJKL"="EJBDIFLK", "BDEGHIJK"="EJBDHGIK",
    "BDEGHIJL"="EJBDHGLI", "BDEGHIKL"="EGBDIHLK", "BDEGHJKL"="EJBDHGLK", "BDEGIJKL"="EJBDIGLK", "BDEHIJKL"="EJBDIHLK",
    "BDFGHIJK"="HGBDJFIK", "BDFGHIJL"="HGBDJFLI", "BDFGHIKL"="HGBDIFLK", "BDFGHJKL"="HGBDJFLK", "BDFGIJKL"="IGBDJFLK",
    "BDFHIJKL"="HJBDIFLK", "BDGHIJKL"="HJBDIGLK", "BEFGHIJK"="EJBFHGIK", "BEFGHIJL"="EJBFHGLI", "BEFGHIKL"="EGBFIHLK",
    "BEFGHJKL"="EJBFHGLK", "BEFGIJKL"="EJBFIGLK", "BEFHIJKL"="EJBFIHLK", "BEGHIJKL"="EJIBHGLK", "BFGHIJKL"="HJBFIGLK",
    "CDEFGHIJ"="CGJDHFEI", "CDEFGHIK"="CGEDHFIK", "CDEFGHIL"="CGEDHFLI", "CDEFGHJK"="CGJDHFEK", "CDEFGHJL"="CGJDHFLE",
    "CDEFGHKL"="CGEDHFLK", "CDEFGIJK"="CGEDJFIK", "CDEFGIJL"="CGEDJFLI", "CDEFGIKL"="CGEDIFLK", "CDEFGJKL"="CGEDJFLK",
    "CDEFHIJK"="CJEDHFIK", "CDEFHIJL"="CJEDHFLI", "CDEFHIKL"="CEIDHFLK", "CDEFHJKL"="CJEDHFLK", "CDEFIJKL"="CJEDIFLK",
    "CDEGHIJK"="EGJCHDIK", "CDEGHIJL"="EGJCHDLI", "CDEGHIKL"="EGICHDLK", "CDEGHJKL"="EGJCHDLK", "CDEGIJKL"="EGICJDLK",
    "CDEHIJKL"="EJICHDLK", "CDFGHIJK"="CGJDHFIK", "CDFGHIJL"="CGJDHFLI", "CDFGHIKL"="CGIDHFLK", "CDFGHJKL"="CGJDHFLK",
    "CDFGIJKL"="CGIDJFLK", "CDFHIJKL"="CJIDHFLK", "CDGHIJKL"="HGICJDLK", "CEFGHIJK"="EGJCHFIK", "CEFGHIJL"="EGJCHFLI",
    "CEFGHIKL"="EGICHFLK", "CEFGHJKL"="EGJCHFLK", "CEFGIJKL"="EGICJFLK", "CEFHIJKL"="EJICHFLK", "CEGHIJKL"="EJICHGLK",
    "CFGHIJKL"="HGICJFLK", "DEFGHIJK"="EGJDHFIK", "DEFGHIJL"="EGJDHFLI", "DEFGHIKL"="EGIDHFLK", "DEFGHJKL"="EGJDHFLK",
    "DEFGIJKL"="EGIDJFLK", "DEFHIJKL"="EJIDHFLK", "DEGHIJKL"="EJIDHGLK", "DFGHIJKL"="HGIDJFLK", "EFGHIJKL"="EJIFHGLK"
  )

  combo_key  <- paste(sort(thirds$group), collapse = "")
  assign_str <- annexC[[combo_key]]
  if (is.null(assign_str))
    stop("Annex-C-Kombination nicht gefunden: ", combo_key)
  third_for  <- setNames(strsplit(assign_str, "")[[1]],
                         c("A", "B", "D", "E", "G", "I", "K", "L"))

  # id des Dritten der Gruppe, die laut Annex C gegen 'winner_grp' antritt
  third_id <- function(winner_grp)
    std %>% filter(group == third_for[[winner_grp]], rank == 3) %>% pull(id)

  r32_pairs <- list(
    c(get_t("A",2), get_t("B",2)),
    c(get_t("C",1), get_t("F",2)),
    c(get_t("E",1), third_id("E")),
    c(get_t("F",1), get_t("C",2)),
    c(get_t("E",2), get_t("I",2)),
    c(get_t("I",1), third_id("I")),
    c(get_t("A",1), third_id("A")),
    c(get_t("L",1), third_id("L")),
    c(get_t("G",1), third_id("G")),
    c(get_t("D",1), third_id("D")),
    c(get_t("H",1), get_t("J",2)),
    c(get_t("K",2), get_t("L",2)),
    c(get_t("B",1), third_id("B")),
    c(get_t("D",2), get_t("G",2)),
    c(get_t("J",1), get_t("H",2)),
    c(get_t("K",1), third_id("K"))
  )

  ko_args <- list(elo_live = elo_live, teams_df = teams_df, params = params)

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

  # final_elo: enthält neben final_elo (Endstand nach Turnier) und
  # start_elo (Wert NACH Modifiern, mit dem das Turnier tatsächlich
  # gestartet wurde) auch orig_elo (ORIGINAL aus teams_init, ohne
  # jegliche Modifier). orig_elo ist der stabile Referenzanker für
  # die ELO-Anpassungs-Slider in der Rangliste.
  final_elo <- data.frame(
    id        = as.integer(names(elo_live)),
    final_elo = as.numeric(elo_live)
  ) %>%
    left_join(teams_df   %>% select(id, team_name, team_name_de, fifa_code, elo), by = "id") %>%
    left_join(teams_init %>% select(id, orig_elo = elo), by = "id") %>%
    mutate(
      change    = round(final_elo - elo),
      final_elo = round(final_elo),
      start_elo = round(elo),
      orig_elo  = round(orig_elo),
      flag      = sapply(fifa_code, get_flag)
    ) %>%
    arrange(desc(final_elo)) %>%
    mutate(rank = row_number())

  list(standings = std, group_matches = gs$matches, ko_matches = all_ko_matches,
       champion = champion, runner_up = runner, third = third,
       final_elo = final_elo)
}


# ── UI ───────────────────────────────────────────────────────

# Modul-weit gecachter Default-Lauf (seed = 111, default_params).
# Erste Berechnung erfolgt lazy beim ersten Session-Start; danach
# instantaner Return für alle weiteren Sessions im selben R-Prozess.
default_result_cache <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) cached <<- run_tournament(seed = 111)
    cached
  }
})

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
      #seed_elo, #seed_dc {
        background: var(--input-bg); border: 1px solid var(--border); color: var(--text);
        border-radius: 6px; padding: 8px 12px; width: 120px;
        font-family: monospace; font-size: 14px;
        transition: background 0.3s, border-color 0.3s, color 0.3s;
      }
      body:not(.light-mode) #seed_elo,
      body:not(.light-mode) #seed_dc { background: #000000; color: #FFFFFF; }
      body:not(.light-mode) #seed_elo::placeholder,
      body:not(.light-mode) #seed_dc::placeholder { color: #FFFFFF; }
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

      /* ── SCORE MODE RADIO (Poisson / Empirisch) ── */
      .score-mode-radio .shiny-options-group { margin-top: 2px; }
      .score-mode-radio .radio { margin: 4px 0; padding: 0; }
      .score-mode-radio .radio label {
        font-family: 'Source Sans 3', Arial, sans-serif;
        font-size: 13px; font-weight: 600; color: var(--text);
        padding-left: 24px; cursor: pointer;
      }
      .score-mode-radio input[type='radio'] {
        accent-color: var(--gold);
        margin-right: 6px;
        cursor: pointer;
      }
      body.light-mode .score-mode-radio input[type='radio'] { accent-color: #000000; }

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
      .group-table { width: 100%; border-collapse: collapse; table-layout: auto; }
      .group-table th {
        color: var(--muted); font-size: 10px; letter-spacing: 1px; text-transform: uppercase;
        padding: 6px 12px; text-align: right; font-weight: 400;
      }
      body:not(.light-mode) .group-table th { color: #F8F8F8; }
      .group-table th:first-child { text-align: left; }
      .group-table td { padding: 8px 12px; border-top: 1px solid var(--ko-border); text-align: right; }
      .group-table td:first-child { text-align: left; }
      /* Numerische Spalten (2–6: Pkt, Tore+, Tore−, Diff, ELO) kompakter darstellen */
      .group-table th:nth-child(n+2),
      .group-table td:nth-child(n+2) {
        padding-left: 4px; padding-right: 8px;
        font-variant-numeric: tabular-nums;
        white-space: nowrap;
      }
      .group-table th:last-child,
      .group-table td:last-child { padding-right: 12px; }
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
      .elo-up { color: var(--green); }
      body.light-mode .elo-up { color: #007a30; }
      .elo-dn { color: var(--red); }

      /* ── Per-Team ELO-Anpassungs-Slider in der ELO-Rangliste ── */
      /* Kompakter Shiny-Slider in jeder Tabellenzeile. */
      .elo-adj-slider {
        width: 200px;
        min-width: 180px;
        margin: 0;
      }
      .elo-adj-slider .form-group { margin: 0 !important; padding: 0 !important; }
      .elo-adj-slider .irs--shiny { margin-top: 0; margin-bottom: 0; min-height: 50px; }
      .elo-adj-slider .irs { height: 44px; top: 0; }
      .elo-adj-slider .irs-line { top: 6px; }
      .elo-adj-slider .irs-bar  { top: 6px; }
      .elo-adj-slider .irs-handle { top: -2px; }
      /* Min/Max-Labels in jeder Zeile ausblenden — Spaltentitel reicht. */
      .elo-adj-slider .irs-min,
      .elo-adj-slider .irs-max { display: none; }
      /* Aktuellen Wert UNTER den Regler legen (statt darüber), kleiner & mit Abstand. */
      .elo-adj-slider .irs-single {
        top: 26px !important;
        background: transparent !important;
        color: var(--text) !important;
        font-family: monospace;
        font-size: 10px;
        font-weight: 600;
        padding: 0 !important;
      }
      .elo-adj-slider .irs-single::before { display: none !important; }

      .elo-adj-reset-wrap {
        display: flex;
        justify-content: flex-end;
        margin: 0 0 12px 0;
      }

      /* Spalte 'ELO-Änderung': Differenz aus Slider und Start-ELO,
         rein lesend, grün/rot gefärbt mit Vorzeichen. */
      .elo-user-delta { font-family: monospace; font-weight: 600; }

      /* ── EINSTELLUNGEN-BOX (permanent ausgeklappt) ── */
      .settings-box {
        margin: 0 0 20px 0;
        border: 1px solid var(--border);
        border-radius: 10px;
        background: var(--panel);
        position: relative;
        z-index: 50;          /* über Podium, damit Dropdown nicht verdeckt wird */
      }
      /* Header zeigt Titel links, Reset-Button rechts in derselben Zeile. */
      .settings-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 12px;
        padding: 8px 16px;
        font-weight: 600;
        font-size: 13px;
        letter-spacing: 1px;
        text-transform: uppercase;
        color: var(--text);
        border-bottom: 1px solid var(--border);
        background: var(--input-bg);
        border-radius: 10px 10px 0 0;
      }
      .settings-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
        gap: 14px 18px;
        padding: 16px;
      }
      .settings-grid-fixed {
        grid-template-columns: repeat(auto-fill, 200px);
      }
      .settings-grid .control-group { margin: 0; }
      .settings-box .tabbable + .settings-header { border-radius: 0; }
      .settings-box > .tabbable > .tab-content {
        display: none;
      }
      /* Sekundärer Reset-Button: dezent, klar als Rückgängig-Aktion erkennbar.
         Im Header kompakter, damit die Headerhöhe nicht aufgebläht wird. */
      .btn-reset {
        background: transparent;
        color: #000000;
        border: 1px solid var(--border);
        padding: 6px 14px;
        border-radius: 6px;
        font-family: 'Source Sans 3', Arial, sans-serif;
        font-size: 11px;
        font-weight: 600;
        letter-spacing: 1px;
        text-transform: uppercase;
        cursor: pointer;
        transition: background 0.15s, color 0.15s, border-color 0.15s;
      }
      .btn-reset:hover {
        background: var(--input-bg);
        color: #000000;
        border-color: #000000;
      }
      /* Hilfe-Links im Settings-Header (Desktop). Container links für Titel + Links;
         Reset-Button bleibt rechts. */
      .settings-header-left {
        display: flex;
        align-items: center;
        gap: 14px;
        flex: 1;
        flex-wrap: wrap;
      }
      .help-link {
        font-family: 'Source Sans 3', Arial, sans-serif;
        font-size: 11px;
        font-weight: 600;
        letter-spacing: 1px;
        text-transform: uppercase;
        color: var(--text);
        text-decoration: none;
        padding: 4px 10px;
        border: 1px solid var(--border);
        border-radius: 6px;
        transition: background 0.15s, border-color 0.15s, color 0.15s;
      }
      .help-link:hover {
        background: var(--input-bg);
        border-color: #000000;
        color: #000000;
        text-decoration: none;
      }
      /* Mobile-Hilfeknöpfe über/unter SIMULIEREN. Standardmäßig versteckt,
         im @media-Block für schmale Bildschirme aktiviert. */
      .help-link-mobile {
        display: none;
        justify-content: center;
        margin: 0 0 12px 0;
      }
      .help-link-mobile.below { margin: 0 0 24px 0; }
      /* Disclaimer am Fuß der gesamten App. */
      .disclaimer {
        margin: 40px 0 24px 0;
        padding: 20px 0 0 0;
        border-top: 1px solid var(--border);
        color: var(--muted);
        font-size: 11px;
        line-height: 1.55;
      }
      .disclaimer-title {
        font-size: 12px;
        font-weight: 700;
        letter-spacing: 1.5px;
        text-transform: uppercase;
        margin: 0 0 8px 0;
        color: var(--text);
      }
      .disclaimer p { margin: 0 0 8px 0; }
      /* Zentraler Simulieren-Button außerhalb der Einstellungsbox */
      .run-section {
        display: flex;
        justify-content: center;
        margin: 0 0 24px 0;
      }
      .run-section .btn { min-width: 220px; }
      /* Selectize-Dropdown muss über dem Podium liegen */
      .selectize-dropdown { z-index: 9999 !important; }
      .selectize-control  { position: relative; z-index: 100; }
      /* Selectize-Felder/Dropdowns an das Theme anpassen.
         Standard-Selectize ist immer weiß — wir überschreiben mit CSS-Variablen,
         damit im Dark Mode korrekt schwarz/weiß angezeigt wird. */
      .selectize-input,
      .selectize-input input,
      .selectize-dropdown,
      .selectize-dropdown-content,
      .selectize-dropdown .option {
        background: var(--input-bg) !important;
        color: var(--text) !important;
      }
      .selectize-input,
      .selectize-dropdown {
        border: 1px solid var(--border) !important;
      }
      .selectize-dropdown .option.active,
      .selectize-dropdown .option:hover {
        background: #CCFF00 !important;
        color: #000000 !important;
      }
      /* Podium darf keinen eigenen stacking context erzeugen, der das Dropdown abschneidet */
      .podium { position: relative; z-index: 1; }

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
        /* Header-Hilfe-Links auf Mobile ausblenden, Mobile-Buttons stattdessen anzeigen */
        .settings-header-left .help-link { display: none; }
        .help-link-mobile { display: flex; }
        /* Mobile-Hilfe-Buttons: exakt so groß wie der SIMULIEREN-Button.
           Volle Breite, identisches Padding/Schriftgröße/letter-spacing.
           Border bleibt dünn — kennzeichnet die Buttons als sekundär. */
        .help-link-mobile .help-link {
          flex: 1;
          text-align: center;
          padding: 10px 28px;
          font-size: 18px;
          font-weight: 700;
          letter-spacing: 2px;
        }
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
        #seed_elo, #seed_dc { width: 100%; }
        .settings-grid-fixed { grid-template-columns: 1fr; }
        .podium { flex-direction: column; gap: 8px; }
        .groups-grid { grid-template-columns: 1fr; }
        .nav-tabs > li > a { font-size: 11px; padding: 8px 8px; letter-spacing: 0; }
        .group-table td, .group-table th { padding: 5px 6px; font-size: 12px; }
        .ko-table td, .ko-table th { padding: 5px 8px; font-size: 12px; }
        .tab-content { padding: 12px; }
        .settings-box > .tabbable > .tab-content { display: none; }
        .podium { flex-direction: column; gap: 8px; }
        .podium-card.first  { order: 1; }
        .podium-card.second { order: 2; }
        .podium-card.third  { order: 3; }
        /* Slider in der ELO-Rangliste auf Mobile schmaler */
        .elo-adj-slider { width: 140px; min-width: 130px; }
        /* ELO-Rangliste auf Mobile: nur Platz, Team, Slider, End-ELO zeigen.
           Spalten 3 (Start-ELO) und 5 (Δ Turnier) werden ausgeblendet,
           damit die Tabelle nicht über den Bildschirmrand hinausläuft. */
        .elo-rangliste-table th:nth-child(3),
        .elo-rangliste-table td:nth-child(3),
        .elo-rangliste-table th:nth-child(5),
        .elo-rangliste-table td:nth-child(5) { display: none; }
      }
    "))
  ),

  div(class="loading-overlay", id="loader",
      div(class="spinner"),
      div(class="loading-text", "Turnier wird simuliert...")
  ),

  div(class="wc-header",
      div(class="header-inner",
          div(class="header-branding",
              p(class="wc-subtitle", "ELO- und Dixon-Coles-Simulation"),
              h1(class="wc-title", "🏆 FIFA WM 2026")
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

      # ── EINSTELLUNGEN (permanent ausgeklappt) ───────────────────
      div(class="settings-box",
        tabsetPanel(
          id = "prediction_model",
          type = "tabs",
          tabPanel("ELO", value = "elo"),
          tabPanel("Dixon-Coles", value = "dixon_coles")
        ),

        # Header mit Titel links und Zurücksetzen-Button rechts in derselben Zeile.
        div(class="settings-header",
            div(class="settings-header-left",
                span(class="settings-header-title", "⚙️  Einstellungen"),
                tags$a(href="https://www.wiwiss.fu-berlin.de/wm2026/Einstellungen/index.html",
                       target="_blank", class="help-link",
                       "ℹ️ Einstellungshilfe"),
                tags$a(href="https://www.wiwiss.fu-berlin.de/wm2026/Prognose_Methodik/index.html",
                       target="_blank", class="help-link",
                       "📐 Methodik")
            ),
            actionButton("reset_btn", "↺  Zurücksetzen", class="btn-reset")
        ),

        conditionalPanel(
          condition = "input.prediction_model == 'elo'",
          div(class="settings-grid",
              # Zufallsgenerator Startwert (vormals Random Seed)
              div(class="control-group",
                  div(class="control-label", "Startwert Zufallsgenerator"),
                  tags$input(id="seed_elo", type="number", value="", min="1", max="99999",
                             class="form-control", placeholder="zufällig")
              ),

              # Turnier Elo Gewicht (vormals K-Factor / Lerngeschwindigkeit)
              div(class="control-group",
                  div(class="control-label", "ELO-Faktor Turnier"),
                  sliderInput("k_slider", label=NULL, min=0, max=100, value=60, step=5,
                              ticks=FALSE, width="100%")
              ),

              # Tor-Modell als Radio-Buttons (klassisch, sofort sichtbar welche Option aktiv ist)
              div(class="control-group score-mode-radio",
                  div(class="control-label", "Tor-Modell"),
                  radioButtons("use_historical", label = NULL,
                               choices = c("Poisson"    = "0",
                                           "Empirisch"  = "1"),
                               selected = "0",
                               inline = FALSE)
              ),

              # Außenseiter-Faktor
              div(class="control-group",
                  div(class="control-label", "Außenseiter-Faktor"),
                  sliderInput("upset_factor", label=NULL,
                              min=-3, max=3, value=0, step=0.1,
                              ticks=FALSE, width="100%")
              ),

              # Torreichtum
              div(class="control-group",
                  div(class="control-label", "Torreichtum"),
                  sliderInput("goal_scale", label=NULL,
                              min=0.3, max=2.0, value=1.0, step=0.1,
                              ticks=FALSE, width="100%")
              ),

              # Unentschieden-Häufigkeit (max)
              # Slider-Wert ist direkt die max. Wahrscheinlichkeit eines
              # Unentschieden bei ELO-Gleichstand. Default 1/3 entspricht dem
              # bisherigen Verhalten der App.
              div(class="control-group",
                  div(class="control-label", "Unentschieden bei gleichem Elo"),
                  sliderInput("draw_max", label=NULL,
                              min=0, max=1.0, value=.3, step=0.01,
                              ticks=FALSE, width="100%")
              ),

              # Team-Boost: Auswahl
              div(class="control-group",
                  div(class="control-label", "ELO Änderung für …"),
                  selectInput("team_boost_id", label=NULL,
                              choices = setNames(
                                teams_init$id,
                                paste(sapply(teams_init$fifa_code, get_flag),
                                      teams_init$team_name_de)
                              ),
                              selected = (teams_init %>%
                                            filter(fifa_code == "GER") %>%
                                            pull(id))[1],
                              width="100%")
              ),

              # Team-Boost: ELO-Differenz
              div(class="control-group",
                  div(class="control-label", "… ELO  Änderung"),
                  sliderInput("team_boost_value", label=NULL,
                              min=-500, max=500, value=0, step=10,
                              ticks=FALSE, width="100%")
              )
          )
        ),

        conditionalPanel(
          condition = "input.prediction_model == 'dixon_coles'",
          div(class="settings-grid",
              # Zufallsgenerator Startwert
              div(class="control-group",
                  div(class="control-label", "Startwert Zufallsgenerator"),
                  tags$input(id="seed_dc", type="number", value="", min="1", max="99999",
                             class="form-control", placeholder="zufällig")
              ),
              
              # Heimvorteil USA / Kanada / Mexiko: Multiplikator auf den
              # host-Koeffizienten des Modells. 1 = original, 0 = aus, > 1 = verstärkt.
              div(class="control-group",
                  div(class="control-label", "Heimvorteil 🇺🇸🇨🇦🇲🇽 (×)"),
                  sliderInput("dc_host_factor", label=NULL,
                              min=0, max=5, value=1, step=0.1,
                              ticks=FALSE, width="100%")
              ),
              
              # Torniveau / Intercept (Δ)
              div(class="control-group",
                  div(class="control-label", "Torniveau (Δ)"),
                  sliderInput("dc_intercept_delta", label=NULL,
                              min=-0.5, max=0.5, value=0, step=0.05,
                              ticks=FALSE, width="100%")
              ),
              
          )
        )
      ),

      # ── Mobile: Einstellungshilfe-Button (nur auf schmalen Bildschirmen sichtbar) ──
      div(class="help-link-mobile",
          tags$a(href="https://www.wiwiss.fu-berlin.de/wm2026/Einstellungen/index.html",
                 target="_blank", class="help-link",
                 "ℹ️ Einstellungshilfe")
      ),
      # ── Simulieren-Button (zentral, außerhalb der Box) ─────────
      div(class="run-section",
          actionButton("run_btn", "▶  SIMULIEREN", class="btn")
      ),
      # ── Mobile: Methodik-Button (nur auf schmalen Bildschirmen sichtbar) ──
      div(class="help-link-mobile below",
          tags$a(href="https://www.wiwiss.fu-berlin.de/wm2026/Methodik/index.html",
                 target="_blank", class="help-link",
                 "📐 Methodik")
      ),
      uiOutput("podium_ui"),

      tabsetPanel(id="main_tabs",
                  tabPanel("🏅 Gruppenübersicht", div(style="margin-top:16px;", uiOutput("groups_ui"))),
                  tabPanel("⚽ Gruppenspiele",   div(style="margin-top:16px;", uiOutput("group_matches_ui"))),
                  tabPanel("⚔️ K.O.-Runde",      div(style="margin-top:16px;", uiOutput("ko_ui"))),
                  tabPanel("⚙️ Mannschaftseinstellung", div(style="margin-top:16px;", uiOutput("elo_ui")))
      ),

      # ── Haftungsausschluss am Fuß der App ───────────────────────
      div(class="disclaimer",
          div(class="disclaimer-title", "Hinweis"),
          tags$p("Die hier gezeigte Simulation dient ausschließlich Bildungs- und Unterhaltungszwecken im Rahmen der Lehre am Fachbereich Wirtschaftswissenschaft der Freien Universität Berlin. Sie basiert auf statistischen Modellen (ELO-Bewertungen, Dixon-Coles, Monte-Carlo-Verfahren) und stellt weder eine sportliche Prognose noch eine Empfehlung dar – insbesondere nicht im Hinblick auf Sportwetten oder andere Formen des Glücksspiels."),
          tags$p("Die Freie Universität Berlin und der Fachbereich Wirtschaftswissenschaft übernehmen keine Haftung für Entscheidungen, die auf Grundlage dieser Simulation getroffen werden, oder für daraus resultierende Schäden gleich welcher Art. Die Nutzung erfolgt auf eigene Verantwortung.")
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

    /* ── Spinner ── */
    $(document).on('click', '#run_btn', function() {
      $('#loader').addClass('active');
    });
    $(document).on('shiny:idle', function() {
      $('#loader').removeClass('active');
    });

    /* ── Reset-Handler: setzt Seed-Felder zurück ── */
    /* (Slider/Select/Radio werden vom Server via update*Input zurückgesetzt) */
    $(document).on('shiny:connected', function() {
      Shiny.addCustomMessageHandler('resetUI', function(message) {
        document.getElementById('seed_elo').value = '';
        document.getElementById('seed_dc').value = '';
      });
    });
  "))
)

# ── SERVER ───────────────────────────────────────────────────

server <- function(input, output, session) {
  # ── Session-Tracking ──
  # Schreibt "start" beim Verbindungsaufbau und "end" beim Schließen
  # der Session in die TSV-Datei (Setup oben in der Datei).
  log_session_event("start", session$token)
  session$onSessionEnded(function() {
    log_session_event("end", session$token)
  })

  result <- reactiveVal(NULL)

  # ── Persistenter Speicher für die Per-Team-ELO-Slider ──
  # Wir cachen die zuletzt vom User eingestellten Slider-Werte (Slider-Wert
  # = gewünschte Start-ELO im Bereich 1000–2500) in einem reactiveValues-Container.
  # So überleben die Einstellungen jedes Re-Rendering der ELO-Tabelle, ohne
  # auf das (beim Re-Render unzuverlässige) input$adj_<id> angewiesen zu sein.
  adj_state <- reactiveValues(
    values = setNames(as.list(round(teams_init$elo)),
                      as.character(teams_init$id))
  )

  # ── Sync: Slider-Veränderungen → reactiveValues ──
  # Wird einmal pro Team registriert. Jeder observeEvent feuert nur, wenn der
  # zugehörige Slider tatsächlich (vom User) verändert wurde.
  lapply(teams_init$id, function(tid) {
    input_id <- paste0("adj_", tid)
    observeEvent(input[[input_id]], {
      adj_state$values[[as.character(tid)]] <- as.numeric(input[[input_id]])
    }, ignoreInit = TRUE)
  })
  
  # ── Dixon-Coles: persistente Slider-States für Angriff & Abwehr (absolut, 0–2.5) ──
  # Slider zeigen exp(attack_log) bzw. exp(defense_log) — d.h. 1.0 = neutral.
  # Init aus dem gefitteten Modell, damit untouched = Originalverhalten.
  # Fallback auf 1.0 (neutraler Wert), falls Modell oder Team-ID nicht verfügbar.
  dc_init_values <- local({
    n <- nrow(teams_init)
    att_default <- setNames(as.list(rep(0, n)), as.character(teams_init$id))
    def_default <- setNames(as.list(rep(0, n)), as.character(teams_init$id))
    if (dc_available()) {
      tryCatch({
        fit <- load_dc_model()
        p_a <- fit$parameters$attack
        p_d <- fit$parameters$defense
        mu_fit <- as.numeric(fit$parameters$intercept %||% 0)
        for (idc in as.character(teams_init$id)) {
          va <- p_a[idc]; vd <- p_d[idc]
          # Slider-Init = direkter Modellparameter (log-Skala, Range ~−3 bis +1.5).
          if (length(va) > 0 && !is.na(va)) att_default[[idc]] <- round(as.numeric(va), 2)
          if (length(vd) > 0 && !is.na(vd)) def_default[[idc]] <- round(as.numeric(vd), 2)
        }
      }, error = function(e) {
        message("DC-Initialisierung fehlgeschlagen: ", conditionMessage(e))
      })
    }
    list(attack = att_default, defense = def_default)
  })
  dc_att_state <- reactiveValues(values = dc_init_values$attack)
  dc_def_state <- reactiveValues(values = dc_init_values$defense)
  lapply(teams_init$id, function(tid) {
    att_id <- paste0("dc_att_", tid)
    def_id <- paste0("dc_def_", tid)
    observeEvent(input[[att_id]], {
      dc_att_state$values[[as.character(tid)]] <- as.numeric(input[[att_id]])
    }, ignoreInit = TRUE)
    observeEvent(input[[def_id]], {
      dc_def_state$values[[as.character(tid)]] <- as.numeric(input[[def_id]])
    }, ignoreInit = TRUE)
  })
  
  # Run on startup with defaults (entspricht ursprünglichem Verhalten)
  # Wichtig: seed = 111 mit default_params ist deterministisch und für ALLE Sessions
  # identisch. Wir berechnen das genau einmal pro R-Prozess und teilen das Ergebnis —
  # spart bei N parallelen Session-Starts (N−1) Simulationen.
  observe({ result(default_result_cache()) })

  observeEvent(input$run_btn, {
    seed_input <- if (identical(input$prediction_model, "dixon_coles")) {
      input$seed_dc
    } else {
      input$seed_elo
    }
    seed <- suppressWarnings(as.integer(seed_input))
    if (is.na(seed)) seed <- as.integer(as.numeric(Sys.time())) %% .Machine$integer.max

    # Per-Team-ELO-Adjustments aus dem persistenten Cache einsammeln und in
    # Differenzen zur Original-Start-ELO umrechnen, weil apply_elo_modifiers
    # die Werte additiv aufschlägt. Auf [-500, 500] geclamped als Schutz vor
    # extremen Eingaben.
    orig_lookup <- setNames(teams_init$elo, as.character(teams_init$id))
    adj_vec <- sapply(teams_init$id, function(tid) {
      key   <- as.character(tid)
      slv   <- adj_state$values[[key]]
      if (is.null(slv) || is.na(slv)) return(0)
      diff <- as.numeric(slv) - as.numeric(orig_lookup[[key]])
      max(-500, min(500, diff))
    })
    names(adj_vec) <- as.character(teams_init$id)

    # DC-Per-Team-Werte aus den reactiveValues einsammeln.
    # Werte sind absolute Slider-Stände auf 0–2.5 Skala (exp(attack_log)).
    dc_att_vec <- sapply(teams_init$id, function(tid) {
      v <- dc_att_state$values[[as.character(tid)]]
      if (is.null(v) || is.na(v)) 1.0 else as.numeric(v)
    })
    dc_def_vec <- sapply(teams_init$id, function(tid) {
      v <- dc_def_state$values[[as.character(tid)]]
      if (is.null(v) || is.na(v)) 1.0 else as.numeric(v)
    })
    names(dc_att_vec) <- as.character(teams_init$id)
    names(dc_def_vec) <- as.character(teams_init$id)
    
    # UI-Werte in params-Liste übersetzen.
    # Alle Defaults entsprechen dem Originalverhalten der App.
    user_params <- modifyList(default_params, list(
      k                      = as.integer(input$k_slider %||% 60),
      use_historical         = isTRUE(input$use_historical == "1"),
      home_advantage         = as.numeric(input$home_advantage   %||% 0),
      team_boost_id          = as.integer(input$team_boost_id    %||% NA),
      team_boost_value       = as.numeric(input$team_boost_value %||% 0),
      team_adjustments       = adj_vec,
      goal_scale             = as.numeric(input$goal_scale       %||% 1.0),
      draw_max               = as.numeric(input$draw_max         %||% (.3)),
      upset_factor           = 1 - as.numeric(input$upset_factor %||% 0),
      prediction_model       = input$prediction_model %||% "elo",
      dc_host_factor         = as.numeric(input$dc_host_factor     %||% 1),
      dc_intercept_delta     = as.numeric(input$dc_intercept_delta %||% 0),
      dc_attack_values       = dc_att_vec,
      dc_defense_values      = dc_def_vec
    ))

    # Asynchrone Berechnung im Hintergrund-Worker. Der R-Hauptprozess
    # bleibt frei für andere Sessions, während diese Simulation läuft.
    # Lokale Kopien, damit der Future garantiert die richtigen Werte
    # captured (defensive Programmierung gegen Lazy-Evaluation-Überraschungen).
    local_seed   <- seed
    local_params <- user_params
    future_promise({
      run_tournament(seed = local_seed, params = local_params)
    }, seed = TRUE) %...>%
      result() %...!%
      (function(e) {
        showNotification(
          paste("Simulation fehlgeschlagen:", conditionMessage(e)),
          type = "error", duration = 10
        )
      })
    # WICHTIG: NULL zurückgeben, damit observeEvent keinen Promise zurückgibt
    # (Shiny würde sonst eine Warnung loggen).
    NULL
  })
  # ── Zurücksetzen-Button: alle Einstellungen auf Default ──
  # Default-Werte stammen aus default_params (siehe Datei-Anfang).
  # JS-Anteil (Seed-Feld leeren) läuft über Custom Message,
  # weil das Seed-Feld kein klassisches Shiny-Input ist.
  # Die Per-Team-Slider werden bewusst NICHT mit zurückgesetzt — dafür gibt es
  # einen eigenen Button in der ELO-Rangliste, weil sie konzeptuell dorthin gehören.
  observeEvent(input$reset_btn, {
    # Modus-abhängig: nur die Slider des aktuell sichtbaren Tabs zurücksetzen.
    # Insbesondere NICHT den Tab wechseln — der User bleibt da wo er ist.
    current_mode <- input$prediction_model %||% "elo"
    
    if (identical(current_mode, "elo")) {
      updateSliderInput(session, "k_slider",           value = default_params$k)
      updateSliderInput(session, "home_advantage",     value = default_params$home_advantage)
      updateSliderInput(session, "upset_factor",       value = 0)
      updateSliderInput(session, "goal_scale",         value = default_params$goal_scale)
      updateSliderInput(session, "draw_max",           value = default_params$draw_max)
      updateSliderInput(session, "team_boost_value",   value = default_params$team_boost_value)
      updateRadioButtons(session, "use_historical",    selected = "0")
      updateSelectInput(session, "team_boost_id",
                        selected = (teams_init %>%
                                      filter(fifa_code == "GER") %>%
                                      pull(id))[1])
    } else if (identical(current_mode, "dixon_coles")) {
      updateSliderInput(session, "dc_host_factor",     value = default_params$dc_host_factor)
      updateSliderInput(session, "dc_intercept_delta", value = default_params$dc_intercept_delta)
    }
    session$sendCustomMessage("resetUI", list())
  })

  # ── Per-Team-ELO-Slider: Reset-Button im ELO-Tab ──
  # Setzt alle Slider AUF die jeweilige Original-Start-ELO zurück,
  # womit alle Anpassungen wieder 0 sind.
  # Greift wirksam erst beim nächsten "Simulieren"-Klick (wie alle anderen Einstellungen).
  observeEvent(input$reset_adj_btn, {
    for (i in seq_len(nrow(teams_init))) {
      tid  <- teams_init$id[i]
      orig <- round(teams_init$elo[i])
      updateSliderInput(session, paste0("adj_", tid), value = orig)
      adj_state$values[[as.character(tid)]] <- orig
    }
  })
  
  # ── DC-Per-Team-Slider: Reset-Button in der Rangliste (DC-Modus) ──
  # Setzt alle Angriffs-Δ und Abwehr-Δ Slider auf 0 zurück.
  observeEvent(input$reset_dc_btn, {
    # Reset = zurück zu den exp(Originalwerten) des gefitteten Modells.
    fit_full <- if (dc_available()) tryCatch(load_dc_model(), error = function(e) NULL) else NULL
    p_att <- if (dc_available()) tryCatch(load_dc_model()$parameters$attack,  error = function(e) NULL) else NULL
    p_def <- if (dc_available()) tryCatch(load_dc_model()$parameters$defense, error = function(e) NULL) else NULL
    
    for (i in seq_len(nrow(teams_init))) {
      tid <- teams_init$id[i]
      idc <- as.character(tid)
      av <- if (!is.null(p_att)) p_att[idc] else NA
      dv <- if (!is.null(p_def)) p_def[idc] else NA
      a_init <- if (length(av) > 0 && !is.na(av)) round(as.numeric(av), 2) else 0
      d_init <- if (length(dv) > 0 && !is.na(dv)) round(as.numeric(dv), 2) else 0
      updateSliderInput(session, paste0("dc_att_", tid), value = a_init)
      updateSliderInput(session, paste0("dc_def_", tid), value = d_init)
      dc_att_state$values[[idc]] <- a_init
      dc_def_state$values[[idc]] <- d_init
    }
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
          div(class="podium-team", team$team_name_de),
          div(style="color:var(--muted);font-size:11px;margin-top:4px;",
              paste("ELO:", round(elo_val)))
      )
    }

    tagList(div(class="podium",
                make_card(runner, "second", "Zweiter Platz", "🥈"),
                make_card(champ,  "first",  "Weltmeister",   "🏆"),
                make_card(third,  "third",  "Dritter Platz", "🥉")
    ))
  })

  # ── Gruppen-Tabellen ──
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
                        get_flag(row$fifa_code), " ", row$team_name_de),
                tags$td(class="pts-cell", row$pts),
                tags$td(row$gf), tags$td(row$ga),
                tags$td(class=gd_cls, gd_str),
                tags$td(style="color:var(--muted);font-size:12px;", round(row$elo))
        )
      })
      div(class="group-card",
          div(class="group-header", paste("GRUPPE", grp)),
          tags$table(class="group-table",
                     tags$thead(tags$tr(
                       tags$th("Team"), tags$th("Pkt"), tags$th("Tore+"),
                       tags$th("Tore−"), tags$th("Diff"), tags$th("ELO")
                     )),
                     tags$tbody(rows)
          )
      )
    })
    div(class="groups-grid", cards)
  })

  # ── Gruppenspiele ──
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
          div(class="ko-round-title", paste("Gruppe", grp)),
          tags$table(class="ko-table",
                     tags$thead(tags$tr(
                       tags$th("Heim"), tags$th(style="text-align:center","Ergebnis"), tags$th("Auswärts")
                     )),
                     tags$tbody(rows)
          )
      )
    })
    div(sections)
  })

  # ── K.O.-Runde ──
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
          div(class="ko-round-title", stage_de_map[rnd]),
          tags$table(class="ko-table",
                     tags$thead(tags$tr(
                       tags$th("Heim / Team A"), tags$th(style="text-align:center","Ergebnis"),
                       tags$th("Auswärts / Team B"), tags$th("Sieger")
                     )),
                     tags$tbody(rows)
          )
      )
    })
    div(sections)
  })

  # ── ELO-Rangliste ──
  # Spaltenreihenfolge: # | Team | Start-ELO | ELO-Anpassung (Slider 1000–2500)
  #                     | ELO-Änderung (Slider − Start-ELO) | End-ELO
  #
  # Persistenz: Slider-Werte werden in adj_state$values gehalten und beim
  # Re-Render der Tabelle als Default-Wert benutzt. Beim allerersten Aufbau
  # eines Slider-Eintrags ist der Wert = Original-Start-ELO (siehe Initialisierung
  # von adj_state oben), so dass das angezeigte Delta beim ersten Mal 0 ist.
  output$elo_ui <- renderUI({
    mode <- input$prediction_model %||% "elo"
    
    # ── Dixon-Coles-Modus: Per-Team-Slider (absolut, 0–2.5; 1.0 = neutral) ──
    if (identical(mode, "dixon_coles")) {
      if (!dc_available()) {
        return(div(style="margin-top:16px;color:var(--muted);",
                   "Dixon-Coles-Modell nicht verfügbar."))
      }
      model_fit <- load_dc_model()
      p_att <- model_fit$parameters$attack
      p_def <- model_fit$parameters$defense
      
      # Sortierung nach Originalstärke (attack − defense auf log-Skala).
      dc_df <- teams_init %>%
        mutate(
          id_chr      = as.character(id),
          flag        = sapply(fifa_code, get_flag),
          attack_log  = as.numeric(p_att[id_chr]),
          defense_log = as.numeric(p_def[id_chr])
        ) %>%
        filter(!is.na(attack_log) & !is.na(defense_log)) %>%
        arrange(desc(attack_log - defense_log))
      if (nrow(dc_df) == 0) {
        return(div(style="margin-top:16px;color:var(--muted);",
                   "Keine Mannschaften mit Modellparametern gefunden."))
      }
      
      dc_rows <- lapply(seq_len(nrow(dc_df)), function(i) {
        row    <- dc_df[i,]
        idc    <- as.character(row$id)
        att_id <- paste0("dc_att_", row$id)
        def_id <- paste0("dc_def_", row$id)
        
        # Slider-Startposition: persistenter State (per-Server-Init bereits
        # mit exp(Originalwert) gefüllt). Fallback auf exp(Originalwert),
        # falls State leer ist.
        cur_att <- isolate(dc_att_state$values[[idc]])
        cur_def <- isolate(dc_def_state$values[[idc]])
        mu_fit <- as.numeric(model_fit$parameters$intercept %||% 0)
        if (is.null(cur_att) || is.na(cur_att)) cur_att <- round(row$attack_log,  2)
        if (is.null(cur_def) || is.na(cur_def)) cur_def <- round(row$defense_log, 2)
        
        tags$tr(
          tags$td(as.character(i)),
          tags$td(paste(row$flag, row$team_name_de)),
          tags$td(div(class="elo-adj-slider",
                      sliderInput(att_id, label = NULL,
                                  min = -1, max = 2.5,
                                  value = cur_att, step = 0.05,
                                  ticks = FALSE, width = "100%"))),
          tags$td(div(class="elo-adj-slider",
                      sliderInput(def_id, label = NULL,
                                  min = -1, max = 2.5,
                                  value = cur_def, step = 0.05,
                                  ticks = FALSE, width = "100%")))
        )
      })
      
      return(tagList(
        div(class="elo-adj-reset-wrap",
            actionButton("reset_dc_btn", "↺  DC-Anpassungen zurücksetzen", class="btn-reset")
        ),
        tags$table(class="ko-table elo-rangliste-table",
                   tags$thead(tags$tr(
                     tags$th("#"),
                     tags$th("Team"),
                     tags$th("Angriff"),
                     tags$th("Abwehr")
                   )),
                   tags$tbody(dc_rows)
        )
      ))
    }
    
    # ── ELO-Modus: bisherige Rangliste ──
    r <- result(); req(r)
    fe <- r$final_elo
    # Wenn beim Modus-Wechsel das letzte Simulationsergebnis aus dem DC-Lauf
    # stammt, hat es keine final_elo-Komponente. In dem Fall einen Platzhalter
    # zeigen statt zu crashen.
    if (is.null(fe) || !is.data.frame(fe) || nrow(fe) == 0) {
      return(div(style="margin-top:16px;color:var(--muted);",
                 "Bitte einmal 'Simulieren' drücken, um die ELO-Rangliste zu sehen."))
    }

    rows <- lapply(seq_len(nrow(fe)), function(i) {
      row     <- fe[i,]
      orig    <- as.numeric(row$orig_elo)        # ORIGINAL Start-ELO (vor Modifiern)
      medal   <- as.character(i)                 # Nur Platznummer, keine Medaille
      slider_id <- paste0("adj_", row$id)

      # Aktueller Slider-Wert aus dem persistenten Cache (überlebt Re-Renderings).
      # isolate(): wir wollen den aktuellen Wert beim Aufbau der Tabelle lesen,
      # aber KEINE reaktive Abhängigkeit erzeugen — sonst würde jeder
      # Slider-Schub die ganze Tabelle neu rendern.
      cur_val <- isolate(adj_state$values[[as.character(row$id)]])
      if (is.null(cur_val) || is.na(cur_val)) cur_val <- orig

      # Turnier-Δ: Differenz zwischen Endstand und Start-ELO NACH Modifiern.
      # Diese Spalte zeigt ausschließlich das Ergebnis der Simulation und ändert
      # sich nicht beim Schieben der Regler — erst wenn Simulieren neu gedrückt wird.
      tour_delta     <- as.numeric(row$change)
      tour_delta_cls <- if (tour_delta > 0) "elo-up" else if (tour_delta < 0) "elo-dn" else ""
      tour_delta_str <- if (tour_delta > 0) paste0("+", round(tour_delta)) else as.character(round(tour_delta))

      tags$tr(
        tags$td(medal),
        tags$td(paste(row$flag, row$team_name_de)),
        # Start-ELO: in Textfarbe (schwarz im Light Mode, weiß im Dark Mode)
        tags$td(style="font-family:monospace;color:var(--text);", orig),
        # ELO-Anpassung: voller Shiny-Slider in der Tabellenzelle
        tags$td(div(class="elo-adj-slider",
                    sliderInput(slider_id, label = NULL,
                                min = 1000, max = 2500,
                                value = cur_val, step = 10,
                                ticks = FALSE, width = "100%"))),
        # Turnier-Δ: rein lesend, Vorzeichen + Farbe
        tags$td(class=paste("elo-user-delta", tour_delta_cls), tour_delta_str),
        # End-ELO: nach Simulation
        tags$td(style="font-family:monospace;font-weight:600;color:var(--gold);", row$final_elo)
      )
    })

    tagList(
      div(class="elo-adj-reset-wrap",
          actionButton("reset_adj_btn", "↺  ELO-Anpassungen zurücksetzen", class="btn-reset")
      ),
      tags$table(class="ko-table elo-rangliste-table",
                 tags$thead(tags$tr(
                   tags$th("#"),
                   tags$th("Team"),
                   tags$th("Start-ELO"),
                   tags$th("ELO-Anpassung"),
                   tags$th("Δ Turnier"),
                   tags$th("End-ELO")
                 )),
                 tags$tbody(rows)
      )
    )
  })
}

shinyApp(ui, server)
