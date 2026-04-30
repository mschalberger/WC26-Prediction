#!/usr/bin/env Rscript
# ============================================================
# update_results.R
# Zieht beendete Spiele der WM 2026 von football-data.org
# und schreibt data/cache/results.tsv (atomar, idempotent).
# Aufruf via cron, ~15 Minuten nach geplantem Spielende.
# ============================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
})

# ── CONFIG ───────────────────────────────────────────────────
data_dir       <- "/srv/shiny-server/wm2026/data"
teams_path     <- file.path(data_dir, "teams.csv")
results_path   <- file.path(data_dir, "cache", "results.tsv")
overrides_path <- file.path(data_dir, "team_name_map.tsv")  # optional
log_path       <- file.path(data_dir, "cache", "update_results.log")

# football-data.org stage codes → app.R stage labels.
# WM 2026 hat eine Runde der letzten 32 — dafür gibt es bei
# football-data.org noch keinen verifizierten Stage-Code. LAST_32
# ist die naheliegendste Annahme; wenn es anders heißt, taucht das
# im Log unter "unknown stage" auf und du ergänzt hier.
stage_map <- c(
  "GROUP_STAGE"     = "Group",
  "LAST_32"         = "Round of 32",
  "LAST_16"         = "Round of 16",
  "QUARTER_FINALS"  = "Quarter-Final",
  "SEMI_FINALS"     = "Semi-Final",
  "THIRD_PLACE"     = "Third Place",
  "FINAL"           = "Final"
)

# ── HELPERS ──────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

log_msg <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
                paste0(..., collapse = ""))
  message(msg)
  cat(msg, "\n", file = log_path, append = TRUE)
}

# Spiegelt das Namens-Mapping aus app.R, damit API-Namen zur teams.csv passen
normalize_team_name <- function(x) {
  x <- gsub("^\\s+|\\s+$", "", x)
  case_when(
    x == "United States"                ~ "USA",
    x == "Iran"                         ~ "IR Iran",
    x == "Cape Verde"                   ~ "Cabo Verde",
    x == "Ivory Coast"                  ~ "Côte d'Ivoire",
    x == "Democratic Republic of Congo" ~ "DR Congo",
    TRUE ~ x
  )
}

load_overrides <- function() {
  if (file.exists(overrides_path)) {
    read.delim(overrides_path, sep = "\t", header = TRUE,
               stringsAsFactors = FALSE)
  } else {
    data.frame(fdorg_name = character(), team_name = character(),
               stringsAsFactors = FALSE)
  }
}

read_token <- function() {
  tok <- Sys.getenv("FOOTBALL_DATA_TOKEN")
  if (tok != "") return(tok)
  f <- path.expand("~/.football_data_token")
  if (file.exists(f)) return(trimws(readLines(f, n = 1, warn = FALSE)))
  stop("Kein API-Token. FOOTBALL_DATA_TOKEN setzen oder ~/.football_data_token anlegen.")
}

# ── FETCH ────────────────────────────────────────────────────
fetch_wc_matches <- function(token) {
  url <- "https://api.football-data.org/v4/competitions/WC/matches"
  resp <- tryCatch(
    GET(url, add_headers(`X-Auth-Token` = token), timeout(30)),
    error = function(e) { log_msg("HTTP error: ", conditionMessage(e)); NULL })
  if (is.null(resp)) return(NULL)
  if (status_code(resp) != 200) {
    log_msg("API status ", status_code(resp), ": ",
            substr(content(resp, "text", encoding = "UTF-8"), 1, 200))
    return(NULL)
  }
  fromJSON(content(resp, "text", encoding = "UTF-8"),
           simplifyVector = FALSE)$matches
}

# ── BUILD ROW ────────────────────────────────────────────────
build_row <- function(m, teams_df, overrides) {
  status <- m$status %||% ""
  if (!status %in% c("FINISHED", "AWARDED")) return(NULL)
  
  api_stage  <- m$stage %||% ""
  stage_name <- stage_map[api_stage]
  if (is.na(stage_name)) {
    log_msg("Skip match ", m$id, ": unknown stage '", api_stage, "'")
    return(NULL)
  }
  
  resolve_id <- function(api_name) {
    api_name <- normalize_team_name(api_name)
    o <- overrides[overrides$fdorg_name == api_name, "team_name"]
    if (length(o) > 0) api_name <- o[1]
    hit <- teams_df[teams_df$team_name == api_name, "id"]
    if (length(hit) == 0) return(NA_integer_)
    hit[1]
  }
  
  hid <- resolve_id(m$homeTeam$name %||% "")
  aid <- resolve_id(m$awayTeam$name %||% "")
  if (is.na(hid) || is.na(aid)) {
    log_msg("Skip match ", m$id, " (", m$homeTeam$name, " vs ",
            m$awayTeam$name, "): team not in teams.csv")
    return(NULL)
  }
  
  ft <- m$score$fullTime
  hg <- ft$home %||% NA
  ag <- ft$away %||% NA
  if (is.na(hg) || is.na(ag)) {
    log_msg("Skip match ", m$id, ": no fullTime score")
    return(NULL)
  }
  
  pens_winner_id <- NA_integer_
  if ((m$score$duration %||% "REGULAR") == "PENALTY_SHOOTOUT") {
    w <- m$score$winner %||% ""
    pens_winner_id <- if (w == "HOME_TEAM") hid
    else if (w == "AWAY_TEAM") aid
    else NA_integer_
  }
  
  data.frame(stage = stage_name, home_id = hid, away_id = aid,
             home_goals = as.integer(hg), away_goals = as.integer(ag),
             pens_winner_id = pens_winner_id, stringsAsFactors = FALSE)
}

# ── MAIN ─────────────────────────────────────────────────────
main <- function() {
  log_msg("Fetching WC matches…")
  token   <- read_token()
  matches <- fetch_wc_matches(token)
  if (is.null(matches)) {
    log_msg("Aborting: fetch failed; results.tsv unchanged.")
    quit(status = 1)
  }
  
  teams_df  <- read.csv(teams_path, stringsAsFactors = FALSE)
  overrides <- load_overrides()
  
  rows <- lapply(matches, build_row,
                 teams_df = teams_df, overrides = overrides)
  rows <- rows[!sapply(rows, is.null)]
  
  df <- if (length(rows) == 0) {
    data.frame(stage=character(), home_id=integer(), away_id=integer(),
               home_goals=integer(), away_goals=integer(),
               pens_winner_id=integer(), stringsAsFactors=FALSE)
  } else do.call(rbind, rows)
  
  log_msg("Found ", nrow(df), " finished matches.")
  
  tmp <- paste0(results_path, ".tmp")
  write.table(df, tmp, sep = "\t", row.names = FALSE,
              quote = FALSE, na = "NA")
  file.rename(tmp, results_path)
  log_msg("results.tsv updated.")
}

main()