library(devtools)
library(Rcpp)
library(dplyr)
library(tidyr)
pak::pak("jalapic/engsoccerdata")
pak::pak("opisthokonta/goalmodel")
library(goalmodel)

teams <- read.csv("data/teams.csv")

test <- all_international_matches %>%
  select(1:20) %>%
  mutate(home = as.numeric(match_venue == "home"),
         host = as.numeric(match_venue == "host_home")) %>%
  mutate(home_team = case_when(home_team == "Iran" ~ "IR Iran",
                              home_team == "Bosnia &amp; Herzegovina" ~ "Bosnia and Herzegovina",
                              home_team == "Türkiye" ~ "Turkey",
                              TRUE ~ home_team),
         away_team = case_when(away_team == "Iran" ~ "Iran, Islamic Republic of",
                              away_team == "Bosnia &amp; Herzegovina" ~ "Bosnia and Herzegovina",
                              away_team == "Türkiye" ~ "Turkey",
                              TRUE ~ away_team)) %>%
  filter(!is.na(home_score), !is.na(away_score), is_international, is_senior_mens) %>%
  left_join(teams %>% select(team_name, id), by = c("home_team" = "team_name")) %>%
  left_join(teams %>% select(team_name, id), by = c("away_team" = "team_name"), suffix = c("_home", "_away")) %>%
  mutate(id_home = ifelse(is.na(id_home), home_team, id_home),
         id_away = ifelse(is.na(id_away), away_team, id_away)) %>%
  #filter(id_home %in% teams$id |id_away %in% teams$id) %>%
  mutate(date = as.Date(date))

xx1_hfa <- matrix(c(test$home, test$host), ncol = 2)
colnames(xx1_hfa) <- c("home", "host")

friendly <- ifelse(test$tournament == "Int. Friendly Games", .5, 1)

my_weights <- weights_dc(test$date, xi=0.0015)
plot(y = friendly*my_weights , x = test$date)

dc <- goalmodel(goals1 = test$home_score, goals2 = test$away_score,
                team1 = test$id_home, team2 = test$id_away,
                hfa = FALSE, weights = my_weights*friendly,
                dc = T,
                x1 = xx1_hfa)
summary(dc)

sourceCpp("dc/dc.cpp")


build_prob_map <- function(model_fit, teams_df, maxgoal = 10) {
  # Use character team names as keys — must match model's internal team names
  team_names <- teams_df$team_name
  ids        <- teams_df$id
  name_to_id <- setNames(ids, team_names)
  id_to_name <- setNames(team_names, as.character(ids))

  n          <- length(ids)
  prob_list  <- list()

  pairs <- expand.grid(h = ids, a = ids, stringsAsFactors = FALSE) %>%
    filter(h != a)

  message(sprintf("Pre-computing %d Dixon-Coles matrices...", nrow(pairs)))

  n_ok  <- 0L
  n_err <- 0L

  HOST_IDS <- c(1L, 5L, 13L)

  for (i in seq_len(nrow(pairs))) {
    h_id <- pairs$h[i]
    a_id <- pairs$a[i]

    host_home <- h_id %in% HOST_IDS
    host_away <- a_id %in% HOST_IDS
    x1 <- matrix(c(0, as.integer(host_home)), ncol = 2)
    x2 <- matrix(c(0, as.integer(host_away)), ncol = 2)
    colnames(x1) <- colnames(x2) <- c("home", "host")

    mat <- tryCatch({
      res <- predict_goals(model_fit,
                           team1    = h_id,
                           team2    = a_id,
                           maxgoal  = maxgoal,
                           return_df = FALSE,
                           x1 = x1,
                           x2 = x2)
      # predict_goals returns a list of matrices (one per match pair)
      if (is.list(res)) res[[1]] else res
    }, error = function(e) {
      message("  ERROR for ", h_nm, " vs ", a_nm, ": ", conditionMessage(e))
      NULL
    })

    if (!is.null(mat) && is.matrix(mat) && !any(is.na(mat)) && nrow(mat) > 0) {
      key <- paste0(h_id, "_", a_id)
      prob_list[[key]] <- mat
      n_ok <- n_ok + 1L
    } else {
      n_err <- n_err + 1L
    }
  }

  message(sprintf("Done. %d matrices computed, %d failed.", n_ok, n_err))

  # Diagnose if still 0
  if (n_ok == 0) {
    message("\n--- DIAGNOSIS ---")
    message("Model teams: ", paste(head(model_fit$all_teams, 5), collapse=", "), "...")
    message("Your teams_df names: ", paste(head(teams_df$team_name, 5), collapse=", "), "...")
    message("Name mismatch? Check that team_name values match model$all_teams exactly.")
  }

  prob_list
}

# ── BUILD GROUPS LIST ─────────────────────────────────────────────────────────

build_groups_list <- function(teams_df) {
  grp_letters <- sort(unique(teams_df$group_letter))
  groups_list <- setNames(
    lapply(grp_letters, function(g) {
      teams_df %>% filter(group_letter == g) %>% pull(id) %>% as.integer()
    }),
    grp_letters
  )
  groups_list
}

# ── MAIN DC MONTE CARLO ───────────────────────────────────────────────────────

run_mc_dc <- function(model_fit, teams_df, n_sims = 10000,
                      maxgoal = 10, seed_base = 42) {

  groups_list <- build_groups_list(teams_df)
  prob_list   <- build_prob_map(model_fit, teams_df, maxgoal = maxgoal)

  # Guard: fail early with a helpful message
  if (length(prob_list) == 0) {
    stop(
      "No probability matrices were computed.\n",
      "Check that teams_df$team_name exactly matches the team names in your fitted model.\n",
      "Run: intersect(teams_df$team_name, dc$all_teams) to see matches.\n",
      "Run: setdiff(teams_df$team_name, dc$all_teams) to see mismatches."
    )
  }

  message(sprintf("Running %d simulations via C++...", n_sims))

  t0 <- proc.time()

  raw <- run_mc_cpp(
    groups_list = groups_list,
    prob_list   = prob_list,
    n_sims      = n_sims,
    seed_base   = seed_base
  )

  elapsed <- (proc.time() - t0)[["elapsed"]]
  message(sprintf("Done in %.1f seconds.", elapsed))

  # Join team metadata
  reach_df <- as.data.frame(raw[names(raw) != "n_sims"]) %>%
    left_join(
      teams_df %>% select(id, team_name, fifa_code, group_letter),
      by = "id"
    ) %>%
    rename(
      `Group Winner` = GroupWinner,
      `Group Stage`   = Group.Stage,
      `Round of 32`   = Round.of.32,
      `Round of 16`   = Round.of.16,
      `Quarter-Final` = Quarter.Final,
      `Semi-Final`    = Semi.Final,
      `Final`         = Final,
      `Champion`      = Champion
    )

  list(reach_df = reach_df, n_sims = n_sims)
}

# ── SINGLE TOURNAMENT (with score inspection) ─────────────────────────────────

run_tournament_dc <- function(model_fit, teams_df,
                              maxgoal = 10, seed = NULL) {
  groups_list <- build_groups_list(teams_df)
  prob_list   <- build_prob_map(model_fit, teams_df, maxgoal = maxgoal)

  simulate_tournament_cpp(
    groups_list = groups_list,
    prob_list   = prob_list,
    seed        = if (is.null(seed)) -1L else as.integer(seed)
  )
}

results <- run_mc_dc(
  model_fit = dc,   # your fitted dixoncoles/goalmodel object
  teams_df  = teams,  # must have: id, team_name, group_letter
  n_sims    = 10000,
  maxgoal   = 10
)

results$reach_df %>%
  arrange(desc(Champion)) %>%
  select(team_name, fifa_code, group_letter,
         `Round of 32`, `Group Winner`, `Round of 16`,
         `Quarter-Final`, `Semi-Final`, Final, Champion)


