pak::pak("jalapic/engsoccerdata")
pak::pak("opisthokonta/goalmodel")

library(devtools)
library(Rcpp)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(goalmodel)

teams <- read.csv("data/teams.csv")
all_international_matches <- read_csv("data/all_international_matches.csv")

test <- all_international_matches %>%
  select(1:20) %>%
  mutate(home = as.numeric(match_venue == "home"),
         host_home = as.numeric(match_venue == "host_home"),
         host_away = as.numeric(match_venue == "host_away"),
         friendly = as.numeric(tournament == "Int. Friendly Games")) %>%
  mutate(home_team = case_when(home_team == "Iran" ~ "IR Iran",
                               home_team == "Bosnia &amp; Herzegovina" ~ "Bosnia and Herzegovina",
                               home_team == "Türkiye" ~ "Turkey",
                               TRUE ~ home_team),
         away_team = case_when(away_team == "Iran" ~ "Iran, Islamic Republic of",
                               away_team == "Bosnia &amp; Herzegovina" ~ "Bosnia and Herzegovina",
                               away_team == "Türkiye" ~ "Turkey",
                               TRUE ~ away_team)) %>%
  filter(!is.na(home_score), !is.na(away_score), is_international, is_senior_mens) %>%
  #left_join(teams %>% select(team_name, id), by = c("home_team" = "team_name")) %>%
  #left_join(teams %>% select(team_name, id), by = c("away_team" = "team_name"), suffix = c("_home", "_away")) %>%
  #mutate(id_home = ifelse(is.na(id_home), home_team, id_home),
  #       id_away = ifelse(is.na(id_away), away_team, id_away)) %>%
  mutate(date = as.Date(date))

xx1 <- matrix(c(test$home, test$host_home), ncol = 2)
colnames(xx1) <- c("home", "host")

xx2 <- matrix(c(test$host_away), ncol = 1)
colnames(xx2) <- c("host")

friendly <- ifelse(test$tournament == "Int. Friendly Games", .5, 1)

my_weights <- weights_dc(test$date, xi = 0.0015) * friendly

dc <- goalmodel(goals1 = test$home_score, goals2 = test$away_score,
                team1 = test$home_team, team2 = test$away_team,
                hfa = FALSE, weights = my_weights,
                dc = T,
                x1 = xx1,
                x2 = xx2)
summary(dc)

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
    away_code = V5
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

host_teams <- c("United States", "Mexico", "Canada")
xx1 <- matrix(c(rep(0, nrow(fixtures)),
                ifelse(fixtures$home_name %in% host_teams, 1, 0)),ncol = 2)

xx2 <- matrix(c(ifelse(fixtures$home_name %in% host_teams, 1, 0)), ncol = 1)

pred <- predict_goals(dc, team1 = fixtures$home_name, team2 = fixtures$away_name, x1 = xx1, x2 = xx2, return_df = TRUE)
modal <- pred %>%
  group_by(team1, team2) %>%
  filter(probability == max(probability))

write.csv(modal, "output/most_likely_scores_dc.csv", row.names = FALSE)

