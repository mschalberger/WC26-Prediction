library(tidyverse)
library(data.table)
setwd("tsv")

files <- list.files(pattern = "\\.tsv$", full.names = TRUE)
elo_all_matches <- rbindlist(lapply(files, fread, header = FALSE, sep = "\t"))
colnames(elo_all_matches) <- c("year", "month", "day", "home_team", "away_team", "home_goals",
                               "away_goals", "tournament", "location", "elo_change_home", "elo_after_match_home",
                               "elo_after_match_away", "rank_change_home", "rank_change_away", 
                               "rank_after_match_home", "rank_after_match_away")

elo_all_matches <- elo_all_matches %>% 
  mutate(elo_change_away = - elo_change_home, .after = elo_change_home)
