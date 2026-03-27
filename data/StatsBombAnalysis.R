rm(list = ls())

install.packages("devtools")
devtools::install_github("statsbomb/StatsBombR")
library(StatsBombR)

Comp <- FreeCompetitions()
Matches <- FreeMatches(Comp)

# 43 ist World Cups
foo = get.matchFree(Matches[Matches$competition.competition_id == 43,])

names(foo)

shots_goals = StatsBombData %>%
  group_by(team.name) %>% 
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE),
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE)) 


