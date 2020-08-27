## count of number of first goals by player

library(dplyr)

original_score_progression %>%
        filter(Event != "PS" & Event != "S" & Event != "F" & Season == 2017) %>%
        ungroup() %>%
        group_by(GameID, Event) %>%
        mutate(GoalCount = cumsum(ifelse(Event == "G", 1, 0))) %>%
        mutate(BehindCount = cumsum(ifelse(Event == "B", 1, 0))) %>%
        #select(GameID, Teamscore, Playerscore, Event, GoalCount, BehindCount) %>%
        ungroup() %>%
        group_by(Playerscore, PlayerID, Teamscore) %>%
        summarise(TotalFirstGoal = sum(ifelse(GoalCount == 1, 1, 0)), 
                  TotalGoals = sum(ifelse(Event == "G", 1, 0)),
                  TotalFirstGoalPerc = TotalFirstGoal/TotalGoals
        ) %>%
        #filter(TotalGoals > 100) %>%
        arrange(-TotalFirstGoal)