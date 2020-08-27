library(dplyr)

original_score_progression %>%
        filter(Quarter == 1 | Quarter == 2) %>%
        filter(Event == "G") %>%
        group_by(GameID, Season, Round, Teamscore, Team1, Team2) %>%
        summarise(ind = n_distinct(PlayerID)) %>%
        arrange(-ind)