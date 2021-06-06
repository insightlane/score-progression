library(dplyr)

original_score_progression %>%
        ungroup() %>%
        filter(Event == "G") %>%
        group_by(GameID, Season, Round, Teamscore, Team1, Team2, Playerscore, Quarter) %>%
        summarise(goals_quarter = n()) %>%
        ungroup() %>%        
        group_by(GameID, Season, Round, Teamscore, Team1, Team2, Playerscore) %>%
        mutate(goals_game = sum(goals_quarter)) %>%
        arrange(-goals_quarter) %>%
        filter(goals_game == goals_quarter)