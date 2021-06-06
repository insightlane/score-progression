library(dplyr)

original_score_progression %>%
        ungroup() %>%
        group_by(GameID, Playerscore) %>%
        filter(Event == "G") %>%
        #filter(Team1 == "Geelong" & Season == 2021 & Round == "R6")  %>%
        mutate(game_time_since_last_player_goal = GameTime - lag(GameTime),
               previous_quarter = lag(Quarter)) %>%
        #select(Season, Round, Team1, Team2, Dateadj, Playerscore, Event, Quarter, Timescore, GameTime, GameTimeSinceLastScore, game_time_since_last_player_goal, previous_quarter) %>%
        select(game_time_since_last_player_goal, Playerscore, Quarter, previous_quarter, Season, Round) %>%
        #filter(Playerscore == "Mitch Duncan")
        arrange(game_time_since_last_player_goal)
        
        

               
               
