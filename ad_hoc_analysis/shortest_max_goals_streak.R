library(dplyr)


test <- original_score_progression %>%
        filter(Event == "G") %>%
        group_by(GameID) %>%
        mutate(last_goal_team = lag(Teamscore)) %>% #note: that's dplyr::lag, not stats::lag
        mutate(start = (Teamscore != last_goal_team)) %>%
        mutate(start = ifelse(is.na(start), TRUE, start)) %>%
        mutate(total_game_goals = n()) %>%
        #select(GameID, Season, Round, Team1, Team2, total_game_goals, Teamscore, last_goal_team, start) %>%
        mutate(streak_id = cumsum(start)) %>%
        ungroup() %>%
        group_by(GameID, streak_id) %>%
        mutate(streak = row_number()) %>% 
        ungroup() %>%
        group_by(GameID) %>%
        mutate(max_streak = max(streak)) 


test %>%
        distinct(GameID, Season, Round, Team1, Team2, total_game_goals, max_streak) %>%
        filter(max_streak <= 2) %>%
        arrange(-total_game_goals)

