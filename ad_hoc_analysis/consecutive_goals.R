score_progression_worm %>%
        ungroup() %>%
        filter(Event != "PS" & Event != "S" & Event != "F") %>%
        group_by(GameID, Teamscore) %>% 
        mutate(LastTeamEvent = lag(Event, 1)) %>%
        mutate(consecgoals = ifelse((Event == "G" & Team1CumGoals == 1), 1, 
                                    ifelse(Event == "G", lag(1 + 1), 0))) %>%
        select(GameID, Timescore, Teamscore, Event, LastTeamEvent, consecgoals)