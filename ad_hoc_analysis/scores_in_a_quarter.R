library(dplyr)

original_score_progression %>%
        group_by(Season, GameID, Quarter, Playerscore, PlayerID) %>%
        filter(Event != "PS" & Event != "S" & Event != "F") %>%
        summarise(count = sum(ifelse(Event == "G" | Event == "B",1,0))) %>%
        arrange(-count, -Season)



original_score_progression %>%
        group_by(Season, GameID, Playerscore, PlayerID) %>%
        filter(Event != "PS" & Event != "S" & Event != "F") %>%
        summarise(count = sum(ifelse((Event == "G" & Quarter == 1) |
                                             (Event == "G" & Quarter == 2 & Timescore <= 1200),1,0))) %>%
        arrange(-count, -Season)