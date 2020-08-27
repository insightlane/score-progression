

test <- score_progression_worm %>%
        ungroup() %>%
        #filter(Season == 2005 & Team1 == "Sydney" & Team2 == "Brisbane Lions" & Round == "R3") %>%
        filter(Event != "PS") %>%
        group_by(ScoresGameID,  Team1, Team2) %>%
        mutate(GameTimeToNextScore = lead(GameTimeSinceLastScore)) %>%
        mutate(GameTimeToNextScore = ifelse(is.na(GameTimeToNextScore), 0, GameTimeToNextScore)) %>%
        filter(Team1 == Winner) %>%
        filter(Team1 == InFront) %>%
        ungroup() %>%
        group_by(Season, Round, ScoresGameID, Team1, Team2, Team1FinalMargin) %>%
        summarise(Gametimeinfront = sum(GameTimeToNextScore)) %>%
        arrange(Gametimeinfront)
