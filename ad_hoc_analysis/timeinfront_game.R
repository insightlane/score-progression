
#Pretty sure this has errors

score_progression_worm %>%
        ungroup() %>%
        filter(Event != "PS" & TimePercSinceLastScore != "NA" & Result == "Team 1 win") %>%
        #filter(Round == "EF" | Round == "SF" | Round == "QF" | Round == "PF" | Round == "GF") %>%
        group_by(Season, Round, ScoresGameID, Team1, Team2, Team1FinalMargin) %>%
        summarise(Gametimeinfront = sum(GameTimeSinceLastScore[Team1 == InFront]),
                  Timeinfront = sum(TimePercSinceLastScore[Team1 == InFront]),
                  Perctimeinfront = Timeinfront/no_games * 100) %>%
        arrange(Gametimeinfront)
