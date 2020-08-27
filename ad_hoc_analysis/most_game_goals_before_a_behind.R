library(dplyr)

test <- score_progression_worm %>%
        ungroup() %>%
        mutate(comb_goals = Team1CumGoals + Team2CumGoals,
               comb_behinds = Team1CumBehinds + Team2CumBehinds) %>%
        filter(comb_behinds == 1) %>%
        filter(Event == "B") %>%
        filter(Status == "Original") %>%
        arrange(-comb_goals) %>%
        select(comb_goals, Season, Round, Team1, Team2, Team1CumGoals, Team2CumGoals)