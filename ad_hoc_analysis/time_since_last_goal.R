library(dplyr)
library(lubridate)

timesincelastgoal <- original_score_progression %>%
        ungroup() %>%
        group_by(ScoresGameID) %>%
        filter(Event == "G") %>%
        mutate(GameTimeSinceLastGoal = GameTime - lag(GameTime, 1)) %>%
        mutate(GameTimeSinceLastGoal_ms = 
                       sprintf('%02d:%02d', 
                               minute(seconds_to_period(GameTimeSinceLastGoal)), 
                               second(seconds_to_period(GameTimeSinceLastGoal))))