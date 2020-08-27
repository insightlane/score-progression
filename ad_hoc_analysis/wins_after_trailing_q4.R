library(dplyr)
library(splitstackshape)

ess_test <- 
score_progression_worm %>%
        ungroup() %>%
        filter(Quarter == 4) %>%
        group_by(Season, Round, Team1, Team2, Dateadj, Team1FinalMargin) %>%
        summarise(min_margin = min(Team1Margin)) %>%
        arrange(Team1, Dateadj) %>%
        mutate(check = ifelse(Team1FinalMargin > 0 & min_margin < 0, 0, 1)) %>%
        ungroup() %>%
        group_by(Team1, idx = cumsum(check == 1L)) %>%
        mutate(counter = row_number() - 1) %>%
        #filter(counter >= 3)
        filter(Team1 == "Geelong" & Season <= 2010)