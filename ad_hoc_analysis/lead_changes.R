library(dplyr)

original_score_progression %>%
        group_by(Season) %>%
        summarise(Games = n_distinct(GameID),
                  LeadChanges = sum(LeadChangeFlag),
                  AveLeadChangesPerGAme = LeadChanges/Games)


original_score_progression %>%
        #filter(Season == 2016,  Round == "R4") %>%
        group_by(Season) %>%
        summarise(LeadChanges = sum(LeadChangeFlag)) %>%
        arrange(-LeadChanges)