library(dplyr)

original_score_progression %>%
        filter(Event == "G" | Event == "B" | Event == "RB") %>%
        group_by(LastScoreTeam, LastEvent) %>%
        summarise(G = sum(ifelse(Event == "G", 1, 0)),
                  B = sum(ifelse(Event == "B", 1, 0)),
                  RB = sum(ifelse(Event == "RB", 1, 0))
                  )