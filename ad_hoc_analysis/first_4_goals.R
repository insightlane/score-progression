
first4goal <- score_progression_worm %>%
        ungroup %>%
        mutate(First4goal = ifelse((HomeG == 1 & HomeCumGoals == 4 & AwayCumGoals < 4), Home, 
                                   ifelse((AwayG == 1 & AwayCumGoals == 4 & HomeCumGoals < 4), Away, 
                                          "NA"))) %>%
        filter(First4goal != "NA")

write.csv(first4goal, file="first4goal.csv")
