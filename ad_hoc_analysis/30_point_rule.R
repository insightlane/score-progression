library(dplyr)
library(tidyr)

comebacksyear <- score_progression_worm %>%
        ungroup() %>%
        mutate(Season = as.numeric(format(as.Date(Date.x, "%d-%b-%Y"), "%Y"))) %>%
        #filter(Event != "PS" & Event != "S" & Event != "F") %>%
        group_by(Season) %>%
        summarise(count = n_distinct(GameID),
                  countwin6d = length(unique(GameID[ScorerMargin <= -6 & ScorerFinalMargin > 0])),
                  count6d = length(unique(GameID[ScorerMargin <= -6])),
                  percwin6d = countwin6d/count,
                  countwin12d = length(unique(GameID[ScorerMargin <= -12 & ScorerFinalMargin > 0])),
                  count12d = length(unique(GameID[ScorerMargin <= -12])),
                  percwin12d = countwin12d/count,
                  countwin18d = length(unique(GameID[ScorerMargin <= -18 & ScorerFinalMargin > 0])),
                  count18d = length(unique(GameID[ScorerMargin <= -18])),
                  percwin18d = countwin18d/count,
                  countwin24d = length(unique(GameID[ScorerMargin <= -24 & ScorerFinalMargin > 0])),
                  count24d = length(unique(GameID[ScorerMargin <= -24])),
                  percwin24d = countwin24d/count,
                  countwin30d = length(unique(GameID[ScorerMargin <= -30 & ScorerFinalMargin > 0])),
                  count30d = length(unique(GameID[ScorerMargin <= -30])),
                  percwin30d = countwin30d/count)


score_progression_worm %>%
        ungroup() %>%
        mutate(Season = as.numeric(format(as.Date(Date.x, "%d-%b-%Y"), "%Y"))) %>%
        group_by(Season) %>%
        filter(Team1Margin <= -24 & Team1FinalMargin > 0) %>%
        summarise(n_distinct(GameID, Team1))


score_progression_worm %>%
        ungroup() %>%
        mutate(Season = as.numeric(format(as.Date(Date.x, "%d-%b-%Y"), "%Y"))) %>%
        
        group_by(Season) %>%
        #filter(Team1Margin <= -24 & Team1FinalMargin > 0) %>%
        summarise(count = n_distinct(GameID),
                  countwin24d = length(unique(GameID[Team1Margin <= -24 & Team1FinalMargin > 0])),
                  count24d = length(unique(GameID[Team1Margin <= -24])))

dots <- setNames(list(  ~ mean(value),  
                        ~ sum(value),  
                        ~ median(value), 
                        ~ sd(value)),  
                 c("Right", "Wrong", "Unanswered", "Invalid"))


comebacksteam <- score_progression_worm %>%
        ungroup() %>%
        mutate(Season = as.numeric(format(as.Date(Date.x, "%d-%b-%Y"), "%Y"))) %>%
        #filter(Event != "PS" & Event != "S" & Event != "F") %>%
        group_by(Team1) %>%
        summarise(#count = n_distinct(GameID),
                  #countwin6d = length(unique(GameID[Team1Margin <= -6 & Team1FinalMargin > 0])),
                  count6d = length(unique(GameID[Team1Margin <= -6])),
                  #percwin6d = countwin6d/count,
                  #countwin12d = length(unique(GameID[Team1Margin <= -12 & Team1FinalMargin > 0])),
                  count12d = length(unique(GameID[Team1Margin <= -12])),
                  #percwin12d = countwin12d/count,
                  #countwin18d = length(unique(GameID[Team1Margin <= -18 & Team1FinalMargin > 0])),
                  count18d = length(unique(GameID[Team1Margin <= -18])),
                  #percwin18d = countwin18d/count,
                  countwin24d = length(unique(GameID[Team1Margin <= -24 & Team1FinalMargin > 0])),
                  count24d = length(unique(GameID[Team1Margin <= -24])),
                  #percwin24d = countwin24d/count,
                  countwin30d = length(unique(GameID[Team1Margin <= -30 & Team1FinalMargin > 0])),
                  count30d = length(unique(GameID[Team1Margin <= -30])),
                  #percwin30d = countwin30d/count,
                  countwin36d = length(unique(GameID[Team1Margin <= -36 & Team1FinalMargin > 0])),
                  count36d = length(unique(GameID[Team1Margin <= -36]))
                  #percwin36d = countwin30d/count
                  ) %>%
        summar
        gather(situation, number, countwin6d:count6d) %>%
        spread()


### Teams behind in last quarter

score_progression_worm %>%
        ungroup() %>%
        mutate(Season = as.numeric(format(as.Date(Date.x, "%d-%b-%Y"), "%Y"))) %>%
        filter(Event != "PS" & Season == 2017) %>%
        group_by(Team1) %>%
        summarise(count = n_distinct(GameID),
                  countq4def = length(unique(GameID[Team1Margin < 0 
                                                     & Quarter == 4])))
        