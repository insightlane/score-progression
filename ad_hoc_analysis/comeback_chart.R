library(dplyr)
library(ggplot2)

library(zoo)

######
## Comebacks from 

zgamedetails <- predicttimes %>%
        ungroup() %>%
        #select(GameID, Team1) %>%
        distinct(GameID, Team1, Team2, Result, .keep_all = FALSE)


zallminutes <- expand.grid(GameID = unique(predicttimes$GameID), MinsRemRounddown = c(1:20)) %>%
        arrange(GameID) %>%
        left_join(y = zgamedetails, by = "GameID") %>%
        select(GameID, Team1, Team2, Result, MinsRemRounddown) %>%
        arrange(GameID, Team1, Team2, Result, -MinsRemRounddown) %>%
        mutate(GoalsDown = NA)


predicttimes_SQ4 <- score_progression_worm %>%
        ungroup() %>%
        filter(Quarter == 4 & Event == "S") %>%
        mutate(TimeAdjustedDelta = ifelse(Event == "G" & (QtrLength - Timescore) >= 44, -33, 
                                          ifelse((Event == "B" | Event == "RB") & (QtrLength - Timescore) >= 11, -9, 
                                                 0))) %>%
        group_by(GameID, Team1, Quarter) %>%
        mutate(CumTimeAdjustedDelta = cumsum(TimeAdjustedDelta)) %>%
        mutate(AdjustedTimescore = ifelse(is.na(lag(cumsum(TimeAdjustedDelta))), 
                                          Timescore, 
                                          Timescore + lag(cumsum(TimeAdjustedDelta)))) %>%
        mutate(AdjustedQtrLength = QtrLength + min(CumTimeAdjustedDelta)) %>%
        mutate(AdjustedTimePerc = ifelse((Quarter == 1), 0.25*AdjustedTimescore/AdjustedQtrLength, 
                                         ifelse((Quarter == 2), 0.25 + 0.25*AdjustedTimescore/AdjustedQtrLength, 
                                                ifelse((Quarter == 3), 0.5 + 0.25*AdjustedTimescore/AdjustedQtrLength, 
                                                       ifelse((Quarter == 4), 0.75 + 0.25*AdjustedTimescore/AdjustedQtrLength, 
                                                              NA))))) %>%
        mutate(AdjustedQtrTimePerc = ifelse((Quarter == 1), 0.25*AdjustedTimescore/AdjustedQtrLength, 
                                            ifelse((Quarter == 2), 0.25*AdjustedTimescore/AdjustedQtrLength, 
                                                   ifelse((Quarter == 3), 0.25*AdjustedTimescore/AdjustedQtrLength, 
                                                          ifelse((Quarter == 4), 0.25*AdjustedTimescore/AdjustedQtrLength, 
                                                                 NA))))) %>%
        mutate(TimePerc = ifelse(QtrTimePerc == 0.25, TimePerc - 0.0001, TimePerc)) %>%
        mutate(QtrTimePerc = ifelse(QtrTimePerc == 0.25, QtrTimePerc - 0.0001, QtrTimePerc)) %>%
        mutate(AdjustedTimePerc = ifelse(AdjustedQtrTimePerc == 0.25, AdjustedTimePerc - 0.0001, AdjustedTimePerc)) %>%
        mutate(AdjustedQtrTimePerc = ifelse(AdjustedQtrTimePerc == 0.25, AdjustedQtrTimePerc - 0.0001, AdjustedQtrTimePerc))

zcomebackrate_scores <- rbind(predicttimes, predicttimes_SQ4) %>%
        ungroup() %>%
        filter(Quarter == 4) %>%
        filter(Event == "PS" | Event == "S") %>%
        arrange(GameID, Team1, AdjustedTimePerc) %>%
        mutate(AdjustedQtrMins = AdjustedQtrTimePerc/0.25 * 20) %>%
        mutate(MinutesRemaining = 20 - AdjustedQtrMins) %>%
        mutate(MinsRemRounddown = floor(MinutesRemaining)) %>%
        mutate(GoalsDown = ceiling(-1*Team1Margin/6)) %>%
        select(GameID, Team1, Team2, Result, MinsRemRounddown, GoalsDown) %>%
        distinct(GameID, Team1, Team2, Result, MinsRemRounddown, GoalsDown, .keep_all = TRUE)


zcomebackrate_combined <- rbind(zcomebackrate_scores, zallminutes) %>%
        distinct(GameID, Team1, Team2, MinsRemRounddown, .keep_all = TRUE) %>%
        arrange(GameID, Team1, Team2, -MinsRemRounddown) %>%
        group_by(GameID, Team1) %>%
        #filter(GameID == 87) %>%
        mutate(GoalsDown = na.locf(GoalsDown, na.rm = FALSE)) %>%
        filter(!is.na(GoalsDown)) %>%
        filter(GoalsDown >= 1) %>%
        #filter(MinsRemRounddown >= 1) %>%
        arrange(GameID, -MinsRemRounddown) %>%
        #filter(MinsRemRounddown <= 15) %>%
        group_by(GoalsDown, MinsRemRounddown) %>%
        summarise(total = n(),
                  wins = sum(ifelse(Result == "Team 1 win", 1, 0)),
                  winrate = wins/total) %>%
        filter(GoalsDown <= 6)

zcomebackrate_combined2 <- zcomebackrate_combined %>%
        ungroup() %>%
        mutate(GoalsDown_all = GoalsDown) %>%
        select(-GoalsDown)


ggplot() +

        geom_line(data = zcomebackrate_combined2, aes(x = MinsRemRounddown, y = winrate, group = interaction(GoalsDown_all)), colour = "grey") +
        geom_line(data = zcomebackrate_combined, aes(x = MinsRemRounddown, y = winrate, group = interaction(GoalsDown)), colour = "black") +
        geom_vline(data = zcomebackrate_combined, aes(xintercept = GoalsDown), linetype = "dashed") + 
        scale_x_reverse() +
        theme_minimal() +
        scale_color_grey() +
        facet_wrap(~GoalsDown) + 
        theme(legend.position = "none")


        
        

