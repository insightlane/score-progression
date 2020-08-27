# ### For goal streaks
# 
# 
 library(dplyr)
# 
# original_score_progression_streaks <- score_progression_worm %>%
#         filter(Event != "PS")
# 
# original_score_progression_streaks <- score_progression_worm %>%
#         filter(Event == "G") %>%
#         mutate(GoalFlag = ifelse(Event == "G", 1, 0)) %>%
#         group_by(GameID) %>%
#         mutate(StartStreak = ifelse((Teamscore != lag(Teamscore)) | is.na(lag(Teamscore)), 1, 0)) %>%
#         mutate(GoalStreak = ifelse((StartStreak == 1), 1, lag(GoalStreak) + 1)) %>%
#         ungroup %>%
#         group_by(GameID, Teamscore) %>%
#         mutate(TeamGoals = cumsum(GoalFlag)) %>%
#         select(GameID, Home, Away, Event, Teamscore, GoalFlag, StartStreak, TeamGoals)
# 
# 
# 
# original_score_progression_streaks$StartStreak[is.na(original_score_progression_streaks$StartStreak)] <- 1

#######################################3

# Add team event column (HG/HB/AG/AB/NA)

original_score_progression_streaks <- original_score_progression %>%
        filter(Event != "PS")

original_score_progression_streaks$TeamEvent <- ifelse((original_score_progression_streaks$Team1 == original_score_progression_streaks$Teamscore) & (original_score_progression_streaks$Event == "G"), "T1G", 
                             ifelse((original_score_progression_streaks$Team1 == original_score_progression_streaks$Teamscore) & ((original_score_progression_streaks$Event == "B") | (original_score_progression_streaks$Event == "RB")), "T1B", 
                                    ifelse((original_score_progression_streaks$Team2 == original_score_progression_streaks$Teamscore) & (original_score_progression_streaks$Event == "G"), "T2G", 
                                           ifelse((original_score_progression_streaks$Team2 == original_score_progression_streaks$Teamscore) & ((original_score_progression_streaks$Event == "B") | (original_score_progression_streaks$Event == "RB")), "T2B", 
                                                  NA))))

# Add team event column with game ID (xHG/xHB/xAG/xAB/xNA)

original_score_progression_streaks$LastGoal <- "-"

original_score_progression_streaks$GoalStreak <- NA

original_score_progression_streaks[1, ]$GoalStreak <- 0

# Loop through rows to update the 'last goal' prior to every event, and update the goal streak at every event

for(i in 2:nrow(original_score_progression_streaks)){
#        for(i in 2:100){        # Loop through rows
        if(original_score_progression_streaks[i, ]$Score == 6){                                                   # If event (score) is a goal
                if(original_score_progression_streaks[i, ]$TeamEvent == original_score_progression_streaks[i - 1, ]$LastGoal){              # And if the event (goal) was by the same team as the previous goal
                        original_score_progression_streaks[i, ]$GoalStreak <- 1 + original_score_progression_streaks[i - 1, ]$GoalStreak    # Add one to the goal streak
                } else {                                                                # If the event (goal) was not by the same team as the previous goal
                        original_score_progression_streaks[i, ]$GoalStreak <- 1                                   # Set the goal streak back to 1
                }
                original_score_progression_streaks[i, ]$LastGoal <- original_score_progression_streaks[i, ]$TeamEvent                       # Set the new last goal as that event  
        } else {
                if((original_score_progression_streaks[i, ]$Quarter == 1) & (original_score_progression_streaks[i, ]$Event == "S")){
                        original_score_progression_streaks[i, ]$LastGoal <- "-"
                        original_score_progression_streaks[i, ]$GoalStreak <- 0
                } else {
                        # If the event (score) is not a goal
                        original_score_progression_streaks[i, ]$LastGoal <- original_score_progression_streaks[i - 1, ]$LastGoal            # Do not change the last goal event
                        original_score_progression_streaks[i, ]$GoalStreak <- original_score_progression_streaks[i - 1, ]$GoalStreak        # No not change the last goal streak
                }
        }
}

# Add team event score column (HG/HB/AG/AB/NA)

original_score_progression_streaks$TeamScoreEvent <- ifelse((original_score_progression_streaks$TeamEvent == "T1G") | (original_score_progression_streaks$TeamEvent == "T1B"), "T1",
                                  ifelse((original_score_progression_streaks$TeamEvent == "T2G") | (original_score_progression_streaks$TeamEvent == "T2B"), "T2",
                                         NA))

# Do the same thing with scores

original_score_progression_streaks$LastScore <- "-"

original_score_progression_streaks$ScoreStreak <- NA

original_score_progression_streaks[1, ]$ScoreStreak <- 0

# Loop through rows to update the 'last score' prior to every event, and update the Score streak at every event

for(i in 2:nrow(original_score_progression_streaks)){
        #        for(i in 2:100){        # Loop through rows
        if(original_score_progression_streaks[i, ]$Score > 0){                                                            # If event (score) is a score
                if(original_score_progression_streaks[i, ]$TeamScoreEvent == original_score_progression_streaks[i - 1, ]$LastScore){                # And if the event (score) was by the same team as the previous score
                        original_score_progression_streaks[i, ]$ScoreStreak <- 1 + original_score_progression_streaks[i - 1, ]$ScoreStreak          # Add one to the score streak
                } else {                                                                        # If the event (score) was not by the same team as the previous score
                        original_score_progression_streaks[i, ]$ScoreStreak <- 1                                          # Set the score streak back to 1
                }
                original_score_progression_streaks[i, ]$LastScore <- original_score_progression_streaks[i, ]$TeamScoreEvent                         # Set the new last score as that event  
        } else {
                if((original_score_progression_streaks[i, ]$Quarter == 1) & (original_score_progression_streaks[i, ]$Event == "S")){
                        original_score_progression_streaks[i, ]$LastScore <- "-"
                        original_score_progression_streaks[i, ]$ScoreStreak <- 0
                } else {
                        # If the event (score) is not a score
                        original_score_progression_streaks[i, ]$LastScore <- original_score_progression_streaks[i - 1, ]$LastScore                  # Do not change the last score event
                        original_score_progression_streaks[i, ]$ScoreStreak <- original_score_progression_streaks[i - 1, ]$ScoreStreak              # No not change the last score streak
                }
        }
}

streaks <- original_score_progression_streaks %>%
        filter(Event != "S" & Event != "F") %>%
        group_by(GameID) %>%
        # mutate(EndScoreStreakFlag = ifelse((Score > 0) & (LastScore != lead(LastScore) | is.na(lead(LastScore)))
        #                                  , 1
        #                                  , 0)) %>%
        # mutate(EndScoreStreakValue = EndScoreStreakFlag * ScoreStreak) %>%
        # 
        # mutate(ScoreStreakTeamFor = ifelse((LastScore == "T1")
        #                                    , Team1
        #                                    , Team2)) %>%
        # 
        # mutate(ScoreStreakTeamAgst = ifelse((LastScore == "T1")
        #                                    , Team1
        #                                    , Team2)) %>%
       
        mutate(EndGoalStreakFlag = ifelse((Score > 0) & (LastGoal != lead(LastGoal) | is.na(lead(LastScore)))
                                       , 1
                                       , 0)) %>%
        mutate(EndGoalStreakValue = EndGoalStreakFlag * GoalStreak) %>%
        
        mutate(GoalStreakTeamFor = ifelse((LastGoal == "T1G")
                                           , Team1
                                           , Team2)) %>%
        
        mutate(GoalStreakTeamAgst = ifelse((LastGoal == "T1G")
                                            , Team1
                                            , Team2))
                
        

write.csv(streaks, file="streaks.csv")