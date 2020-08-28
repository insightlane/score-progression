#***********************************************************************************************
# TITLE: AFL match score progression (2001-2019)
#
# DESCRIPTION: Transformation of raw score progression data into useable formats for AFL matches (2008-)
#
# AUTHOR: InsightLane
#
# CREATED:   Original 2016
# MODIFIED:  Last updated 2020 
#
# INPUTS:    Time series score data from AFL Tables (https://afltables.com/afl/stats/times.csv)
#
# OUTPUTS:   CSV files of enriched score progression data and score progression ScoreWorm data
#
# STEPS:
#               1. Download extract data from AFL Tables
#               2. Import match scores from AFL Tables big list
#               3. Join match scores onto score progression data
#               4. Transform and cleanse score progression events
#               5. Create new data frame to add rows for the moment before a score
#               6. Create new data frame with lengths of quarter for each game
#               7. Join data frames together
#               8. Create additional columns to describe score progression data
#               9. Duplicate and flip team names to double score progression data to have associated with both teams
#               10. Append both data frames together for final output
#               11. Output CSV files
#
# ***********************************************************************************************

library(dplyr)
rm(list = ls())

# -----------------------------------------------------------------------------------------------
# 1. Download extract data from AFL Tables
# -----------------------------------------------------------------------------------------------

# #Download latest zip file from AFL tables to working directory
# 
# data <- download.file("http://afltables.com/public/times.zip", destfile="times.zip")
# 
# # Unzip times.csv in working directory
# 
# dataunz <- unzip("times.zip")

# Download latest zip file from AFL tables to working directory
 
download.file("https://afltables.com/afl/stats/times.csv", destfile="times.csv")

# Load CSV file

raw_score_progression <- read.csv("C:/Local Code/insightlane/score-progression/times.csv", 
                  header = FALSE,
                  stringsAsFactors = FALSE)

# Rename columns as appropriate

colnames(raw_score_progression) <- c("GameID","Team1","Team2","Date",
                     "Quarter","Event","Timescore","Teamscore",
                     "Playerscore","PlayerID")

# Create new DF with adjusted times, Team Event ()

original_score_progression <- data.frame(raw_score_progression, 
                            Dateadj = as.Date(raw_score_progression$Date, "%d-%b-%Y"), 
                            Season = as.numeric(format(as.Date(raw_score_progression$Date, "%d-%b-%Y"), "%Y")), 
                            Status = "Original")

# Fix Timescore to be numeric

original_score_progression$Timescore <- as.numeric(original_score_progression$Timescore)

# Fix Kangaroos to North Melbourne

original_score_progression$Team1 <- ifelse((original_score_progression$Team1 == "Kangaroos"), "North Melbourne", original_score_progression$Team1)
original_score_progression$Team2 <- ifelse((original_score_progression$Team2 == "Kangaroos"), "North Melbourne", original_score_progression$Team2)
original_score_progression$Teamscore <- ifelse((original_score_progression$Teamscore == "Kangaroos"), "North Melbourne", original_score_progression$Teamscore)

# -----------------------------------------------------------------------------------------------
# 2. Import match scores from AFL Tables big list
# -----------------------------------------------------------------------------------------------

# Import scores from http://afltables.com/afl/stats/biglists/bg3.txt, using fixed widths

#rm(Team1scores, awayscores, match_scores)

raw_match_scores <- read.fwf("http://afltables.com/afl/stats/biglists/bg3.txt", 
                   skip = 2, header = FALSE, 
                   widths = c(7, 17, 5, 18, 17, 18, 18, 18), 
                   col.names = c("ScoresGameID","Date","Round","Team1",
                                 "Team1Score","Team2","Team2Score","Venue")
                   )

# Split scores into goals/behinds/points for both teams

team1scores <- data.frame(do.call('rbind', strsplit(as.character(raw_match_scores$Team1Score),"\\.")))
team2scores <- data.frame(do.call('rbind', strsplit(as.character(raw_match_scores$Team2Score),"\\.")))

# Convert new goals/behinds/points columns into numeric for both teams

team1scores <- data.frame("Team1Goals" = as.numeric(as.character(team1scores$X1)), 
                         "Team1Behinds" = as.numeric(as.character(team1scores$X2)), 
                         "Team1Points" = as.numeric(as.character(team1scores$X3)))
team2scores <- data.frame("Team2Goals" = as.numeric(as.character(team2scores$X1)), 
                         "Team2Behinds" = as.numeric(as.character(team2scores$X2)), 
                         "Team2Points" = as.numeric(as.character(team2scores$X3)))

# Combine goals/behind/points into new data frame 

match_scores <- data.frame(raw_match_scores, Dateadj = as.Date(raw_match_scores$Date, "%d-%b-%Y"), team1scores, team2scores)
match_scores$Team1 <- trimws(as.character(match_scores$Team1), which = "right")
match_scores$Team2 <- trimws(as.character(match_scores$Team2), which = "right")
match_scores$Round <- trimws(as.character(match_scores$Round), which = "right")
match_scores$Venue <- trimws(as.character(match_scores$Venue), which = "right")

# Fix GW Sydney to Greater Western Sydney

match_scores$Team1 <- ifelse((match_scores$Team1 == "GW Sydney"), "Greater Western Sydney", match_scores$Team1)
match_scores$Team2 <- ifelse((match_scores$Team2 == "GW Sydney"), "Greater Western Sydney", match_scores$Team2)

# Create new column for margin (relative to team 1) for each match

match_scores$Team1FinalMargin <- match_scores$Team1Points - match_scores$Team2Points 
match_scores$Team2FinalMargin <- -1 * match_scores$Team1FinalMargin

# Create new column for margin (absolute) for each match

match_scores$AbsFinalMargin <- abs(match_scores$Team1Points - match_scores$Team2Points)

# Create new column for result (Team1 win/away win/draw) for each match

match_scores$Result <- ifelse((match_scores$Team1FinalMargin > 0), "Team 1 win",
                           ifelse((match_scores$Team1FinalMargin < 0), "Team 2 win",
                                  ifelse((match_scores$Team1FinalMargin == 0), "Draw",
                                         NA)))

# Create new column for victor (or draw) for each match

match_scores$Winner <- ifelse((match_scores$Team1FinalMargin > 0), as.character(match_scores$Team1),
                           ifelse((match_scores$Team1FinalMargin < 0),  as.character(match_scores$Team2),
                                  ifelse((match_scores$Team1FinalMargin == 0), "Draw",
                                         NA)))

match_scores$Loser <- ifelse((match_scores$Team1FinalMargin > 0), as.character(match_scores$Team2),
                           ifelse((match_scores$Team1FinalMargin < 0),  as.character(match_scores$Team1),
                                  ifelse((match_scores$Team1FinalMargin == 0), "Draw",
                                         NA)))

# -----------------------------------------------------------------------------------------------
# 3. Join match scores onto score progression data
# -----------------------------------------------------------------------------------------------

original_score_progression <- merge(x = original_score_progression, y = match_scores, 
                    by = c("Dateadj", "Team1", "Team2"), 
                    all.x = TRUE)

# -----------------------------------------------------------------------------------------------
# 4. Transform and cleanse score progression events
# -----------------------------------------------------------------------------------------------

# Rename events as Goal/Behind/Rushed Behind/Final

original_score_progression$Event[original_score_progression$Event == 0] <- "G"
original_score_progression$Event[original_score_progression$Event == 1] <- "B"
original_score_progression$Event[original_score_progression$Event == 2] <- "RB"
original_score_progression$Event[original_score_progression$Event == 3] <- "F"

# Create new data frame to add rows in for start of quarters

# Take copy of all end of quarter events (where Event = F)

qtrstarts <- original_score_progression[(original_score_progression$Event == "F"), ]

# Remove existing row names

rownames(qtrstarts) <- NULL

# Set all times to 0 for start of quarter

qtrstarts$Timescore <- 0

# Set all event codes to NA for quarter start

qtrstarts$Event <- "S"

# -----------------------------------------------------------------------------------------------
# 5. Create new data frame to add rows for the moment before a score
# -----------------------------------------------------------------------------------------------

# Take copy of all end of quarter events (where Event = G/B/RB)

prescore <- original_score_progression[(original_score_progression$Event == "G") 
                          | (original_score_progression$Event == "B") 
                          | (original_score_progression$Event == "RB"), ]

# Remove existing row names

rownames(prescore) <- NULL

# Set all times to 1 second before the score

prescore$Timescore <- prescore$Timescore - 1

# Set all event codes to PS for moment before the score

prescore$Event <- "PS"

original_score_progression <- rbind(original_score_progression, qtrstarts, prescore)

original_score_progression <- original_score_progression[order(original_score_progression$GameID, original_score_progression$Quarter, original_score_progression$Timescore), ]

rownames(original_score_progression) <- NULL

# -----------------------------------------------------------------------------------------------
# 6. Create new data frame with lengths of quarter for each game
# -----------------------------------------------------------------------------------------------

qtrlengths <- original_score_progression %>% 
        filter(Event == "F") %>% 
        select(GameID, Quarter, Timescore) %>%
        group_by(GameID) %>%
        mutate(RunningQtrLength = cumsum(Timescore))

# Rename column to QtrLength

names(qtrlengths)[names(qtrlengths) == "Timescore"] <- "QtrLength"

# -----------------------------------------------------------------------------------------------
# 7. Join data frames together
# -----------------------------------------------------------------------------------------------

# Join quarter length onto time score data

original_score_progression <- merge(x = original_score_progression, y = qtrlengths, 
                  by = c("GameID", "Quarter"), 
                  all.x = TRUE)

original_score_progression <- original_score_progression[order(original_score_progression$GameID, original_score_progression$Quarter, original_score_progression$Timescore), ]

# -----------------------------------------------------------------------------------------------
# 8. Create additional columns to describe score progression data
# -----------------------------------------------------------------------------------------------

# Create new column for percentage time (of quarters) of event through the match

original_score_progression$TimePerc <- ifelse((original_score_progression$Quarter == 1), 0.25*original_score_progression$Timescore/original_score_progression$QtrLength, 
                            ifelse((original_score_progression$Quarter == 2), 0.25 + 0.25*original_score_progression$Timescore/original_score_progression$QtrLength, 
                                   ifelse((original_score_progression$Quarter == 3), 0.5 + 0.25*original_score_progression$Timescore/original_score_progression$QtrLength, 
                                          ifelse((original_score_progression$Quarter == 4), 0.75 + 0.25*original_score_progression$Timescore/original_score_progression$QtrLength, 
                                                 ifelse((original_score_progression$Quarter == 5), 1 + 0.0625*original_score_progression$Timescore/original_score_progression$QtrLength, 
                                                        ifelse((original_score_progression$Quarter == 6), 1.0625 + 0.0625*original_score_progression$Timescore/original_score_progression$QtrLength, 
                                                               NA))))))

original_score_progression$QtrTimePerc <- ifelse((original_score_progression$Quarter == 1), 0.25*original_score_progression$Timescore/original_score_progression$QtrLength, 
                                 ifelse((original_score_progression$Quarter == 2), 0.25*original_score_progression$Timescore/original_score_progression$QtrLength, 
                                        ifelse((original_score_progression$Quarter == 3), 0.25*original_score_progression$Timescore/original_score_progression$QtrLength, 
                                               ifelse((original_score_progression$Quarter == 4), 0.25*original_score_progression$Timescore/original_score_progression$QtrLength, 
                                                      ifelse((original_score_progression$Quarter == 5), 0.0625*original_score_progression$Timescore/original_score_progression$QtrLength, 
                                                             ifelse((original_score_progression$Quarter == 6), 0.0625*original_score_progression$Timescore/original_score_progression$QtrLength, 
                                                                    NA))))))

original_score_progression$GameTime <- original_score_progression$RunningQtrLength - original_score_progression$QtrLength + original_score_progression$Timescore


# Add column for points scored for each event (6/1/0)

original_score_progression$Score <- ifelse((original_score_progression$Event == "G"), 6, 
                         ifelse((original_score_progression$Event == "B")|(original_score_progression$Event == "RB"), 1, 
                                0))

# Create new column for directional individual scores towards/away from team 1

original_score_progression$Team1Worm <- ifelse((original_score_progression$Team1 == original_score_progression$Teamscore), 
                            original_score_progression$Score, 
                            original_score_progression$Score * -1)
original_score_progression$Team2Worm <- -1 * original_score_progression$Team1Worm
original_score_progression$WinWorm <- ifelse((original_score_progression$Result == "Team 1 win"), 
                                original_score_progression$Team1Worm, 
                                ifelse((original_score_progression$Result == "Team 2 win"), 
                                       original_score_progression$Team2Worm, 
                                       NA))

# Create new column for individual team 1 team scores, goals, behinds and shots

original_score_progression$Team1P <- ifelse((original_score_progression$Team1 == original_score_progression$Teamscore), 
                              original_score_progression$Score, 0)

original_score_progression$Team1G <- ifelse((original_score_progression$Team1 == original_score_progression$Teamscore) 
                          & (original_score_progression$Event == "G"), 
                          1, 0)

original_score_progression$Team1B <- ifelse((original_score_progression$Team1 == original_score_progression$Teamscore) 
                          & (original_score_progression$Event == "B" | original_score_progression$Event == "RB"), 
                          1, 0)

original_score_progression$Team1SS <- ifelse((original_score_progression$Team1 == original_score_progression$Teamscore) 
                           & (original_score_progression$Event == "G" | original_score_progression$Event == "B" | original_score_progression$Event == "RB"), 
                           1, 0)

# Create new column for individual away team scores

original_score_progression$Team2P <- ifelse((original_score_progression$Team2 == original_score_progression$Teamscore), 
                             original_score_progression$Score, 0)

original_score_progression$Team2G <- ifelse((original_score_progression$Team2 == original_score_progression$Teamscore) 
                          & (original_score_progression$Event == "G"), 
                          1, 0)

original_score_progression$Team2B <- ifelse((original_score_progression$Team2 == original_score_progression$Teamscore) 
                          & (original_score_progression$Event == "B" | original_score_progression$Event == "RB"), 
                          1, 0)

original_score_progression$Team2SS <- ifelse((original_score_progression$Team2 == original_score_progression$Teamscore) 
                          & (original_score_progression$Event == "G" | original_score_progression$Event == "B" | original_score_progression$Event == "RB"), 
                          1, 0)

# Create new column for cumulative team 1 score, away score and team 1 margin

original_score_progression <- original_score_progression %>% 
        group_by(GameID) %>% 
        mutate(Team1CumPoints = cumsum(Team1P)) %>% 
        mutate(Team1CumGoals = cumsum(Team1G)) %>% 
        mutate(Team1CumBehinds = cumsum(Team1B)) %>% 
        mutate(Team1CumShots = cumsum(Team1SS)) %>% 
        mutate(Team2CumPoints = cumsum(Team2P)) %>%
        mutate(Team2CumGoals = cumsum(Team2G)) %>% 
        mutate(Team2CumBehinds = cumsum(Team2B)) %>% 
        mutate(Team2CumShots = cumsum(Team2SS)) %>% 
        mutate(Team1Margin = cumsum(Team1Worm)) %>%
        mutate(Team2Margin = -1 * Team1Margin) %>%
        mutate(WinMargin = ifelse(Team1FinalMargin > 0, Team1Margin, 
                                  ifelse(Team1FinalMargin < 0, -1 * Team1Margin, 
                                         NA))) %>%
        mutate(InFront = ifelse(Team1Margin > 0, Team1, 
                                ifelse(Team1Margin < 0, Team2, 
                                       "Scores level"))) %>%
        mutate(Behind = ifelse(Team1Margin > 0, Team2, 
                                ifelse(Team1Margin < 0, Team1, 
                                       "Scores level"))) %>%
        mutate(AbsMargin = abs(Team1Margin)) %>%
        mutate(PreAbsMargin = lag(AbsMargin, 2)) %>%
        mutate(ScorerMargin = ifelse(Teamscore == Team1, Team1Margin, 
                                     ifelse(Teamscore == Team2, Team2Margin, 
                                            NA))) %>%
        mutate(ScorerStatus = ifelse(ScorerMargin > 0, "In front", 
                                     ifelse(ScorerMargin < 0, "Behind", 
                                            ifelse(ScorerMargin == 0, "Level",
                                                   NA)))) %>%
        mutate(PreScorerMargin = ifelse(Teamscore == Team1, Team1Margin - Team1P, 
                                     ifelse(Teamscore == Team2, Team2Margin - Team2P, 
                                            NA))) %>%
        mutate(PreScorerStatus = ifelse(PreScorerMargin > 0, "In front", 
                                     ifelse(PreScorerMargin < 0, "Behind", 
                                            ifelse(PreScorerMargin == 0, "Level",
                                                   NA)))) %>%
        mutate(ScorerFinalMargin = ifelse(Teamscore == Team1, Team1FinalMargin, 
                                     ifelse(Teamscore == Team2, Team2FinalMargin, 
                                            NA))) %>%
 
        mutate(LastLeadingTeam = ifelse(lag(InFront, 2) == "Scores level",
                                    lag(InFront, 4),
                                    lag(InFront, 2))
               ) %>%
        
        mutate(LastLeadingTeam = ifelse(LastLeadingTeam == "Scores level",
                                        lag(LastLeadingTeam, 2),
                                        LastLeadingTeam)
        ) %>%
        
        mutate(LeadChangeFlag = ifelse((InFront != LastLeadingTeam) & Event != "PS" & Event != "S" & Event != "F" & InFront != "Scores level", 
                                       1, 
                                       0)
               ) %>%
        mutate(LeadChangeFlag = ifelse(is.na(LeadChangeFlag), 0, LeadChangeFlag)
               ) %>%
        ungroup() %>%
        group_by(GameID, Quarter) %>% 
        mutate(GameTimeSinceLastScore = ifelse((Event == "F"),
                                               (GameTime - lag(GameTime, 1)),
                                               (GameTime - lag(GameTime, 2)))) %>%
        mutate(TimePercSinceLastScore = ifelse((Event == "F"),
                                               (TimePerc - lag(TimePerc, 1)),
                                               (TimePerc - lag(TimePerc, 2)))) %>%
        mutate(LastEvent = lag(Event, 2)) %>%
        mutate(LastScoreTeam = ifelse(Teamscore == lag(Teamscore, 2), "Same", "Other"))


# -----------------------------------------------------------------------------------------------
# 9. Duplicate and flip team names to double score progression data to have associated with both teams
# -----------------------------------------------------------------------------------------------
        
flipped_score_progression <- original_score_progression[ , c("GameID", "Quarter", "Dateadj", "Team2", "Team1",
                                   "Date.x", "Event", "Timescore", "Teamscore", "Playerscore",
                                   "PlayerID", "Season", "Status", "ScoresGameID", "Date.y",
                                   "Round", "Team2Score", "Team1Score", "Venue", "Team2Goals",
                                   "Team2Behinds", "Team2Points", "Team1Goals", "Team1Behinds", "Team1Points",
                                   "Team2FinalMargin", "Team1FinalMargin", "AbsFinalMargin", "Result", "Winner", "Loser",
                                   "QtrLength", "RunningQtrLength", "TimePerc", "QtrTimePerc", "GameTime", "Score",
                                   "Team2Worm", "Team1Worm", "WinWorm", "Team2P", "Team2G", "Team2B", "Team2SS",
                                   "Team1P", "Team1G", "Team1B", "Team1SS", "Team2CumPoints",
                                   "Team2CumGoals", "Team2CumBehinds", "Team2CumShots", "Team1CumPoints", "Team1CumGoals",
                                   "Team1CumBehinds", "Team1CumShots", "Team2Margin", "Team1Margin", "WinMargin", "InFront",
                                   "Behind", "AbsMargin", "PreAbsMargin", "ScorerMargin", "ScorerStatus", "PreScorerMargin", 
                                   "PreScorerStatus", "ScorerFinalMargin", "LastLeadingTeam", "LeadChangeFlag", "GameTimeSinceLastScore", "TimePercSinceLastScore",
                                   "LastEvent", "LastScoreTeam")]

flipped_score_progression$Status <- "Flipped"

flipped_score_progression$Result <- ifelse((flipped_score_progression$Team1FinalMargin > 0), "Team 2 win",
                           ifelse((flipped_score_progression$Team1FinalMargin < 0), "Team 1 win",
                                  ifelse((flipped_score_progression$Team1FinalMargin == 0), "Draw",
                                         NA)))

colnames(flipped_score_progression) <- c("GameID", "Quarter", "Dateadj", "Team1", "Team2",
                             "Date.x", "Event", "Timescore", "Teamscore", "Playerscore",
                             "PlayerID", "Season", "Status", "ScoresGameID", "Date.y",
                             "Round", "Team1Score", "Team2Score", "Venue", "Team1Goals",
                             "Team1Behinds", "Team1Points", "Team2Goals", "Team2Behinds", "Team2Points",
                             "Team1FinalMargin", "Team2FinalMargin", "AbsFinalMargin", "Result", "Winner", "Loser",
                             "QtrLength", "RunningQtrLength", "TimePerc", "QtrTimePerc", "GameTime", "Score",
                             "Team1Worm", "Team2Worm", "WinWorm", "Team1P", "Team1G", "Team1B", "Team1SS",
                             "Team2P", "Team2G", "Team2B", "Team2SS", "Team1CumPoints",
                             "Team1CumGoals", "Team1CumBehinds", "Team1CumShots", "Team2CumPoints", "Team2CumGoals",
                             "Team2CumBehinds", "Team2CumShots", "Team1Margin", "Team2Margin", "WinMargin", "InFront",
                             "Behind", "AbsMargin", "PreAbsMargin", "ScorerMargin", "ScorerStatus", "PreScorerMargin", 
                             "PreScorerStatus", "ScorerFinalMargin", "LastLeadingTeam", "LeadChangeFlag", "GameTimeSinceLastScore", "TimePercSinceLastScore",
                             "LastEvent", "LastScoreTeam")

# -----------------------------------------------------------------------------------------------
# 10. Append both data frames together for final output
# -----------------------------------------------------------------------------------------------

score_progression_worm <- rbind(original_score_progression, flipped_score_progression)

rm(flipped_score_progression, prescore, qtrlengths, qtrstarts, team1scores, team2scores, raw_score_progression, raw_match_scores)

# -----------------------------------------------------------------------------------------------
# 11. Output CSV files
# -----------------------------------------------------------------------------------------------

write.csv(original_score_progression, file="C:/Local Code/insightlane/score-progression/output_files/original_score_progression.csv")
write.csv(score_progression_worm, file="C:/Local Code/insightlane/score-progression/output_files/score_progression_worm.csv")



