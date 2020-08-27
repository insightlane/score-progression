#binned time

library(dplyr)

# Gives overview of binned time for game 184 and Quarter 2

score_progression_worm %>%
        ungroup() %>%
        mutate(Season = as.numeric(format(as.Date(Date.x, "%d-%b-%Y"), "%Y"))) %>%
        filter(Event != "PS" & Event != "S" & Event != "F" & Season == 2016) %>%
        group_by(GameID, Team1) %>%
        arrange(GameID, Team1, TimePerc) %>%
        mutate(TimePercBin = floor(TimePerc/0.01)*0.01 + 0.01) %>%
        ungroup() %>%
        group_by(GameID, Team1, TimePercBin) %>%
        mutate(runcount = dense_rank(desc(TimePerc))) %>%
        mutate(count = n()) %>%
        filter(runcount == 1) %>%
        filter(GameID == 184, TimePercBin > 0.25) %>%
        select(Season, GameID, Team1, Team2, Result, TimePerc, TimePercBin, count, runcount) 



### Creates new data frame with margin for eventual winner by 1% bucket

winprobtimes <- data.frame(GameID = rep(1:max(score_progression_worm$GameID),101), 
                           TimePercBin = rep(seq(0.00, 1, 0.01), max(score_progression_worm$GameID)),
                           Margin = 0
                           )

winprobtimes <- winprobtimes[order(winprobtimes$GameID, winprobtimes$TimePercBin),]

row.names(winprobtimes) <- NULL
 
for(i in 1:nrow(winprobtimes)){
#for(i in 1:200){
        winprobtimes[i, ]$Margin <- sum(score_progression_worm$Team1Worm[
                score_progression_worm$GameID == winprobtimes[i, ]$GameID & score_progression_worm$Score > 0 & 
                        score_progression_worm$Team1 == score_progression_worm$Winner & 
                        score_progression_worm$TimePerc < winprobtimes[i, ]$TimePerc])
        
}

write.csv(winprobtimes, file="winprobtimes.csv")

### Creates new data frame with margin by 1% bucket 
        
