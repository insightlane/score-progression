library(extrafont)
library(dplyr)
library(ggplot2)
library(tidyr)


group.colors <- c(concedebutwin = "#e7b425", scoretowin = "#f8d159", scoretodraw ="#fce6b2", concedetolose = "#1f77b4", concedetodraw = "#a6cbe5", misstolose = "#2a5783")
group.labels <- c(concedebutwin = "Survived conceded score to win          ", scoretowin = "Kicked score to win          ", scoretodraw ="Kicked score to draw          ", concedetolose = "Conceded score to lose          ", concedetodraw = "Conceded score to draw          ", misstolose = "Missed score to lose          ")


order <- score_progression_worm %>%
        ungroup() %>%
        #filter(Season == 2018 & Round == "R18") %>%
        filter(Event != "PS" & Event != "S" & Event != "F") %>%
        filter(Team1Points == Team1CumPoints, Team2Points == Team2CumPoints) %>%
        group_by(Team1) %>%
        summarise(concedebutwin = sum(1 * ifelse(Winner == Team1 & Teamscore == Team2 & PreScorerMargin > -6 & PreScorerMargin < 0, 1, 0)),
                  scoretowin = sum(ifelse(Winner == Team1 & ScorerStatus != PreScorerStatus, 1, 0)),
                  scoretodraw = sum(ifelse(Winner == "Draw" & Teamscore == Team1 & ScorerStatus != PreScorerStatus, 1, 0)),
                  
                  
                  concedetolose = sum(-1 * ifelse(Loser == Team1 & ScorerStatus != PreScorerStatus, 1, 0)),
                  concedetodraw = sum(-1 * ifelse(Winner == "Draw" & Teamscore == Team2 & ScorerStatus != PreScorerStatus, 1, 0)),
                  misstolose = sum(-1 * ifelse(Loser == Team1 & Teamscore == Team1 & PreScorerMargin > -6 & PreScorerMargin < 0, 1, 0))
                  
        ) %>%
        #mutate(misstolose = ifelse(Team1 == "Melbourne", misstolose - 1, misstolose)) %>%
        #mutate(concedebutwin = ifelse(Team1 == "Geelong", concedebutwin + 1, concedebutwin)) %>%
        mutate(Team1 = ifelse(Team1 == "Greater Western Sydney", "GW Sydney", Team1)) %>%
        mutate(nogames = abs(scoretowin) + abs(concedebutwin) + abs(scoretodraw) + abs(concedetolose) + abs(misstolose) + abs(concedetodraw)) %>%
        mutate(netpoints = scoretowin + concedebutwin + 0.5*scoretodraw + concedetolose + misstolose + 0.5 * concedetodraw) %>%
        arrange(netpoints, nogames)

teamorder <- order$Team1

graph <- score_progression_worm %>%
        ungroup() %>%
        
        filter(Event != "PS" & Event != "S" & Event != "F") %>%
        filter(Team1Points == Team1CumPoints, Team2Points == Team2CumPoints) %>%
        group_by(Team1) %>%
        summarise(concedebutwin = sum(1 * ifelse(Winner == Team1 & Teamscore == Team2 & PreScorerMargin > -6 & PreScorerMargin < 0, 1, 0)),
                  scoretowin = sum(ifelse(Winner == Team1 & ScorerStatus != PreScorerStatus, 1, 0)),
                  scoretodraw = sum(ifelse(Winner == "Draw" & Teamscore == Team1 & ScorerStatus != PreScorerStatus, 1, 0)),
                  
                  
                  concedetolose = sum(-1 * ifelse(Loser == Team1 & ScorerStatus != PreScorerStatus, 1, 0)),
                  concedetodraw = sum(-1 * ifelse(Winner == "Draw" & Teamscore == Team2 & ScorerStatus != PreScorerStatus, 1, 0)),
                  misstolose = sum(-1 * ifelse(Loser == Team1 & Teamscore == Team1 & PreScorerMargin > -6 & PreScorerMargin < 0, 1, 0))
                  
        ) %>%
        #mutate(misstolose = ifelse(Team1 == "Melbourne", misstolose - 1, misstolose)) %>%
        #mutate(concedebutwin = ifelse(Team1 == "Geelong", concedebutwin + 1, concedebutwin)) %>%
        gather(result, number, concedebutwin:misstolose) %>%
        mutate(Team1 = ifelse(Team1 == "Greater Western Sydney", "GW Sydney", Team1)) %>%
        mutate(result = as.factor(result)) %>%
        mutate(result = factor(result, levels = c("scoretodraw", "scoretowin", "concedebutwin", "concedetodraw", "concedetolose", "misstolose")))


ggplot(data = graph, aes(x = Team1, y = number, fill = identity(result))) +
        geom_bar(stat = "identity") + 
        coord_flip() +
        scale_fill_manual(values=group.colors,
                          labels = group.labels,
                          name = "Outcome") +
        scale_x_discrete(limits = teamorder) +
        scale_y_continuous(breaks = c(-10:10)*2) +
        theme_minimal() + 
        theme(plot.title = element_text(family = "Trebuchet MS", color="#555555", face = "bold", size = 28),
              plot.subtitle = element_text(family = "Trebuchet MS", color="#555555", size = 22),
              axis.text.x=element_text(size = 14, angle = 90, vjust = 0.5), 
              axis.text.y=element_text(size = 14), 
              legend.title =element_text(size = 16), 
              legend.text = element_text(size = 14),
              legend.position="bottom",
              legend.direction = "horizontal",
              axis.title = element_text(size = 16), 
              plot.caption = element_text(size = 12)) +
        labs(title = "Eight times the Cats have kicked the winner since 2008",
             subtitle = "All match results decided by the final score | 2008-2018 AFL seasons",
             caption = "Source: AFL Tables", 
             y = "Number of matches", x = "Team") 


geelonggraph <- score_progression_worm %>%
        ungroup() %>%
        
        filter(Event != "PS" & Event != "S" & Event != "F") %>%
        filter(Team1Points == Team1CumPoints, Team2Points == Team2CumPoints) %>%
        filter(Team1 == "Geelong") %>%
        mutate(scoretowin = ifelse(Winner == Team1 & ScorerStatus != PreScorerStatus, 1, 0)) %>%
        filter(scoretowin == 1) %>%
        select(Season, Round, Team2, Playerscore, Event) %>%
        arrange(Season)