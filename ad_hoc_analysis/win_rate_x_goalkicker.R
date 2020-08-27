library(extrafont)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)

        
score_progression_worm %>%
        mutate(totalgoals = Team1CumGoals + Team2CumGoals) %>%
        filter(Event == "G" & Team1 == Teamscore) %>%
        ##filter(Round == "EF" | Round == "QF") %>%
        group_by(totalgoals) %>%
        summarise(count = n(),
                  count_win = sum(ifelse(Team1FinalMargin > 0, 1, 0)),
                  winrate = count_win/count) %>%
        ggplot(aes(x = totalgoals, y = winrate)) +
        geom_point(aes(size = count), colour = "#f8d159") + 
        stat_smooth(weights=count, colour = "#1f77b4") + 

        
        scale_y_continuous(labels = scales::percent) +
        coord_cartesian(ylim = c(0, 1)) +
        theme_minimal() + 
        theme(plot.title = element_text(family = "Trebuchet MS", color="#555555", face = "bold", size = 28),
              plot.subtitle = element_text(family = "Trebuchet MS", color="#555555", size = 18),
              axis.text.x=element_text(size = 16, vjust = 0.5), 
              axis.text.y=element_text(size = 16), 
              legend.title =element_text(size = 16, face = "bold"), 
              legend.text = element_text(size = 14),
              axis.title = element_text(size = 16, face = "bold"), 
              plot.caption = element_text(size = 12)) +
        labs(title = "Win rate by team kicking nth goal of the game",
             subtitle = "Proportion of matches won by the team kicking the nth goal of each game| 2008-2017 AFL seasons",
             caption = "Source: AFL Tables", 
             size = "No. games",
             y = "Proportion of matches won (%)", x = "nth goal of the game")