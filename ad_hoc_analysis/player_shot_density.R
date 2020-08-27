library(extrafont)
library(dplyr)
library(ggplot2)

booktimes2017 %>%
        filter((Event == "G" | Event == "B") & Season == 2017) %>%
        filter( PlayerID == 4169 | PlayerID == 12199 | PlayerID == 4065 | PlayerID == 12289 | PlayerID == 11713 | PlayerID == 11576) %>%
        group_by(PlayerID) %>%
        mutate(ngames = n_distinct(GameID),
               score = 1/ngames) %>%
        #select(PlayerID, Playerscore, Event, ngames, score)
        ggplot(aes(x = AdjustedTimePerc, ..density..)) +
        #geom_histogram(aes(group = Playerscore, fill = Playerscore), position="identity", bins = 20) +
        stat_density(aes(group = Playerscore, color = Playerscore), position = "identity", geom = "line", size = 2, alpha = 0.8, adjust = 1, trim = FALSE) +
        theme_minimal() + 
        theme(plot.title = element_text(family = "Trebuchet MS", color="#555555", face = "bold", size = 28),
              plot.subtitle = element_text(family = "Trebuchet MS", color="#555555", size = 19),
              axis.text.x=element_text(size = 16, angle = 90, vjust = 0.5),
              axis.text.y=element_blank(),
              axis.title = element_text(size = 18, face = "bold"), 
              strip.text = element_text(size = 14), 
              plot.caption = element_text(size = 14),
              legend.text = element_text(size = 18),
              legend.title = element_text(size = 18, face = "bold")
              #, legend.position="bottom"
        ) +
        scale_x_continuous(labels = scales::percent) + 
        scale_color_manual(values = c("saddlebrown", "forestgreen", "black", "dodgerblue1", "red", "goldenrod2")) +
        labs(title = "Full-forwards compared (continued)",
             subtitle = "Kernel density estimate of the game time of a player's score | 2017 AFL season",
             caption = "Source: AFL Tables",
             colour = "Player",             
             x = "Game duration (percentage)", y = "Score density") 


        
original_score_progression %>%
        filter((Event == "G" | Event == "B") & Season == 2017) %>%
        group_by(PlayerID, Playerscore) %>%
        summarise(goalstotal = n()) %>%
        arrange(-goalstotal) 

original_score_progression %>%
        filter(Event == "G" & Season == 2017) %>%
        filter(PlayerID == 12199 | PlayerID == 12289) %>%
        group_by(PlayerID, Playerscore, Quarter) %>%
        summarise(goalstotal = n()) %>%
        arrange(-goalstotal)


