library(extrafont)
library(dplyr)
library(ggplot2)
library(tidyr)

margin <- 6

order <- scoresadj %>%
        ungroup() %>%
        
        
        filter(Season >= 2010) %>%
        group_by(Team1) %>%
        summarise(losses = -1 * sum(ifelse(Winner == Team2 & AbsFinalMargin <= margin, 1, 0)),
                  wins = sum(ifelse(Winner == Team1 & AbsFinalMargin <= margin, 1, 0))
        ) %>%
        mutate(Team1 = ifelse(Team1 == "Greater Western Sydney", "GW Sydney", Team1)) %>%
        mutate(netpoints = wins + losses) %>%
        arrange(netpoints)

teamorder <- order$Team1


group.colors <- c(losses = "#1f77b4", wins = "#f8d159")
group.labels <- c(losses = "#1f77b4", wins = "#f8d159")

scoresadj %>%
        ungroup() %>%
        
        filter(Season >= 2010) %>%
        group_by(Team1) %>%
        summarise(losses = -1 * sum(ifelse(Winner == Team2 & AbsFinalMargin <= margin, 1, 0)),
                  wins = sum(ifelse(Winner == Team1 & AbsFinalMargin <= margin, 1, 0))
        ) %>%
        mutate(Team1 = ifelse(Team1 == "Greater Western Sydney", "GW Sydney", Team1)) %>%
        
        gather(result, number, losses:wins) %>%
        ggplot(aes(x = Team1, y = number, fill = result)) +
        geom_bar(stat = "identity") + 
        coord_flip() +
        scale_fill_manual(values=group.colors,
                          name = "Result", 
                          labels = c("Losses", 
                                     "Wins")) +
        scale_x_discrete(limits = teamorder) +
        scale_y_continuous(breaks = c(-10:10) * 2) +
        theme_minimal() + 
        theme(plot.title = element_text(family = "Trebuchet MS", color="#555555", face = "bold", size = 24),
              plot.subtitle = element_text(family = "Trebuchet MS", color="#555555", size = 18),
              axis.text.x=element_text(size = 12, angle = 90, vjust = 0.5), 
              axis.text.y=element_text(size = 12), 
              legend.title =element_text(size = 14), 
              legend.text = element_text(size = 12),
              axis.title = element_text(size = 14), 
              plot.caption = element_text(size = 10)) +
        labs(title = "Decided in a nailbiter...",
             subtitle = "Matches decided by a goal or less | 2010-2017 AFL seasons",
             caption = "Source: AFL Tables", 
             y = "Number of matches", x = "Team")