#install.packages("extrafont")

#font_import(pattern="[T/t]rebuc")
library(extrafont)
library(dplyr)
library(ggplot2)

ladderscores %>%
        filter(RoundsRemaining == 0) %>%
        group_by(Season) %>%
        summarise(games = max(Playedbye),
                  maxpp = max(RunningPremPoints),
                  minpp = min(RunningPremPoints),
                  diff = maxpp - minpp) %>%
        filter(games == 22) %>%
        arrange(diff)

ladderscores %>%
        filter(RoundsRemaining == 0 & Season >= 1970) %>%
        ggplot() +
        geom_point(aes(x = Season, y = RunningPremPoints, colour = RunningPercentage, group = LadderRank), size = 3) +
        geom_line(aes(x = Season, y = RunningPremPoints, colour = RunningPercentage, group = LadderRank), size = 1.5) +
        theme_minimal() +
        theme(plot.title = element_text(family = "Trebuchet MS", color="#555555", face = "bold", size = 28),
              plot.subtitle = element_text(family = "Trebuchet MS", color="#555555", size = 19),
              axis.text.x=element_text(size = 12, angle = 90, vjust = 0.5), 
              axis.text.y=element_text(size = 12), 
              axis.title = element_text(size = 14, face = "bold"), 
              strip.text = element_text(size = 14), 
              plot.caption = element_text(size = 14),
              legend.text = element_text(size = 14),
              legend.title = element_text(size = 14, face = "bold"),
        ) +
        labs(title = "The 2017 AFL season was the most bunched since 1998",
             subtitle = "End of season points and percentage by ladder position | 1970 to 2017 VFL/AFL seasons",
             caption = "Source: AFL Tables",
             colour = "Percentage",             
             x = "Season", y = "Premiership points") +
        scale_colour_gradient2(low = "#1f77b4", mid = "grey",
                               high = "#f8d159", midpoint = 100, space = "Lab",
                               na.value = "grey50", guide = "colourbar")

, 
ladderscores %>%
        filter(RoundsRemaining == 0) %>%
        ggplot() +
        geom_point(aes(x = Season, y = RunningPremPoints, colour = RunningPercentage))