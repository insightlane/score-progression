library(dplyr) 
library(ggplot2)

        
        
score_progression_worm %>%
        ungroup() %>%
        filter(Season == 2018) %>%
        #filter(Season == 2018 & Team1 == "Carlton" & Team2 == "Richmond") %>%
        mutate(Time5Percbins = floor(TimePerc * 12) / 12) %>%
        group_by(ScoresGameID, Season, Team1, Team2, Time5Percbins) %>%
        summarise(sum_margin = sum(Team1Worm)) %>%
        mutate(rank = rank(-sum_margin, ties.method = c("random"))) %>%
        ggplot(aes(x = rank, y = sum_margin, group = Team1)) +
        geom_point(aes(group = Time5Percbins), alpha = 0.05) +
        theme_minimal() +
        facet_wrap(~ Team1) +
        stat_smooth(se = TRUE) +
        geom_hline(yintercept = 0)
        


