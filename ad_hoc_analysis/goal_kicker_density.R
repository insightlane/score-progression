library(dplyr)
library(ggplot2)

original_score_progression %>%
        ungroup() %>%
        filter(Playerscore == "Lance Franklin" & Event == "G") %>%
        mutate(TimePercbins = floor(TimePerc * 100) / 100) %>%
        group_by(TimePercbins) %>%
        summarise(count = n()) %>%
        ggplot(aes(x = TimePercbins, y = count, group = 1)) + 
        geom_point() +
        stat_smooth(method = "auto", se = TRUE)


original_score_progression %>%
        ungroup() %>%
        filter(Playerscore == "Lance Franklin" & Event == "G") %>%
        ggplot(aes(x = TimePerc, y = Event, group = 1)) + 
        geom_point() +

        stat_smooth(method = "glm", se = TRUE) 


original_score_progression %>%
        ungroup() %>%
        group_by(Quarter) %>%
        filter(Playerscore == "Lance Franklin" & Event == "G") %>%
        ggplot(aes(x = TimePerc, y=..density..)) + 
        geom_histogram(stat_bin(binwidth = 1)) +
        geom_density() +
        facet_wrap(~ Quarter, ncol = 4, scales = "free_x")
