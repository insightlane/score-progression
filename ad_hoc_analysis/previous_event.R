library(dplyr)
library(ggplot2)

original_score_progression %>%
        filter(Event == "G" | Event == "B" | Event == "RB") %>%
        group_by(LastScoreTeam, LastEvent) %>%
        summarise(G = sum(ifelse(Event == "G", 1, 0)),
                  B = sum(ifelse(Event == "B", 1, 0)),
                  RB = sum(ifelse(Event == "RB", 1, 0))
                  )



original_score_progression %>%
        filter(Event == "G" | Event == "B" | Event == "RB") %>%
        filter(LastEvent == "PS")


original_score_progression %>%
        filter(Event != "PS") %>%
        ungroup() %>%
        group_by(GameID) %>%
        mutate(prev_event = lag(Event),
               prev_score_teams = ifelse(Teamscore == lag(Teamscore), "Same", "Other"),
               points_direction = ifelse(prev_score_teams == "Same", Score, -1 * Score)) %>%
        ungroup() %>%
        group_by(Season, prev_event) %>%
        filter(prev_event == "B" | prev_event == "RB" | prev_event == "G") %>%
        summarise(count = n(),
                  sum_next_event = sum(points_direction),
                  ave_next_event = sum_next_event/count) %>%
        ggplot(aes(x = Season, y = ave_next_event, group = prev_event, colour = prev_event)) +
        geom_line()





test <- original_score_progression %>%
        ungroup() %>%
        group_by(GameID, Quarter) %>%
        mutate(prev_event = lag(Event),
               prev_score_teams = ifelse(Teamscore == lag(Teamscore), "Same", "Other"),
               points_direction = ifelse(prev_score_teams == "Same", Score, -1 * Score)) %>%
        ungroup() %>%
        group_by(Season, prev_event) %>%
        filter(prev_event == "B" | prev_event == "RB" | prev_event == "G") %>%
        summarise(count = n(),
                  sum_next_event = sum(points_direction),
                  ave_next_event = sum_next_event/count) %>%
        ggplot(aes(x = Season, y = ave_next_event, group = prev_event, colour = prev_event)) +
        geom_point() + 
        geom_line()
