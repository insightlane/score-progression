library(dplyr)

breaking_game_in_two <- score_progression_worm %>%
        filter(Event == "S" | Event == "G" | Event == "B" | Event == "RB") %>%
        ungroup() %>%
        group_by(GameID, Season, Round, Team1, Team2) %>%
        mutate(most_negative_margin = min(Team1Margin),
               most_positve_margin = max(Team1Margin),
               gap_most_negative_to_final_margin = Team1FinalMargin - most_negative_margin,
               gap_most_positive_to_final_margin = Team1FinalMargin - most_positve_margin,
               team_1_biggest_gap = ifelse(abs(gap_most_positive_to_final_margin) >= abs(gap_most_negative_to_final_margin), gap_most_positive_to_final_margin, gap_most_negative_to_final_margin),
               abs_team_1_biggest_gap = abs(team_1_biggest_gap),
               
               team_1_score_gap = Team1FinalMargin - Team1Margin,
               abs_team_1_score_gap = abs(team_1_score_gap)
               ) %>%
        #filter(Season == 2020 & Round == "R14" & Team1 == "Geelong") %>%
        filter(Season == 2020) %>%
        filter(abs_team_1_biggest_gap == abs_team_1_score_gap) %>%
        distinct(GameID, Season, Round, Team1, Team2, Team1Points, Team2Points, AbsFinalMargin, Team1Margin, Team1FinalMargin, team_1_biggest_gap, abs_team_1_biggest_gap) %>%
        mutate(team_1_positive_difference = pmax(Team1Margin, team_1_biggest_gap),
               team_1_negative_difference = pmin(Team1Margin, team_1_biggest_gap),
               team_1_first_game_margin = Team1Margin,
               team_1_second_game_margin = ifelse(Team1Margin == team_1_positive_difference, team_1_negative_difference, team_1_positive_difference),
               sum_of_differences = abs(team_1_positive_difference) + abs(team_1_negative_difference),
               total_match_score = Team1Points + Team2Points) 
# %>%


write.csv(breaking_game_in_two, file="C:/Local Code/insightlane/score-progression/output_files/breaking_game_in_two.csv")