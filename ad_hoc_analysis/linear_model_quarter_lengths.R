library(dplyr)
library(tidyr)

lm_model_data <- original_score_progression %>% 
        ungroup() %>%
        filter(Event != 'PS' & Event != 'S' & Event != 'F') %>%
        mutate(Round = gsub("R", "", Round)) %>%
        group_by(Season, GameID, Round, Team1, Team2, Quarter, QtrLength, Event) %>%
        summarise(count_events = n()) %>%
        spread(Event, count_events) %>%
        replace(., is.na(.), 0) %>%
        mutate(dummy_2008 = ifelse(Season == 2008, 1, 0),
               dummy_2009 = ifelse(Season == 2009, 1, 0),
               dummy_2010 = ifelse(Season == 2010, 1, 0),
               dummy_2011 = ifelse(Season == 2011, 1, 0),
               dummy_2012 = ifelse(Season == 2012, 1, 0),
               dummy_2013 = ifelse(Season == 2013, 1, 0),
               dummy_2014 = ifelse(Season == 2014, 1, 0),
               dummy_2015 = ifelse(Season == 2015, 1, 0),
               dummy_2016 = ifelse(Season == 2016, 1, 0),
               dummy_2017 = ifelse(Season == 2017, 1, 0),
               dummy_2018 = ifelse(Season == 2018, 1, 0),
               dummy_2019 = ifelse(Season == 2019, 1, 0)
               )

devtools::install_github("jimmyday12/fitzroy")

library(tibble)
library(dplyr)
library(fitzRoy)

afltables_data <- get_afltables_stats(start_date = "2008-01-01", 
                                      end_date = Sys.Date())

afltables_collated <- afltables_data %>%
        ungroup() %>%
        group_by(Season, Round, Home.team, Away.team, Date) %>%
        summarise(total_hitouts = sum(Hit.Outs),
                  total_clearances = sum(Clearances)
        )

lm_model_data_join <- lm_model_data %>%
        left_join(afltables_collated, by = c("Season", "Round", "Team1" = "Home.team", "Team2" = "Away.team"))

model_fit <- lm(data = lm_model_data, formula = QtrLength ~ G + B + RB 
  ## + dummy_2008
   + dummy_2009
   + dummy_2010
   + dummy_2011
   + dummy_2012
   + dummy_2013
   + dummy_2014
   + dummy_2015
   + dummy_2016
   + dummy_2017
   + dummy_2018
   + dummy_2019
   )

summary(model_fit)


model_fit_multiple <- lm(data = lm_model_data, formula = QtrLength ~  B + RB + G * (
                ## dummy_2008
                dummy_2009
                + dummy_2010
                + dummy_2011
                + dummy_2012
                + dummy_2013
                + dummy_2014
                + dummy_2015
                + dummy_2016
                + dummy_2017
                + dummy_2018
                + dummy_2019)
)

summary(model_fit_multiple)


model_fit_multiple_stoppages <- lm(data = lm_model_data_join, formula = QtrLength ~ B + RB + (G 
                                                                                     #+ total_hitouts
                                                                                     + total_clearances
                                                                                     ) * (
        ## dummy_2008
         dummy_2009
        + dummy_2010
        + dummy_2011
        + dummy_2012
        + dummy_2013
        + dummy_2014
        + dummy_2015
        + dummy_2016
        + dummy_2017
        + dummy_2018
        + dummy_2019)
)

summary(model_fit_multiple_stoppages)

lm_model_data_join$resid
