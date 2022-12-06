diabetic_data %>%
  group_by(race) %>%
  tally()


diabetic_data <- diabetic_data %>%
  mutate(
    asian = ifelse(race == 'Asian', 1,0),
    african_american = ifelse(race == 'AfricanAmerican', 1,0),
    caucasian = ifelse(race == 'Caucasian', 1,0),
    hispanic = ifelse(race == 'Hispanic', 1,0),
    unknown_race = ifelse(is.na(race), 1,0),
  )


list_of_race_cat <- c('Asian',
                       'AfricanAmerican', 
                       'Caucasian', 
                       'Hispanic',
                       'Unknown_race'
                       )

glimpse(diabetic_data)
