diabetic_data %>%
  group_by(race) %>%
  tally()


diabetic_data <- diabetic_data %>%
  mutate(
    Asian = ifelse(race == 'Asian', 1,0),
    AfricanAmerican = ifelse(race == 'AfricanAmerican', 1,0),
    Caucasian = ifelse(race == 'Caucasian', 1,0),
    Hispanic = ifelse(race == 'Hispanic', 1,0),
    Unknown = ifelse(is.na(race), 1,0),
  )


list_of_race_cat <- c('Asian',
                       'AfricanAmerican', 
                       'Caucasian', 
                       'Hispanic',
                       'Unknown'
                       )

glimpse(diabetic_data)
