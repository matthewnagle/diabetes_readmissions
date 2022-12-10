diabetic_data %>%
  group_by(gender) %>%
  tally()


diabetic_data <- diabetic_data %>%
  mutate(
    female = ifelse(race == 'Female', 1,0),
    gender_unknown_invalid = ifelse(race == 'Unknown/Invalid', 1,0),
    male = ifelse(race == 'Male', 1,0)
  )
