## create a new column with readmission <30
diabetic_data = mutate(diabetic_data, early_readmission = ifelse(readmitted == '<30', 1, 0))

diabetic_data %>%
  group_by(early_readmission) %>%
  tally()

glimpse(diabetic_data)
