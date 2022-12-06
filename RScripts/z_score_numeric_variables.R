#note that this needs to be done separately for training and test datasets
glimpse(diabetic_data)

cars = mutate(cars, z_age_contin = (age_contin - mean(age_contin, na.rm = TRUE)) / sd(age_contin, na.rm = TRUE))

diabetic_data <- diabetic_data %>%
  mutate(
    z_age_contin = (age_contin - mean(age_contin, na.rm = TRUE)) / sd(age_contin, na.rm = TRUE)

  )
