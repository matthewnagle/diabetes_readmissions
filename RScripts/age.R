diabetic_data %>%
  group_by(age) %>%
  tally() %>%
  arrange(age)

diabetic_data <- diabetic_data %>%
  mutate(
    age_cat = case_when(
      age == '[0-10)' | age == '[10-20)'   ~ 0,
      age == '[20-30)' | age == '[30-40)'  ~ 1,
      age == '[40-50)' | age == '[50-60)'  ~ 2,
      age == '[60-70)' | age == '[70-80)'  ~ 3,
      age == '[80-90)' | age == '[90-100)' ~ 4
    ))

diabetic_data <- diabetic_data %>%
  mutate(
    age_contin = case_when(
      age == '[0-10)'    ~ 5,
      age == '[10-20)'   ~ 15,
      age == '[20-30)'   ~ 25,
      age == '[30-40)'   ~ 35,
      age == '[40-50)'   ~ 45,
      age == '[50-60)'   ~ 55,
      age == '[60-70)'   ~ 65,
      age == '[70-80)'   ~ 75,
      age == '[80-90)'   ~ 85,
      age == '[90-100)'  ~ 95
    ))

diabetic_data %>%
  group_by(age_cat) %>%
  tally() %>%
  arrange(age_cat)

glimpse(diabetic_data)


