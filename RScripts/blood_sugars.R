diabetic_data %>%
  group_by(max_glu_serum) %>%
  tally()

diabetic_data %>%
  group_by(A1Cresult) %>%
  tally()
