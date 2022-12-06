glimpse(diabetic_data)

diabetic_data %>%
  group_by(max_glu_serum) %>%
  tally()

diabetic_data <- diabetic_data %>%
  mutate(
    max_glu_serum_none = ifelse(max_glu_serum == 'None', 1,0),
    max_glu_serum_norm = ifelse(max_glu_serum == 'Norm', 1,0),
    max_glu_serum_300 = ifelse(max_glu_serum == '>300', 1,0),
    max_glu_serum_200 = ifelse(max_glu_serum == '>200', 1,0))



diabetic_data %>%
  group_by(A1Cresult) %>%
  tally()

diabetic_data <- diabetic_data %>%
  mutate(
    A1Cresult_none = ifelse(A1Cresult == 'None', 1,0),
    A1Cresult_norm = ifelse(A1Cresult == 'Norm', 1,0),
    A1Cresult_7 = ifelse(A1Cresult == '>7', 1,0),
    A1Cresult_8 = ifelse(A1Cresult == '>8', 1,0))

#model features
max_glu_serum_none +
max_glu_serum_norm +
max_glu_serum_300 +
max_glu_serum_200 +
A1Cresult_none +
A1Cresult_norm +
A1Cresult_7 +
A1Cresult_8 