glimpse(diabetic_data)

diabetic_data <- diabetic_data %>%
  mutate(
    admission_type_consolidated = case_when(
      admission_type_id %in% c(1) ~ 'emergency',
      admission_type_id %in% c(2) ~ 'urgent',
      admission_type_id %in%c(3) ~ 'elective',
      admission_type_id %in% c(4,5,6,7,8) ~ 'admisison_type_other',
      is.na(admission_type_id) ~ 'admisison_type_other', #NA variables do not get special treatment
      TRUE ~ admission_type_id))


diabetic_data <- diabetic_data %>%
  mutate(
    emergency = ifelse(admission_type_consolidated == 'emergency', 1,0),
    urgent = ifelse(admission_type_consolidated == 'urgent', 1,0),
    elective = ifelse(admission_type_consolidated == 'elective', 1,0),
    admisison_type_other = ifelse(admission_type_consolidated == 'admisison_type_other', 1,0))

#features to add to model
  emergency +
  urgent +
  elective +
  admisison_type_other +
