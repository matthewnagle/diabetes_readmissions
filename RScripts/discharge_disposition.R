glimpse(diabetic_data)

diabetic_data <- diabetic_data %>%
  mutate(
    discharge_disposition_consolidated = case_when(
      discharge_disposition_id %in% c(1) ~ 'home',
      discharge_disposition_id %in% c(2,3,4,5,10,22,23,24,30,27,28,29) ~ 'healthcare_facility',
      discharge_disposition_id %in%c(6,8) ~ 'home_with_help',
      discharge_disposition_id %in% c(6,8) ~ 'AMA',
      discharge_disposition_id %in% c(9) ~ 'admitted',
      discharge_disposition_id %in% c(11,13,14,15,19,20,21) ~ 'hospice_expired',
      discharge_disposition_id %in% c(12,16,27) ~ 'outpatient',
      discharge_disposition_id %in% c(18,25,26) ~ 'unknown_discharge_disposition',
      is.na(discharge_disposition_id) ~ 'unknown_discharge_disposition', #NA variables do not get special treatment
      TRUE ~ discharge_disposition_id))


#one_hot_encode discharge_disposition_consolidated
diabetic_data <- diabetic_data %>%
  mutate(
    home = ifelse(discharge_disposition_consolidated == 'home', 1,0),
    healthcare_facility = ifelse(discharge_disposition_consolidated == 'healthcare_facility', 1,0),
    home_with_help = ifelse(discharge_disposition_consolidated == 'home_with_help', 1,0),
    AMA = ifelse(discharge_disposition_consolidated == 'AMA', 1,0),
    admitted = ifelse(discharge_disposition_consolidated == 'admitted', 1,0),
    hospice_expired = ifelse(discharge_disposition_consolidated == 'hospice_expired', 1,0),
    outpatient = ifelse(discharge_disposition_consolidated == 'outpatient', 1,0),
    unknown_discharge_disposition = ifelse(discharge_disposition_consolidated == 'unknown_discharge_disposition', 1,0))

#features to add to model
home +
healthcare_facility +
home_with_help +
AMA +
admitted +
hospice_expired +
outpatient +
unknown_discharge_disposition +
