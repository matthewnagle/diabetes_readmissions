#note that this needs to be done separately for training and test datasets
glimpse(diabetic_data)

cars = mutate(cars, z_age_contin = (age_contin - mean(age_contin, na.rm = TRUE)) / sd(age_contin, na.rm = TRUE))

diabetic_data <- diabetic_data %>%
  mutate(
    z_age_contin = (age_contin - mean(age_contin, na.rm = TRUE)) / sd(age_contin, na.rm = TRUE),
    z_time_in_hospital = (age_contin - mean(time_in_hospital, na.rm = TRUE)) / sd(time_in_hospital, na.rm = TRUE),
    z_num_lab_procedures = (age_contin - mean(num_lab_procedures, na.rm = TRUE)) / sd(num_lab_procedures, na.rm = TRUE),
    z_num_procedures = (age_contin - mean(num_procedures, na.rm = TRUE)) / sd(num_procedures, na.rm = TRUE),
    z_number_outpatient = (age_contin - mean(number_outpatient, na.rm = TRUE)) / sd(number_outpatient, na.rm = TRUE),
    z_number_emergency = (age_contin - mean(number_emergency, na.rm = TRUE)) / sd(number_emergency, na.rm = TRUE),
    z_number_inpatient = (age_contin - mean(number_inpatient, na.rm = TRUE)) / sd(number_inpatient, na.rm = TRUE),
    z_number_diagnoses = (age_contin - mean(number_diagnoses, na.rm = TRUE)) / sd(number_diagnoses, na.rm = TRUE)
  )

#features to add
z_age_contin +
z_time_in_hospital +
z_num_lab_procedures +
z_num_procedures +
z_number_outpatient +
z_number_emergency +
z_number_inpatient +
z_number_diagnoses +