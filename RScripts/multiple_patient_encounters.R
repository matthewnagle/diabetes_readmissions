##total number of observation (101766)
pull(diabetic_data, patient_nbr)%>%
  length()

#total number of individual patients 71518
diabetic_data %>%
  summarise(count = n_distinct(patient_nbr))

#count of the number of unique values (54745)
diabetic_data %>%
  group_by(patient_nbr) %>%
  filter(n()==1) %>%
  sdf_nrow()

#number of duplicate values (47021)
diabetic_data %>%
  group_by(patient_nbr) %>%
  filter(n()>1) %>%
  sdf_nrow()

#number of patient with repeat values (16773)
diabetic_data %>%
  group_by(patient_nbr) %>%
  filter(n()>1) %>%
  tally() %>%
  sdf_nrow()

#group by patient number then select only the earliest patient encounter
diabetic_data <- diabetic_data %>% 
  group_by(patient_nbr) %>%
  slice_min(encounter_id) %>% #slice_min selects the rows with lowest values
  ungroup()

#patient_nbr and ecnounter_id are now redundant so remove them
diabetic_data <- select(diabetic_data, -c(patient_nbr, encounter_id))

glimpse(diabetic_data)
