# it is unclear whether missing data for diagnosis means it was not entered or
# whether the patient just didn't have other concurrent issues - the latter potentially
# being a significant predictive value

glimpse(diabetic_data)
head(diabetic_data)


#consolidate ICD9 code
diabetic_data <- diabetic_data %>%
  mutate(
    diag_1_cat = case_when(
      rlike(diag_1, "250") ~ 'diabetes', #case_when works in order
      diag_1 >= 000 & diag_1 < 140 ~ 'infection',
      diag_1 >= 140 & diag_1 < 240 ~ 'neoplasms',
      diag_1 >= 240 & diag_1 < 280 ~ 'endo_metabolic_immunity',
      diag_1 >= 280 & diag_1 < 290 ~ 'haematology',
      diag_1 >= 290 & diag_1 < 320 ~ 'mental',
      diag_1 >= 320 & diag_1 < 390 ~ 'neurology',
      diag_1 >= 390 & diag_1 < 460 ~ 'circulatory',
      diag_1 >= 460 & diag_1 < 520 ~ 'respiratory',
      diag_1 >= 520 & diag_1 < 580 ~ 'digestive',
      diag_1 >= 580 & diag_1 < 630 ~ 'genitourinary',
      diag_1 >= 630 & diag_1 < 680 ~ 'preg_birth_puerperium',
      diag_1 >= 680 & diag_1 < 710 ~ 'dermatology',
      diag_1 >= 710 & diag_1 < 740 ~ 'musculoskeletal',
      diag_1 >= 740 & diag_1 < 760 ~ 'congenital',
      diag_1 >= 760 & diag_1 < 780 ~ 'perinatal',
      diag_1 >= 780 & diag_1 < 800 ~ 'ill_defined',
      diag_1 >= 800 & diag_1 < 1000 ~ 'injury_poisoning',
      rlike(diag_1, "V") | rlike(diag_1, "E") ~ 'supplementary',
      #is.na(diag_1) ~ NA, #NA variables do not get special treatment
      TRUE ~ diag_1),
    diag_2_cat = case_when(
      rlike(diag_2, "250") ~ 'diabetes', #case_when work in order
      diag_2 >= 000 & diag_2 < 140 ~ 'infection',
      diag_2 >= 140 & diag_2 < 240 ~ 'neoplasms',
      diag_2 >= 240 & diag_2 < 280 ~ 'endo_metabolic_immunity',
      diag_2 >= 280 & diag_2 < 290 ~ 'haematology',
      diag_2 >= 290 & diag_2 < 320 ~ 'mental',
      diag_2 >= 320 & diag_2 < 390 ~ 'neurology',
      diag_2 >= 390 & diag_2 < 460 ~ 'circulatory',
      diag_2 >= 460 & diag_2 < 520 ~ 'respiratory',
      diag_2 >= 520 & diag_2 < 580 ~ 'digestive',
      diag_2 >= 580 & diag_2 < 630 ~ 'genitourinary',
      diag_2 >= 630 & diag_2 < 680 ~ 'preg_birth_puerperium',
      diag_2 >= 680 & diag_2 < 710 ~ 'dermatology',
      diag_2 >= 710 & diag_2 < 740 ~ 'musculoskeletal',
      diag_2 >= 740 & diag_2 < 760 ~ 'congenital',
      diag_2 >= 760 & diag_2 < 780 ~ 'perinatal',
      diag_2 >= 780 & diag_2 < 800 ~ 'ill_defined',
      diag_2 >= 800 & diag_2 < 1000 ~ 'injury_poisoning',
      rlike(diag_2, "V") | rlike(diag_2, "E") ~ 'supplementary',
      #is.na(diag_2) ~ NA, #NA variable do not get special treatment
      TRUE ~ diag_2),
    diag_3_cat = case_when(
      rlike(diag_3, "250") ~ 'diabetes', #case_when work in order
      diag_3 >= 000 & diag_3 < 140 ~ 'infection',
      diag_3 >= 140 & diag_3 < 240 ~ 'neoplasms',
      diag_3 >= 240 & diag_3 < 280 ~ 'endo_metabolic_immunity',
      diag_3 >= 280 & diag_3 < 290 ~ 'haematology',
      diag_3 >= 290 & diag_3 < 320 ~ 'mental',
      diag_3 >= 320 & diag_3 < 390 ~ 'neurology',
      diag_3 >= 390 & diag_3 < 460 ~ 'circulatory',
      diag_3 >= 460 & diag_3 < 520 ~ 'respiratory',
      diag_3 >= 520 & diag_3 < 580 ~ 'digestive',
      diag_3 >= 580 & diag_3 < 630 ~ 'genitourinary',
      diag_3 >= 630 & diag_3 < 680 ~ 'preg_birth_puerperium',
      diag_3 >= 680 & diag_3 < 710 ~ 'dermatology',
      diag_3 >= 710 & diag_3 < 740 ~ 'musculoskeletal',
      diag_3 >= 740 & diag_3 < 760 ~ 'congenital',
      diag_3 >= 760 & diag_3 < 780 ~ 'perinatal',
      diag_3 >= 780 & diag_3 < 800 ~ 'ill_defined',
      diag_3 >= 800 & diag_3 < 1000 ~ 'injury_poisoning',
      rlike(diag_3, "V") | rlike(diag_3, "E") ~ 'supplementary',
      #is.na(diag_3) ~ NA, #NA variable do not get special treatment
      TRUE ~ diag_3)
  )

#one_hot_encode diagnosis 1
diabetic_data <- diabetic_data %>%
  mutate(
    diag_1_infection = ifelse(diag_1_cat == 'infection', 1,0),
    diag_1_neoplasms = ifelse(diag_1_cat == 'neoplasms', 1,0),
    diag_1_endo_metabolic_immunity = ifelse(diag_1_cat == 'endo_metabolic_immunity', 1,0),
    diag_1_haematology = ifelse(diag_1_cat == 'haematology', 1,0),
    diag_1_mental = ifelse(diag_1_cat == 'mental', 1,0),
    diag_1_neurology = ifelse(diag_1_cat == 'neurology', 1,0),
    diag_1_circulatory = ifelse(diag_1_cat == 'circulatory', 1,0),
    diag_1_respiratory = ifelse(diag_1_cat == 'respiratory', 1,0),
    diag_1_digestive = ifelse(diag_1_cat == 'digestive', 1,0),
    diag_1_genitourinary = ifelse(diag_1_cat == 'genitourinary', 1,0),
    diag_1_preg_birth_puerperium = ifelse(diag_1_cat == 'preg_birth_puerperium', 1,0),
    diag_1_dermatology = ifelse(diag_1_cat == 'dermatology', 1,0),
    diag_1_musculoskeletal = ifelse(diag_1_cat == 'musculoskeletal', 1,0),
    diag_1_congenital = ifelse(diag_1_cat == 'congenital', 1,0),
    diag_1_perinatal = ifelse(diag_1_cat == 'perinatal', 1,0),
    diag_1_ill_defined = ifelse(diag_1_cat == 'ill_defined', 1,0),
    diag_1_injury_poisoning = ifelse(diag_1_cat == 'injury_poisoning', 1,0),
    diag_1_supplementary = ifelse(diag_1_cat == 'supplementary', 1,0),
    diag_1_diabetes = ifelse(diag_1_cat == 'diabetes', 1,0))

#one_hot_encode diagnosis 2
diabetic_data <- diabetic_data %>%
  mutate(
    diag_2_infection = ifelse(diag_2_cat == 'infection', 1,0),
    diag_2_neoplasms = ifelse(diag_2_cat == 'neoplasms', 1,0),
    diag_2_endo_metabolic_immunity = ifelse(diag_2_cat == 'endo_metabolic_immunity', 1,0),
    diag_2_haematology = ifelse(diag_2_cat == 'haematology', 1,0),
    diag_2_mental = ifelse(diag_2_cat == 'mental', 1,0),
    diag_2_neurology = ifelse(diag_2_cat == 'neurology', 1,0),
    diag_2_circulatory = ifelse(diag_2_cat == 'circulatory', 1,0),
    diag_2_respiratory = ifelse(diag_2_cat == 'respiratory', 1,0),
    diag_2_digestive = ifelse(diag_2_cat == 'digestive', 1,0),
    diag_2_genitourinary = ifelse(diag_2_cat == 'genitourinary', 1,0),
    diag_2_preg_birth_puerperium = ifelse(diag_2_cat == 'preg_birth_puerperium', 1,0),
    diag_2_dermatology = ifelse(diag_2_cat == 'dermatology', 1,0),
    diag_2_musculoskeletal = ifelse(diag_2_cat == 'musculoskeletal', 1,0),
    diag_2_congenital = ifelse(diag_2_cat == 'congenital', 1,0),
    diag_2_perinatal = ifelse(diag_2_cat == 'perinatal', 1,0),
    diag_2_ill_defined = ifelse(diag_2_cat == 'ill_defined', 1,0),
    diag_2_injury_poisoning = ifelse(diag_2_cat == 'injury_poisoning', 1,0),
    diag_2_supplementary = ifelse(diag_2_cat == 'supplementary', 1,0),
    diag_2_diabetes = ifelse(diag_2_cat == 'diabetes', 1,0))

#one_hot_encode diagnosis 3
diabetic_data <- diabetic_data %>%
  mutate(
    diag_3_infection = ifelse(diag_3_cat == 'infection', 1,0),
    diag_3_neoplasms = ifelse(diag_3_cat == 'neoplasms', 1,0),
    diag_3_endo_metabolic_immunity = ifelse(diag_3_cat == 'endo_metabolic_immunity', 1,0),
    diag_3_haematology = ifelse(diag_3_cat == 'haematology', 1,0),
    diag_3_mental = ifelse(diag_3_cat == 'mental', 1,0),
    diag_3_neurology = ifelse(diag_3_cat == 'neurology', 1,0),
    diag_3_circulatory = ifelse(diag_3_cat == 'circulatory', 1,0),
    diag_3_respiratory = ifelse(diag_3_cat == 'respiratory', 1,0),
    diag_3_digestive = ifelse(diag_3_cat == 'digestive', 1,0),
    diag_3_genitourinary = ifelse(diag_3_cat == 'genitourinary', 1,0),
    diag_3_preg_birth_puerperium = ifelse(diag_3_cat == 'preg_birth_puerperium', 1,0),
    diag_3_dermatology = ifelse(diag_3_cat == 'dermatology', 1,0),
    diag_3_musculoskeletal = ifelse(diag_3_cat == 'musculoskeletal', 1,0),
    diag_3_congenital = ifelse(diag_3_cat == 'congenital', 1,0),
    diag_3_perinatal = ifelse(diag_3_cat == 'perinatal', 1,0),
    diag_3_ill_defined = ifelse(diag_3_cat == 'ill_defined', 1,0),
    diag_3_injury_poisoning = ifelse(diag_3_cat == 'injury_poisoning', 1,0),
    diag_3_supplementary = ifelse(diag_3_cat == 'supplementary', 1,0),
    diag_3_diabetes = ifelse(diag_3_cat == 'diabetes', 1,0)
  )
  
list_of_diag1_cat <- c('diag_1_infection',
                      'diag_1_neoplasms', 
                      'diag_1_endo_metabolic_immunity', 
                      'diag_1_haematology',
                      'diag_1_mental',
                      'diag_1_neurology',
                      'diag_1_circulatory',
                      'diag_1_respiratory',
                      'diag_1_digestive',
                      'diag_1_genitourinary',
                      'diag_1_preg_birth_puerperium',
                      'diag_1_dermatology',
                      'diag_1_musculoskeletal',
                      'diag_1_congenital',
                      'diag_1_perinatal',
                      'diag_1_ill_defined',
                      'diag_1_injury_poisoning',
                      'diag_1_supplementary',
                      'diag_1_diabetes')

list_of_diag1_cat <- c('diag_1_infection',
                       'diag_1_neoplasms', 
                       'diag_1_endo_metabolic_immunity', 
                       'diag_1_haematology',
                       'diag_1_mental',
                       'diag_1_neurology',
                       'diag_1_circulatory',
                       'diag_1_respiratory',
                       'diag_1_digestive',
                       'diag_1_genitourinary',
                       'diag_1_preg_birth_puerperium',
                       'diag_1_dermatology',
                       'diag_1_musculoskeletal',
                       'diag_1_congenital',
                       'diag_1_perinatal',
                       'diag_1_ill_defined',
                       'diag_1_injury_poisoning',
                       'diag_1_supplementary',
                       'diag_1_diabetes')

#ICD code is 6 characters in length with a decimal point between the 3rd and 4th number
print(seq(1, 10, by=0.01))
