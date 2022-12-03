glimpse(diabetic_data)
head(diabetic_data)

#change variable type -change ICD code from character to numeric
diabetic_data <- diabetic_data %>% 
  mutate(
    diag_1 = as.numeric(diag_1),
    diag_2 = as.numeric(diag_2),
    diag_3 = as.numeric(diag_3)
    )

#consolidate ICD9 code
diabetic_data <- diabetic_data %>%
  mutate(
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
      TRUE ~ diag_3
    )
  )

diabetic_data <- diabetic_data %>%
  mutate(
    infection = ifelse(diag_3_cat == 'infection', 1,0),
    neoplasms = ifelse(diag_3_cat == 'neoplasms', 1,0),
    endo_metabolic_immunity = ifelse(diag_3_cat == 'endo_metabolic_immunity', 1,0),
    haematology = ifelse(diag_3_cat == 'haematology', 1,0),
    mental = ifelse(diag_3_cat == 'mental', 1,0),
    neurology = ifelse(diag_3_cat == 'neurology', 1,0),
    circulatory = ifelse(diag_3_cat == 'circulatory', 1,0),
    respiratory = ifelse(diag_3_cat == 'respiratory', 1,0),
    digestive = ifelse(diag_3_cat == 'digestive', 1,0),
    genitourinary = ifelse(diag_3_cat == 'genitourinary', 1,0),
    preg_birth_puerperium = ifelse(diag_3_cat == 'preg_birth_puerperium', 1,0),
    dermatology = ifelse(diag_3_cat == 'dermatology', 1,0),
    musculoskeletal = ifelse(diag_3_cat == 'musculoskeletal', 1,0),
    congenital = ifelse(diag_3_cat == 'congenital', 1,0),
    perinatal = ifelse(diag_3_cat == 'perinatal', 1,0),
    ill_defined = ifelse(diag_3_cat == 'ill_defined', 1,0),
    injury_poisoning = ifelse(diag_3_cat == 'injury_poisoning', 1,0),
    supplementary = ifelse(diag_3_cat == 'supplementary', 1,0),
    diabetes = ifelse(diag_3_cat == 'diabetes', 1,0)
  )
    

#ICD code is 6 characters in length with a decimal point between the 3rd and 4th number
print(seq(1, 10, by=0.01))
