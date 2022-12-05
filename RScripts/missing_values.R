#summary stats
sdf_describe(diabetic_data, cols = c("medical_specialty", "payer_code", "weight"))

#count the number of  NAs per column
NA_count <- diabetic_data %>%
  summarise_all(~sum(as.integer(is.na(.)))) %>%
  collect()

#transpose dataframe (convert from wide to long)
NA_count <- t(NA_count) 

#rename column 1 to missing_values
colnames(NA_count)[1] ="missing_values"

NA_count
