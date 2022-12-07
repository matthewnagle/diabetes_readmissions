#https://spark.rstudio.com/get-started/model-data.html

diabetic_data_partitions <- diabetic_data %>%
  sdf_random_split(diabetic_data_training = 0.3, diabetic_data_test = 0.7, seed = 1099)

glimpse(diabetic_data_partitions$diabetic_data_training)

