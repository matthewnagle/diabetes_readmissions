library(sparklyr)
library(dplyr)
library(ggplot2)
library(knitr)
library(tidyverse) 
library(lubridate) 
library(maps)
library(ggplot2)
library(countrycode)
library(dbplot)
library(janitor)


## Connecting and read data from csv
sc = spark_connect(master = 'local')
diabetic_data = spark_read_csv(sc, '/Users/matt/Desktop/Dropbox/Home/College/Edinburgh - MSc Data Science/Big Data Analytics/diabetes_readmissions/RawData/diabetic_data.csv') 

##########################
## EXPLORATORY ANALYSIS ##
##########################

glimpse(diabetic_data)

diabetic_data %>% 
  head(15) 

#replace ? with NA
diabetic_data <- diabetic_data %>%
  mutate(across(where(is.character), ~na_if(., "?")))

diabetic_data <- as.data.frame(unclass(diabetic_data), stringsAsFactors = TRUE)


#missing variable
diabetic_data %>%
  filter(is.na(weight)) %>%
  sdf_nrow()

diabetic_data %>%
  filter(is.na(medical_specialty)) %>%
  sdf_nrow()

diabetic_data %>%
  filter(is.na(payer_code)) %>%
  sdf_nrow()

#change variable type
diabetic_data <- diabetic_data %>% 
  mutate(diag_1 = as.character(diag_1))


## Readmissions
diabetic_data %>% 
  count(readmitted) %>%
  mutate(frac = n / sum(n))

## create a new column with readmission <30
diabetic_data = mutate(diabetic_data, early_readmission = ifelse(readmitted == '<30', TRUE, FALSE))
diabetic_data %>% 
  count(early_readmission) %>%
  mutate(frac = n / sum(n))

## proportion of patient gender
diabetic_data %>% 
  count(gender) %>%
  mutate(frac = n / sum(n))

diabetic_data %>%
  group_by(gender) %>%
  tally() %>%
  mutate(frac = n / sum(n))

## breakdown of age
diabetic_data %>% 
  count(age) %>%
  arrange(age)

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

       
## Summary statistics

sdf_describe(diabetic_data, cols = c("time_in_hospital", "num_procedures", "num_lab_procedures"))

#Lab procedures
summarise(diabetic_data, mean_num_lab_procedures = mean(num_lab_procedures))
summarise(diabetic_data, min_num_lab_procedures = min(num_lab_procedures))
summarise(diabetic_data, max_num_lab_procedures = max(num_lab_procedures))
median(pull(diabetic_data, num_lab_procedures))
IQR(pull(diabetic_data, num_lab_procedures))

diabetic_data %>%
  group_by(age) %>%
  summarise(mean_num_lab_procedures = mean(num_lab_procedures)) %>%
  arrange(age)

## Diagnoses
#list the primary diagnoses
diabetic_data %>% 
  count(diag_1) %>%
  #head(15) %>% #reduces to top 15 diagnoses
  arrange(desc(n))
# List the secondary diagnoses
diabetic_data %>% 
  count(diag_2) %>%
  #head(15) %>% #reduces to top 15 diagnoses
  arrange(desc(n))
#list the additional secondary diagnoses
diabetic_data %>% 
  count(diag_3) %>%
  #head(15) %>% #reduces to top 15 diagnoses
  arrange(desc(n))

#count of the number of unique primary Dx (698)
diabetic_data %>%
  summarise(count = n_distinct(diag_1))

#count of the number of unique secondary Dx (749)
diabetic_data %>%
  summarise(count = n_distinct(diag_2))

## STATISTICAL ASSOCIATION

#creating a contingency table

sdf_crosstab(diabetic_data, "early_readmission", "gender")

#create contingency table 'contingency_tbl'
contingency_tbl <- diabetic_data %>% 
  sdf_crosstab("early_readmission", "readmitted") %>%
  collect()

contingency_tbl
glimpse(contingency_tbl)

#change true/false variable to logical
contingency_tbl <- contingency_tbl %>% 
  mutate(early_readmission_readmitted = as.logical(early_readmission_readmitted))

chisq.test(contingency_tbl)

ml_chisquare_test(diabetic_data, features = 'gender', label = "early_readmission")

#correlation - spearman catgorical data has to be ordinal
feature <- c("early_readmission", "num_lab_procedures", "num_procedures", "number_diagnoses") 
ml_corr(diabetic_data, columns = feature, method = "spearman")

#correlation
ml_corr(diabetic_data)

features <- c("early_readmission", "age")
ml_corr(diabetic_data, columns = features, method = "spearman")

ml_corr(diabetic_data, "gender", "early_readmission", method = "spearman")

#linear regression
diabetic_data %>% 
  ml_linear_regression(early_readmission ~ gender) %>%
  summary()

#logistic regression
diabetic_data %>% 
  ml_logistic_regression(early_readmission ~ gender + age)

##PLOTS

# Data manipulations are done first using spark
age_group = diabetic_data %>% 
  count(age) %>%
  arrange(age) %>%
  collect()

# Now use ggplot on the R dataframe age_group
ggplot(aes(as.factor(age), n), data = age_group) +
  geom_col(fill = 'SteelBlue') +
  xlab('Count') +
  ylab('Age group') +
  #coord_flip()

#
dbplot_histogram(diabetic_data, num_lab_procedures)


mean(cars$hp)

cars %>%
  mean(cyl, na.rm = TRUE)

mean(pull(cars, hp))
summarise(cars, mean_hp = mean(hp))

median(pull(cars, hp))

pull(diabetic_data, )

summarise_all(diabetic_data, mean, na.rm = TRUE)
mean_hp
