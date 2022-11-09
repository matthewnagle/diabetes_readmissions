library(sparklyr)
library(dplyr)
library(ggplot2)
library(knitr)
library(tidyverse) 
library(lubridate) 
library(maps)
library(ggplot2)
library(countrycode)

## Connecting and read data from csv
sc = spark_connect(master = 'local')
diabetic_data = spark_read_csv(sc, '/Users/matt/Desktop/Dropbox/Home/College/Edinburgh - MSc Data Science/Big Data Analytics/diabetes_readmissions/RawData/diabetic_data.csv') 

##########################
## EXPLORATORY ANALYSIS ##
##########################

glimpse(diabetic_data)
diabetic_data %>% 
  head(15)

## Readmissions
diabetic_data %>% 
  count(readmitted)

## create a new column with readmission <30
diabetic_data = mutate(diabetic_data, early_readmission = ifelse(readmitted == '<30', TRUE, FALSE))
diabetic_data %>% 
  count(early_readmission)

## proportion of patient gender
diabetic_data %>% 
  count(gender)

## Summary statistics
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



##PLOTS

# Data manipulations are done first using spark
car_group = cars %>%
  group_by(cyl) %>%
  summarise(mpg = sum(mpg, na.rm = TRUE)) %>%
  # collect brings the Spark dataframe back to a regular R dataframe
  collect()

# Now use ggplot on the R dataframe car_group
ggplot(aes(as.factor(cyl), mpg), data = car_group) +
  geom_col(fill = 'SteelBlue') +
  xlab('Cylinders') +
  coord_flip()


mean(cars$hp)

cars %>%
  mean(cyl, na.rm = TRUE)

mean(pull(cars, hp))
summarise(cars, mean_hp = mean(hp))

median(pull(cars, hp))

summarise_all(diabetic_data, mean, na.rm = TRUE)
mean_hp
