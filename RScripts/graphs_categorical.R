##PLOTS
glimpse(diabetic_data)
dbplot_histogram(diabetic_data, age_contin)

library(cowplot)

# Data manipulations are done first using spark
age_group = diabetic_data %>% 
  count(age) %>%
  arrange(age) %>%
  collect()

race_group = diabetic_data %>% 
  count(race) %>%
  arrange(race) %>%
  collect()

gender_group = diabetic_data %>% 
  count(gender) %>%
  arrange(gender) %>%
  collect()

discharge_disposition_group = diabetic_data %>% 
  count(discharge_disposition_consolidated) %>%
  arrange(discharge_disposition_consolidated) %>%
  collect()


# Now use ggplot on the R dataframe age_group
age_plot <- 
  ggplot(aes(as.factor(age), n), data = age_group) +
  geom_col(fill = 'SteelBlue') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('Age group') +
  ylab('Count')
#coord_flip()

race_plot <-
  ggplot(aes(as.factor(race), n), data = race_group) +
  geom_col(fill = 'SteelBlue') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('Race') +
  ylab('Count')
#coord_flip()

gender_plot <-
  ggplot(aes(as.factor(gender), n), data = gender_group) +
  geom_col(fill = 'SteelBlue') +
  xlab('Gender') +
  ylab('Count')
#coord_flip()

discharge_disposition_plot <-
  ggplot(aes(as.factor(discharge_disposition_consolidated), n), data = discharge_disposition_group) +
  geom_col(fill = 'SteelBlue') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('Discharge disposition') +
  ylab('Count')



plot_grid(age_plot, race_plot, gender_plot)
