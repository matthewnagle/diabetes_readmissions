##PLOTS

dbplot_histogram(diabetic_data, age_contin)


# Data manipulations are done first using spark
age_group = diabetic_data %>% 
  count(age) %>%
  arrange(age) %>%
  collect()

race_group = diabetic_data %>% 
  count(race) %>%
  arrange(race) %>%
  collect()



# Now use ggplot on the R dataframe age_group
age_plot <- 
  ggplot(aes(as.factor(age), n), data = age_group) +
  geom_col(fill = 'SteelBlue') +
  xlab('Count') +
  ylab('Age group')
#coord_flip()

race_plot <-
  ggplot(aes(as.factor(race), n), data = race_group) +
  geom_col(fill = 'SteelBlue') +
  xlab('Count') +
  ylab('Age group')
#coord_flip()

library(cowplot)

plot_grid(age_plot, race_plot)
