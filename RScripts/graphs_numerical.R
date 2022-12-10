#numerical variables
c('time_in_hospital',
'num_medications',
'number_inpatient',
'num_lab_procedures',
'number_outpatient',
'number_diagnoses',
'num_procedures',
'number_emergency')

#summary stats numerical variables
summary_stats_num_var <- sdf_describe(diabetic_data, cols = 
               c('time_in_hospital',
                 'num_medications',
                 'number_inpatient',
                 'num_lab_procedures',
                 'number_outpatient',
                 'number_diagnoses',
                 'num_procedures',
                 'number_emergency'))
kable(summary_stats_num_var) %>%
  kable_styling(latex_options = "scale_down")

#kable_styling() from the kable_extra package: kable_styling(latex_options = "scale_down")

#create summary groups to collect and use in ggplot
time_in_hospital_group = diabetic_data %>% 
  count(time_in_hospital) %>%
  arrange(time_in_hospital) %>%
  collect()

num_medications_group = diabetic_data %>% 
  count(num_medications) %>%
  arrange(num_medications) %>%
  collect()

number_inpatient_group = diabetic_data %>% 
  count(number_inpatient) %>%
  arrange(number_inpatient) %>%
  collect()

num_lab_procedures_group = diabetic_data %>% 
  count(num_lab_procedures) %>%
  arrange(num_lab_procedures) %>%
  collect()

number_outpatient_group = diabetic_data %>% 
  count(number_outpatient) %>%
  arrange(number_outpatient) %>%
  collect()

number_diagnoses_group = diabetic_data %>% 
  count(number_diagnoses) %>%
  arrange(number_diagnoses) %>%
  collect()

num_procedures_group = diabetic_data %>% 
  count(num_procedures) %>%
  arrange(num_procedures) %>%
  collect()

number_emergency_group = diabetic_data %>% 
  count(number_emergency) %>%
  arrange(number_emergency) %>%
  collect()

#create plots
time_in_hospital_plot <- 
  ggplot(aes(as.numeric(time_in_hospital), n), data = time_in_hospital_group) +
  geom_col(fill = 'SteelBlue') +
  scale_x_continuous(breaks=seq(0, 20, 2))  +
  xlab('Time in hospital (days)') +
  ylab('Count')

num_medications_plot <- 
  ggplot(aes(as.numeric(num_medications), n), data = num_medications_group) +
  geom_col(fill = 'SteelBlue') +
  scale_x_continuous(breaks=seq(0, 80, 10))  +
  xlab('Number of medications') +
  ylab('Count')

number_inpatient_plot <- 
  ggplot(aes(as.numeric(number_inpatient), n), data = number_inpatient_group) +
  geom_col(fill = 'SteelBlue') +
  #scale_x_continuous(breaks=seq(0, 15, 5))  +
  xlab('Number of inpatient visits \n(within preceding year)') +
  ylab('Count')

number_outpatient_plot <- 
  ggplot(aes(as.numeric(number_outpatient), n), data = number_outpatient_group) +
  geom_col(fill = 'SteelBlue') +
  scale_x_continuous(breaks=seq(0, 40, 5))  +
  xlab('Number of outpatient visits \n(within preceding year)') +
  ylab('Count')

number_emergency_plot <- 
  ggplot(aes(as.numeric(number_emergency), n), data = number_emergency_group) +
  geom_col(fill = 'SteelBlue') +
  xlab('Number of emergency visits \n(within preceding year)') +
  ylab('Count')

num_lab_procedures_plot <- 
  ggplot(aes(as.numeric(num_lab_procedures), n), data = num_lab_procedures_group) +
  geom_col(fill = 'SteelBlue') +
  xlab('Number of lab tests performed') +
  ylab('Count')

num_procedures_plot <- 
  ggplot(aes(as.numeric(num_procedures), n), data = num_procedures_group) +
  geom_col(fill = 'SteelBlue') +
  xlab('Number of procedures performed') +
  ylab('Count')

number_diagnoses_plot <- 
  ggplot(aes(as.numeric(number_diagnoses), n), data = number_diagnoses_group) +
  geom_col(fill = 'SteelBlue') +
  xlab('Number of diagnoses') +
  ylab('Count')

plot_grid(time_in_hospital_plot,
          num_medications_plot,
          number_inpatient_plot,
          number_outpatient_plot,
          number_emergency_plot,
          num_lab_procedures_plot,
          num_procedures_plot,
          number_diagnoses_plot)

