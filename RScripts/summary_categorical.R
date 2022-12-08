diabetic_data %>%
  group_by(metforminpioglitazone) %>%
  tally() %>%
  mutate(frac = n / sum(n))

metformin:metforminpioglitazone

meds <- c('metformin','metforminpioglitazone')

for (med in meds) {
diabetic_data %>%
    group_by(med) %>%
    tally() %>%
    mutate(frac = n / sum(n))
}

summary(diabetic_data)

kable_styling() from the kable_extra package: kable_styling(latex_options = "scale_down")