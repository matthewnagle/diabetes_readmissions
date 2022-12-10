diabetic_data %>%
  group_by(metforminpioglitazone) %>%
  tally() %>%
  mutate(frac = n / sum(n))


diabetic_data %>%
  select(c(metformin:metforminpioglitazone)) %>%
  group_by_all() %>%
  count()

diabetic_data %>%
  select(c(metformin:metforminpioglitazone)) %>%
  summarise(
    across(
      .cols  = everything(),
      .fns   = ~ sum(str_detect(symptoms, "No")),
      .names = "{col}_mean"
    )
  )

mean(x,)
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