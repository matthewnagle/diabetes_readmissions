library(broom)

#correlation
features <- c("early_readmission", "age_cat", list_of_diag1_cat)
ml_corr(diabetic_data, columns = features, method = "spearman")

features <- c("early_readmission", list_of_race_cat)
ml_corr(diabetic_data, columns = features, method = "spearman")

#OLS
ols_model = ml_linear_regression(diabetic_data, early_readmission ~ 
                                   diag_1_infection +
                                   diag_1_neoplasms,
                                  fit_intercept = FALSE)

ols_model = ml_linear_regression(diabetic_data, early_readmission ~ 
                                   diag_1_infection +
                                   diag_1_neoplasms +
                                   diag_1_endo_metabolic_immunity +
                                   diag_1_haematology +
                                   diag_1_mental +
                                   diag_1_neurology +
                                   diag_1_circulatory +
                                   diag_1_respiratory +
                                   diag_1_digestive +
                                   diag_1_genitourinary +
                                   diag_1_preg_birth_puerperium +
                                   diag_1_dermatology +
                                   diag_1_musculoskeletal +
                                   diag_1_congenital +
                                   diag_1_perinatal +
                                   diag_1_ill_defined +
                                   diag_1_injury_poisoning +
                                   diag_1_supplementary +
                                   diag_1_diabetes, fit_intercept = FALSE)

ols_results = tidy(ols_model) #? errors when using a lot of variables
ols_model$coefficients # gives coefficients without tidy
ols_results


#logistic regression - if there are NA it will error
lg_model = ml_logistic_regression(diabetic_data, early_readmission ~
                                    age_contin +
                                    Asian +
                                    AfricanAmerican +
                                    Caucasian +
                                    Hispanic +
                                    Unknown,
                                    #diag_1_infection
                                    fit_intercept = FALSE,
                                  family = "binomial")

lg_model = ml_logistic_regression(diabetic_data, early_readmission ~
                                   diag_1_infection +
                                   diag_1_neoplasms +
                                   diag_1_endo_metabolic_immunity +
                                   diag_1_haematology +
                                   diag_1_mental +
                                   diag_1_neurology +
                                   diag_1_circulatory +
                                   diag_1_respiratory +
                                   diag_1_digestive +
                                   diag_1_genitourinary +
                                   diag_1_preg_birth_puerperium +
                                   diag_1_dermatology +
                                   diag_1_musculoskeletal +
                                   diag_1_congenital +
                                   diag_1_perinatal +
                                   diag_1_ill_defined +
                                   diag_1_injury_poisoning +
                                   diag_1_supplementary +
                                   diag_1_diabetes, fit_intercept = FALSE,
                                  family = "binomial")

ml_generalized_linear_regression(data, formula, family = 'binomial')

lg_model = ml_logistic_regression(diabetic_data, early_readmission ~
                                    diag_1_infection +
                                    diag_1_neoplasms +
                                    diag_1_endo_metabolic_immunity +
                                    diag_1_haematology +
                                    diag_1_mental +
                                    diag_1_neurology +
                                    diag_1_circulatory +
                                    diag_1_respiratory +
                                    diag_1_digestive +
                                    diag_1_genitourinary +
                                    diag_1_preg_birth_puerperium +
                                    diag_1_dermatology +
                                    diag_1_musculoskeletal +
                                    diag_1_congenital +
                                    diag_1_perinatal +
                                    diag_1_ill_defined +
                                    diag_1_injury_poisoning +
                                    diag_1_supplementary +
                                    diag_1_diabetes +
                                    age_contin +
                                    Asian +
                                    AfricanAmerican +
                                    Caucasian +
                                    Hispanic +
                                    Unknown,
                                  fit_intercept = FALSE,
                                  family = "binomial")


summary(lg_model)
lg_results = tidy(lg_model)
lg_results

#evaluate
lg_model_metrics = ml_evaluate(lg_model, diabetic_data)
lg_model_metrics
lg_model_metrics$area_under_roc() 

lg_model_metrics$true_positive_rate_by_label()
lg_model_metrics$false_positive_rate_by_label()
lg_model_metrics$accuracy()
lg_model_metrics$precision_by_label()
lg_model_metrics$aic()
