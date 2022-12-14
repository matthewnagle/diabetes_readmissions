library(broom)



#correlation
features <- c("early_readmission", "age_cat", list_of_diag1_cat)
ml_corr(diabetic_data, columns = features, method = "spearman")

#Chi squared
features <- c("FAF", "FCVC", "TUE", "CH2O") 
ml_chisquare_test(obesity_data, features = features, label = "early_readmission")


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


#logistic regression - if there are NAs it will error
#ran without error on 6/12/22 at 21:40
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
                                    diag_2_infection +
                                    diag_2_neoplasms +
                                    diag_2_endo_metabolic_immunity +
                                    diag_2_haematology +
                                    diag_2_mental +
                                    diag_2_neurology +
                                    diag_2_circulatory +
                                    diag_2_respiratory +
                                    diag_2_digestive +
                                    diag_2_genitourinary +
                                    diag_2_preg_birth_puerperium +
                                    diag_2_dermatology +
                                    diag_2_musculoskeletal +
                                    diag_2_congenital +
                                    diag_2_perinatal +
                                    diag_2_ill_defined +
                                    diag_2_injury_poisoning +
                                    diag_2_supplementary +
                                    diag_2_diabetes +
                                    diag_3_infection +
                                    diag_3_neoplasms +
                                    diag_3_endo_metabolic_immunity +
                                    diag_3_haematology +
                                    diag_3_mental +
                                    diag_3_neurology +
                                    diag_3_circulatory +
                                    diag_3_respiratory +
                                    diag_3_digestive +
                                    diag_3_genitourinary +
                                    diag_3_preg_birth_puerperium +
                                    diag_3_dermatology +
                                    diag_3_musculoskeletal +
                                    diag_3_congenital +
                                    diag_3_perinatal +
                                    diag_3_ill_defined +
                                    diag_3_injury_poisoning +
                                    diag_3_supplementary +
                                    diag_3_diabetes +
                                    home +
                                    healthcare_facility +
                                    home_with_help +
                                    AMA +
                                    admitted +
                                    hospice_expired +
                                    outpatient +
                                    unknown_discharge_disposition +
                                    #age_contin +
                                    asian +
                                    african_american +
                                    caucasian +
                                    hispanic +
                                    unknown_race +
                                    emergency +
                                    urgent +
                                    elective +
                                    admisison_type_other +
                                    max_glu_serum_none +
                                    max_glu_serum_norm +
                                    max_glu_serum_300 +
                                    max_glu_serum_200 +
                                    A1Cresult_none +
                                    A1Cresult_norm +
                                    A1Cresult_7 +
                                    A1Cresult_8 +
                                    z_age_contin +
                                    z_time_in_hospital +
                                    z_num_lab_procedures +
                                    z_num_procedures +
                                    z_number_outpatient +
                                    z_number_emergency +
                                    z_number_inpatient +
                                    z_number_diagnoses +
                                    Cardiology +
                                    ObstetricsandGynecology +
                                    Pediatrics +
                                    SurgeryColonRectal +
                                    PediatricsCriticalCare +
                                    Anesthesiology_Pediatric +
                                    Ophthalmology +
                                    InfectiousDiseases +
                                    SurgeryMaxillofacial +
                                    PsychiatryAddictive +
                                    SurgeryCardiovascular +
                                    Speech +
                                    Endocrinology_Metabolism +
                                    FamilyGeneralPractice +
                                    SurgeryGeneral +
                                    Orthopedics +
                                    EmergencyTrauma +
                                    HematologyOncology +
                                    Otolaryngology +
                                    Oncology +
                                    SurgeryPediatric +
                                    PediatricsEmergencyMedicine +
                                    AllergyandImmunology +
                                    PediatricsInfectiousDiseases +
                                    Osteopath +
                                    SurgicalSpecialty +
                                    Dermatology +
                                    SportsMedicine +
                                    Resident +
                                    InternalMedicine +
                                    Gastroenterology +
                                    SurgeryCardiovascularThoracic +
                                    Nephrology +
                                    OrthopedicsReconstructive +
                                    ObstericsGynecologyGynecologicOnco +
                                    Endocrinology +
                                    Pediatrics_Pulmonology +
                                    Neurology +
                                    Psychology +
                                    Podiatry +
                                    Gynecology +
                                    SurgeryPlastic +
                                    SurgeryThoracic +
                                    SurgeryPlasticwithinHeadandNeck +
                                    PhysicalMedicineandRehabilitation +
                                    Rheumatology +
                                    PediatricsAllergyandImmunology +
                                    Surgeon +
                                    SurgeryVascular +
                                    Pathology +
                                    Hospitalist +
                                    OutreachServices +
                                    CardiologyPediatric +
                                    Neurophysiology +
                                    PediatricsEndocrinology +
                                    Psychiatry +
                                    Pulmonology +
                                    SurgeryNeuro +
                                    Urology +
                                    PsychiatryChildAdolescent +
                                    Radiology +
                                    PediatricsHematologyOncology +
                                    PediatricsNeurology +
                                    Anesthesiology +
                                    Dentistry +
                                    PhysicianNotFound +
                                    Hematology +
                                    Proctology +
                                    Obstetrics +
                                    Radiologist +
                                    Perinatology +
                                    DCPTEAM +
                                    medical_specialty_unkown,
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
lg_model_metrics$roc()

#geradient boosted trees
#ran without error 
gbt_model = ml_gradient_boosted_trees(diabetic_data, early_readmission ~
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
                                        diag_2_unknown +
                                        diag_2_infection +
                                        diag_2_neoplasms +
                                        diag_2_endo_metabolic_immunity +
                                        diag_2_haematology +
                                        diag_2_mental +
                                        diag_2_neurology +
                                        diag_2_circulatory +
                                        diag_2_respiratory +
                                        diag_2_digestive +
                                        diag_2_genitourinary +
                                        diag_2_preg_birth_puerperium +
                                        diag_2_dermatology +
                                        diag_2_musculoskeletal +
                                        diag_2_congenital +
                                        diag_2_perinatal +
                                        diag_2_ill_defined +
                                        diag_2_injury_poisoning +
                                        diag_2_supplementary +
                                        diag_2_diabetes +
                                        diag_2_unknown +
                                        diag_3_infection +
                                        diag_3_neoplasms +
                                        diag_3_endo_metabolic_immunity +
                                        diag_3_haematology +
                                        diag_3_mental +
                                        diag_3_neurology +
                                        diag_3_circulatory +
                                        diag_3_respiratory +
                                        diag_3_digestive +
                                        diag_3_genitourinary +
                                        diag_3_preg_birth_puerperium +
                                        diag_3_dermatology +
                                        diag_3_musculoskeletal +
                                        diag_3_congenital +
                                        diag_3_perinatal +
                                        diag_3_ill_defined +
                                        diag_3_injury_poisoning +
                                        diag_3_supplementary +
                                        diag_3_diabetes +
                                        diag_3_unknown +
                                        home +
                                        healthcare_facility +
                                        home_with_help +
                                        AMA +
                                        admitted +
                                        hospice_expired +
                                        outpatient +
                                        unknown_discharge_disposition +
                                        #age_contin +
                                        asian +
                                        african_american +
                                        caucasian +
                                        hispanic +
                                        unknown_race +
                                        emergency +
                                        urgent +
                                        elective +
                                        admisison_type_other +
                                        max_glu_serum_none +
                                        max_glu_serum_norm +
                                        max_glu_serum_300 +
                                        max_glu_serum_200 +
                                        A1Cresult_none +
                                        A1Cresult_norm +
                                        A1Cresult_7 +
                                        A1Cresult_8 +
                                        z_age_contin +
                                        z_time_in_hospital +
                                        z_num_lab_procedures +
                                        z_num_procedures +
                                        z_number_outpatient +
                                        z_number_emergency +
                                        z_number_inpatient +
                                        z_number_diagnoses +
                                        Cardiology +
                                        ObstetricsandGynecology +
                                        Pediatrics +
                                        SurgeryColonRectal +
                                        PediatricsCriticalCare +
                                        Anesthesiology_Pediatric +
                                        Ophthalmology +
                                        InfectiousDiseases +
                                        SurgeryMaxillofacial +
                                        PsychiatryAddictive +
                                        SurgeryCardiovascular +
                                        Speech +
                                        Endocrinology_Metabolism +
                                        FamilyGeneralPractice +
                                        SurgeryGeneral +
                                        Orthopedics +
                                        EmergencyTrauma +
                                        HematologyOncology +
                                        Otolaryngology +
                                        Oncology +
                                        SurgeryPediatric +
                                        PediatricsEmergencyMedicine +
                                        AllergyandImmunology +
                                        PediatricsInfectiousDiseases +
                                        Osteopath +
                                        SurgicalSpecialty +
                                        Dermatology +
                                        SportsMedicine +
                                        Resident +
                                        InternalMedicine +
                                        Gastroenterology +
                                        SurgeryCardiovascularThoracic +
                                        Nephrology +
                                        OrthopedicsReconstructive +
                                        ObstericsGynecologyGynecologicOnco +
                                        Endocrinology +
                                        Pediatrics_Pulmonology +
                                        Neurology +
                                        Psychology +
                                        Podiatry +
                                        Gynecology +
                                        SurgeryPlastic +
                                        SurgeryThoracic +
                                        SurgeryPlasticwithinHeadandNeck +
                                        PhysicalMedicineandRehabilitation +
                                        Rheumatology +
                                        PediatricsAllergyandImmunology +
                                        Surgeon +
                                        SurgeryVascular +
                                        Pathology +
                                        Hospitalist +
                                        OutreachServices +
                                        CardiologyPediatric +
                                        Neurophysiology +
                                        PediatricsEndocrinology +
                                        Psychiatry +
                                        Pulmonology +
                                        SurgeryNeuro +
                                        Urology +
                                        PsychiatryChildAdolescent +
                                        Radiology +
                                        PediatricsHematologyOncology +
                                        PediatricsNeurology +
                                        Anesthesiology +
                                        Dentistry +
                                        PhysicianNotFound +
                                        Hematology +
                                        Proctology +
                                        Obstetrics +
                                        Radiologist +
                                        Perinatology +
                                        DCPTEAM +
                                        medical_specialty_unkown,
                                    type = "classification")

predictions = ml_predict(gbt_model, diabetic_data)
ml_binary_classification_evaluator(predictions, label_col = "early_readmission")

#GBTClassifier currently only supports binary classification

featureImport <- ml_tree_feature_importance(gbt_model)
featureImport[1:10,] %>% ggplot(aes(reorder(feature, importance),importance,fill=feature)) + 
  geom_bar(stat = "identity") + coord_flip() + ggtitle("Top 10 feature importance") + 
  theme(legend.position="none") + ylab("importance") + xlab("feature")
