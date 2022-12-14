mlp_model = ml_multilayer_perceptron_classifier(
  cars,
  gear_relabelled ~ z_hp + cyl_4 + cyl_6 + cyl_8,
  layers = c(4, 8, 8, 3)
)
mlp_predictions = ml_predict(mlp_model, diabetic_data_test)
head(predictions)

layers =c(163, 80, 80, 2)

head(predictions)
ml_evaluate(mlp_model, diabetic_data_test)
mlp_auc <- ml_binary_classification_evaluator(mlp_predictions, label_col = "early_readmission")

#geradient boosted trees
#ran without error 
mlp_model = ml_multilayer_perceptron_classifier(diabetic_data, early_readmission ~
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
                                        layers =c(163, 80, 80, 2))
