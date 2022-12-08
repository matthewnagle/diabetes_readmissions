list_of_medical_specialty <- diabetic_data %>%
  group_by(medical_specialty) %>%
  tally() %>%
  mutate(frac = n / sum(n)) %>%
  collect()

glimpse(list_of_medical_specialty)

kable(list_of_medical_specialty)

#one_hot_encode diagnosis 1
diabetic_data <- diabetic_data %>%
  mutate(
    Cardiology = ifelse(medical_specialty == 'Cardiology', 1,0),
    ObstetricsandGynecology = ifelse(medical_specialty == 'ObstetricsandGynecology', 1,0),
    Pediatrics = ifelse(medical_specialty == 'Pediatrics', 1,0),
    SurgeryColonRectal = ifelse(medical_specialty == 'Surgery-Colon&Rectal', 1,0),
    PediatricsCriticalCare = ifelse(medical_specialty == 'Pediatrics-CriticalCare', 1,0),
    Anesthesiology_Pediatric = ifelse(medical_specialty == 'Anesthesiology-Pediatric', 1,0),
    Ophthalmology = ifelse(medical_specialty == 'Ophthalmology', 1,0),
    InfectiousDiseases = ifelse(medical_specialty == 'InfectiousDiseases', 1,0),
    SurgeryMaxillofacial = ifelse(medical_specialty == 'Surgery-Maxillofacial', 1,0),
    PsychiatryAddictive = ifelse(medical_specialty == 'Psychiatry-Addictive', 1,0),
    SurgeryCardiovascular = ifelse(medical_specialty == 'Surgery-Cardiovascular', 1,0),
    Speech = ifelse(medical_specialty == 'Speech', 1,0),
    Endocrinology_Metabolism = ifelse(medical_specialty == 'Endocrinology-Metabolism', 1,0),
    FamilyGeneralPractice = ifelse(medical_specialty == 'Family/GeneralPractice', 1,0),
    SurgeryGeneral = ifelse(medical_specialty == 'Surgery-General', 1,0),
    Orthopedics = ifelse(medical_specialty == 'Orthopedics', 1,0),
    EmergencyTrauma = ifelse(medical_specialty == 'Emergency/Trauma', 1,0),
    HematologyOncology = ifelse(medical_specialty == 'Hematology/Oncology', 1,0),
    Otolaryngology = ifelse(medical_specialty == 'Otolaryngology', 1,0),
    Oncology = ifelse(medical_specialty == 'Oncology', 1,0),
    SurgeryPediatric = ifelse(medical_specialty == 'Surgery-Pediatric', 1,0),
    PediatricsEmergencyMedicine = ifelse(medical_specialty == 'Pediatrics-EmergencyMedicine', 1,0),
    AllergyandImmunology = ifelse(medical_specialty == 'AllergyandImmunology', 1,0),
    PediatricsInfectiousDiseases = ifelse(medical_specialty == 'Pediatrics-InfectiousDiseases', 1,0),
    Osteopath = ifelse(medical_specialty == 'Osteopath', 1,0),
    SurgicalSpecialty = ifelse(medical_specialty == 'SurgicalSpecialty', 1,0),
    Dermatology = ifelse(medical_specialty == 'Dermatology', 1,0),
    SportsMedicine = ifelse(medical_specialty == 'SportsMedicine', 1,0),
    Resident = ifelse(medical_specialty == 'Resident', 1,0),
    InternalMedicine = ifelse(medical_specialty == 'InternalMedicine', 1,0),
    Gastroenterology = ifelse(medical_specialty == 'Gastroenterology', 1,0),
    SurgeryCardiovascularThoracic = ifelse(medical_specialty == 'Surgery-Cardiovascular/Thoracic', 1,0),
    Nephrology = ifelse(medical_specialty == 'Nephrology', 1,0),
    OrthopedicsReconstructive = ifelse(medical_specialty == 'Orthopedics-Reconstructive', 1,0),
    ObstericsGynecologyGynecologicOnco = ifelse(medical_specialty == 'Obsterics&Gynecology-GynecologicOnco', 1,0),
    Endocrinology = ifelse(medical_specialty == 'Endocrinology', 1,0),
    Pediatrics_Pulmonology = ifelse(medical_specialty == 'Pediatrics-Pulmonology', 1,0),
    Neurology = ifelse(medical_specialty == 'Neurology', 1,0),
    Psychology = ifelse(medical_specialty == 'Psychology', 1,0),
    Podiatry = ifelse(medical_specialty == 'Podiatry', 1,0),
    Gynecology = ifelse(medical_specialty == 'Gynecology', 1,0),
    SurgeryPlastic = ifelse(medical_specialty == 'Surgery-Plastic', 1,0),
    SurgeryThoracic = ifelse(medical_specialty == 'Surgery-Thoracic', 1,0),
    SurgeryPlasticwithinHeadandNeck = ifelse(medical_specialty == 'Surgery-PlasticwithinHeadandNeck', 1,0),
    PhysicalMedicineandRehabilitation = ifelse(medical_specialty == 'PhysicalMedicineandRehabilitation', 1,0),
    Rheumatology = ifelse(medical_specialty == 'Rheumatology', 1,0),
    PediatricsAllergyandImmunology = ifelse(medical_specialty == 'Pediatrics-AllergyandImmunology', 1,0),
    Surgeon = ifelse(medical_specialty == 'Surgeon', 1,0),
    SurgeryVascular = ifelse(medical_specialty == 'Surgery-Vascular', 1,0),
    Pathology = ifelse(medical_specialty == 'Pathology', 1,0),
    Hospitalist = ifelse(medical_specialty == 'Hospitalist', 1,0),
    OutreachServices = ifelse(medical_specialty == 'OutreachServices', 1,0),
    CardiologyPediatric = ifelse(medical_specialty == 'Cardiology-Pediatric', 1,0),
    Neurophysiology = ifelse(medical_specialty == 'Neurophysiology', 1,0),
    PediatricsEndocrinology = ifelse(medical_specialty == 'Pediatrics-Endocrinology', 1,0),
    Psychiatry = ifelse(medical_specialty == 'Psychiatry', 1,0),
    Pulmonology = ifelse(medical_specialty == 'Pulmonology', 1,0),
    SurgeryNeuro = ifelse(medical_specialty == 'Surgery-Neuro', 1,0),
    Urology = ifelse(medical_specialty == 'Urology', 1,0),
    PsychiatryChildAdolescent = ifelse(medical_specialty == 'Psychiatry-Child/Adolescent', 1,0),
    Radiology = ifelse(medical_specialty == 'Radiology', 1,0),
    PediatricsHematologyOncology = ifelse(medical_specialty == 'Pediatrics-Hematology-Oncology', 1,0),
    PediatricsNeurology = ifelse(medical_specialty == 'Pediatrics-Neurology', 1,0),
    Anesthesiology = ifelse(medical_specialty == 'Anesthesiology', 1,0),
    Dentistry = ifelse(medical_specialty == 'Dentistry', 1,0),
    PhysicianNotFound = ifelse(medical_specialty == 'PhysicianNotFound', 1,0),
    Hematology = ifelse(medical_specialty == 'Hematology', 1,0),
    Proctology = ifelse(medical_specialty == 'Proctology', 1,0),
    Obstetrics = ifelse(medical_specialty == 'Obstetrics', 1,0),
    Radiologist = ifelse(medical_specialty == 'Radiologist', 1,0),
    Perinatology = ifelse(medical_specialty == 'Perinatology', 1,0),
    DCPTEAM = ifelse(medical_specialty == 'DCPTEAM', 1,0),
    medical_specialty_unkown = ifelse(medical_specialty == '?', 1,0))


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
medical_specialty_unkown +
    
