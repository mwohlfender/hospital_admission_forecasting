
# anonymize tables of hospital data

# 1) ICD10 code U07.1 ----
table_days_icd_u07_1_20200101_20230630 <- read_csv(Path_Table_Days_ICD_U07_1_20200101_20230630)

table_days_icd_u07_1_20200101_20230630_anonymized <- table_days_icd_u07_1_20200101_20230630 %>%
  mutate(icd10_code_U07_1 = replace(icd10_code_U07_1, icd10_code_U07_1 < 5, NA)) %>%
  mutate(icd10_code_U07_1_group_age_1 = replace(icd10_code_U07_1_group_age_1, icd10_code_U07_1_group_age_1 < 5, NA)) %>%
  mutate(icd10_code_U07_1_group_age_2 = replace(icd10_code_U07_1_group_age_2, icd10_code_U07_1_group_age_2 < 5, NA)) %>%
  mutate(icd10_code_U07_1_group_age_3 = replace(icd10_code_U07_1_group_age_3, icd10_code_U07_1_group_age_3 < 5, NA)) %>%
  mutate(icd10_code_U07_1_group_age_4 = replace(icd10_code_U07_1_group_age_4, icd10_code_U07_1_group_age_4 < 5, NA)) %>%
  mutate(icd10_code_U07_1_group_age_5 = replace(icd10_code_U07_1_group_age_5, icd10_code_U07_1_group_age_5 < 5, NA))

write_csv(x = table_days_icd_u07_1_20200101_20230630_anonymized, 
          file = Path_Table_Days_ICD_U07_1_20200101_20230630_Anonymized)



# 2) visits at emergency department ----
table_days_visits_emergency_20200101_20230630 <- read_csv(Path_Table_Days_Visits_Emergency_20200101_20230630)

table_days_visits_emergency_20200101_20230630_anonymized <- table_days_visits_emergency_20200101_20230630 %>%
  mutate(visits = replace(visits, visits < 5, NA))

write_csv(x = table_days_visits_emergency_20200101_20230630_anonymized, 
          file = Path_Table_Days_Visits_Emergency_20200101_20230630_Anonymized)



# 3) patients with fever on day of admission ----
table_days_fever_20200101_20230630 <- read_csv(Path_Table_Days_Fever_20200101_20230630)

table_days_fever_20200101_20230630_anonymized <- table_days_fever_20200101_20230630 %>%
  mutate(fever_first_day = replace(fever_first_day, fever_first_day < 5, NA))

write_csv(x = table_days_fever_20200101_20230630_anonymized, 
          file = Path_Table_Days_Fever_20200101_20230630_Anonymized)



# 4) patients with fever on day of admission ----
table_days_number_patients_high_crp_20200101_20230630 <- read_csv(Path_Table_Days_Number_Patients_High_CRP_20200101_20230630)

table_days_number_patients_high_crp_20200101_20230630_anonymized <- table_days_number_patients_high_crp_20200101_20230630 %>%
  mutate(high_crp_first_day = replace(high_crp_first_day, high_crp_first_day < 5, NA))

write_csv(x = table_days_number_patients_high_crp_20200101_20230630_anonymized, 
          file = Path_Table_Days_Number_Patients_High_CRP_20200101_20230630_Anonymized)



# 5) ICD10 codes often appearing together with ICD10 code U07.1 ----
table_days_icd_freq_codes_cat_chap_covid_20200101_20230630 <- read_csv(Path_Table_Days_ICD_Freq_Codes_Cat_Chap_Covid_20200101_20230630)

table_days_icd_freq_codes_cat_chap_covid_20200101_20230630_anonymized <- table_days_icd_freq_codes_cat_chap_covid_20200101_20230630 %>%
  mutate(icd10_code_001_J12.8  = replace(icd10_code_001_J12.8 , icd10_code_001_J12.8 < 5, NA)) %>%
  mutate(icd10_code_002_I10.90 = replace(icd10_code_002_I10.90, icd10_code_002_I10.90 < 5, NA)) %>%
  mutate(icd10_code_003_J96.00 = replace(icd10_code_003_J96.00, icd10_code_003_J96.00 < 5, NA)) %>%
  mutate(icd10_code_004_Z22.8 = replace(icd10_code_004_Z22.8, icd10_code_004_Z22.8 < 5, NA)) %>%
  mutate(icd10_code_005_B33.8 = replace(icd10_code_005_B33.8, icd10_code_005_B33.8 < 5, NA)) %>%
  mutate(icd10_cat_001_E87  = replace(icd10_cat_001_E87 , icd10_cat_001_E87 < 5, NA)) %>%
  mutate(icd10_cat_002_J12 = replace(icd10_cat_002_J12, icd10_cat_002_J12 < 5, NA)) %>%
  mutate(icd10_cat_003_J96 = replace(icd10_cat_003_J96, icd10_cat_003_J96 < 5, NA)) %>%
  mutate(icd10_cat_004_I10 = replace(icd10_cat_004_I10, icd10_cat_004_I10 < 5, NA)) %>%
  mutate(icd10_cat_005_N18 = replace(icd10_cat_005_N18, icd10_cat_005_N18 < 5, NA)) %>%
  mutate(icd10_chap_001_R  = replace(icd10_chap_001_R , icd10_chap_001_R < 5, NA)) %>%
  mutate(icd10_chap_002_I = replace(icd10_chap_002_I, icd10_chap_002_I < 5, NA)) %>%
  mutate(icd10_chap_003_E = replace(icd10_chap_003_E, icd10_chap_003_E < 5, NA)) %>%
  mutate(icd10_chap_004_J = replace(icd10_chap_004_J, icd10_chap_004_J < 5, NA)) %>%
  mutate(icd10_chap_005_Z = replace(icd10_chap_005_Z, icd10_chap_005_Z < 5, NA))

write_csv(x = table_days_icd_freq_codes_cat_chap_covid_20200101_20230630_anonymized, 
          file = Path_Table_Days_ICD_Freq_Codes_Cat_Chap_Covid_20200101_20230630_Anonymized)


