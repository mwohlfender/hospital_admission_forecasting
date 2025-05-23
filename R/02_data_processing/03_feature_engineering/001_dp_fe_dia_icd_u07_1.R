
# feature engineering 001
# create data summary table: assignment of ICD10 code U07.1
# one row: one day between 1 January 2020 and 30 June 2023 and number of patients
# method: for each day between 01 January 2020 and 30 June 2023 count number of case ids to which ICD10 code U07.1 has been assigned

if (!(file.exists(Path_Table_Days_ICD_U07_1_20200101_20230630)) | Bool_Data_Processing_Do_New) {

  # read processed hospital data: diagnoses
  data_hospital_dia <- read_csv(file = Path_Data_Hospital_Dia_Processed)
  
  sequence_days <- seq(from = ymd("2020-01-01"), to = ymd("2023-06-30"), by = "days")
  
  # determine which cases have been assigned an icd10 code U07.1 on which day
  # take into account that the icd10 code U07.1 might have been assigned twice to the same case on the same day
  data_hospital_dia_case_day_U07_1 <- data_hospital_dia %>%
    filter(icd10 == "U07.1") %>%
    group_by(id_case, date_day = date_dia_ymd_date) %>%
    summarize(icd10_code_U07_1 = 0 + (sum(icd10 == "U07.1") > 0)) %>%
    ungroup()
  
  # remove `data_hospital_dia` from environment
  remove(data_hospital_dia)
  
  # read processed hospital data: main
  data_hospital_main <- read_csv(file = Path_Data_Hospital_Main_Processed)
  
  # add age group to `data_hospital_dia_case_day_U07_1`
  data_hospital_dia_case_day_U07_1_age <- data_hospital_dia_case_day_U07_1 %>%
    left_join(data_hospital_main %>% dplyr::select(c("id_case", "group_age_sent")), by = "id_case")
  
  # remove `data_hospital_main` from environment
  remove(data_hospital_main)
  
  # data_hospital_dia_case_day_U07_1_age`: stratify column `icd10_code_U07_1` by age group
  data_hospital_dia_case_day_U07_1_agegroups <- data_hospital_dia_case_day_U07_1_age %>%
    mutate(icd10_code_U07_1_group_age = icd10_code_U07_1) %>%
    arrange(group_age_sent) %>%
    pivot_wider(names_from = group_age_sent, values_from = icd10_code_U07_1_group_age, names_prefix = "icd10_code_U07_1_group_age_") %>%
    mutate_if(is.numeric , replace_na, replace = 0)
  
  # summarize `data_hospital_dia_case_day_U07_1_agegroups` to daily level
  data_hospital_dia_U07_1_days <- data_hospital_dia_case_day_U07_1_agegroups %>%
    group_by(date_day) %>%
    summarize(icd10_code_U07_1 = sum(icd10_code_U07_1),
              icd10_code_U07_1_group_age_1 = sum(icd10_code_U07_1_group_age_1),
              icd10_code_U07_1_group_age_2 = sum(icd10_code_U07_1_group_age_2),
              icd10_code_U07_1_group_age_3 = sum(icd10_code_U07_1_group_age_3),
              icd10_code_U07_1_group_age_4 = sum(icd10_code_U07_1_group_age_4),
              icd10_code_U07_1_group_age_5 = sum(icd10_code_U07_1_group_age_5)) %>%
    ungroup()
  
  # remove `data_hospital_dia_case_day_U07_1` from environment
  remove(data_hospital_dia_case_day_U07_1)
  
  
  data_hospital_dia_U07_1_days_20200101_20230630 <- tibble(date_day = sequence_days) %>%
    left_join(data_hospital_dia_U07_1_days, by = "date_day") %>%
    mutate_if(is.numeric , replace_na, replace = 0)
  
  # store results
  write_csv(x = data_hospital_dia_U07_1_days_20200101_20230630,
            file = Path_Table_Days_ICD_U07_1_20200101_20230630)
  
  # remove `data_hospital_dia_U07_1_days` and `data_hospital_dia_U07_1_days_20200101_20230630` from environment
  remove(data_hospital_dia_U07_1_days, data_hospital_dia_U07_1_days_20200101_20230630)
  
}


