
# feature engineering 004
# create data summary table: number of patients with maximal crp concentration measurement >= 50 mg/l on admission day
# one row: one day between 1 January 2020 and 30 June 2023 and number of patients
# method: for each day between 01 January 2020 and 30 June 2023
# determine maximal crp concentration measurement for each patient admitted on that day

if (!(file.exists(Path_Table_Days_Number_Patients_High_CRP_20200101_20230630)) | Bool_Data_Processing_Do_New) {
  
  # read processed hospital data: lab (crp)
  data_hospital_lab_crp <- read_csv(file = Path_Data_Hospital_Lab_CRP_Processed)
  
  # summarize `data_hospital_lab_crp`:
  # determine highest body temperature for each patient on each day
  data_hospital_patient_day_max_crp <- data_hospital_lab_crp %>%
    group_by(id_patient, date_measured_ymd_date) %>%
    summarize(max_crp = max(value_measured)) %>%
    ungroup()
  
  # remove `data_hospital_lab_crp` from environment
  remove(data_hospital_lab_crp)
  
  # read processed hospital data: main
  data_hospital_main <- read_csv(Path_Data_Hospital_Main_Processed)
  
  # join `data_hospital_main` and `data_hospital_patient_day_max_crp`
  # determine for each patient the maximal crp concentration on all days on which a hospital stay started
  data_hospital_patient_first_day_max_crp <- data_hospital_main %>%
    dplyr::rename(c("date_day" = "date_case_start_ymd_date")) %>%
    dplyr::select(c("id_patient", "date_day")) %>%
    group_by(id_patient, date_day) %>%
    summarize() %>%
    ungroup() %>%
    left_join(data_hospital_patient_day_max_crp %>%
                dplyr::rename(c("date_day" = "date_measured_ymd_date")), by = c("id_patient", "date_day")) %>%
    mutate(max_crp = replace_na(max_crp, 0))
  
  # remove `data_hospital_main` from environment
  remove(data_hospital_main, data_hospital_patient_day_max_crp)
  
  # summarize `data_hospital_patient_first_day_max_crp`
  # determine for each day between January 1, 2020 and June 30, 2023 how many patients were admitted
  # who measured a crp concentration of at least 50 mg/l on that day
  table_patients_high_crp_first_day <- data_hospital_patient_first_day_max_crp %>%
    group_by(date_day) %>%
    summarize(high_crp_first_day = sum(max_crp >= 50)) %>%
    ungroup() %>%
    right_join(tibble(date_day = seq(from = ymd("2020-01-01"), to = ymd("2023-06-30"), by = "days")), by = "date_day") %>%
    mutate(high_crp_first_day = replace_na(high_crp_first_day, 0))
  
  # store `table_patients_high_crp_first_day`
  write_csv(x = table_patients_high_crp_first_day,
            file = Path_Table_Days_Number_Patients_High_CRP_20200101_20230630)
  
  # remove variables from environment
  remove(data_hospital_patient_first_day_max_crp, table_patients_high_crp_first_day)
  
}


