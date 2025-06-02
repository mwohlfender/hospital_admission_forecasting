
# feature engineering 003
# create data summary table: number of patients with maximal body temperature measurement >= 38.5 degrees Celsius on admission day
# one row: one day between 1 January 2020 and 30 June 2023 and number of patients
# method: for each day between 01 January 2020 and 30 June 2023
# determine maximal body temperature measurement for each patient admitted on that day

if (!(file.exists(Path_Table_Days_Fever_20200101_20230630)) | Bool_Data_Processing_Do_New) {
  
  # read processed hospital data: vdat (body temperature)
  data_hospital_vdat_body_temperature <- read_csv(file = Path_Data_Hospital_VDat_Body_Temperature_Processed)
  
  # summarize `data_hospital_vdat_body_temperature`:
  # determine highest body temperature for each patient on each day
  data_hospital_patient_day_max_body_temperature <- data_hospital_vdat_body_temperature %>%
    group_by(id_patient, date_measured_ymd_date) %>%
    summarize(max_body_temp = max(value_measured)) %>%
    ungroup()
  
  # remove `data_hospital_vdat_body_temperature` from environment
  remove(data_hospital_vdat_body_temperature)
  
  # read processed hospital data: main
  data_hospital_main <- read_csv(Path_Data_Hospital_Main_Processed)
  
  # join `data_hospital_main` and `data_hospital_patient_day_max_body_temperature`
  # determine for each patient the maximal body temperature on all days on which a hospital stay started
  data_hospital_patient_first_day_max_body_temp <- data_hospital_main %>%
    dplyr::rename(c("date_day" = "date_case_start_ymd_date")) %>%
    dplyr::select(c("id_patient", "date_day")) %>%
    group_by(id_patient, date_day) %>%
    summarize() %>%
    ungroup() %>%
    left_join(data_hospital_patient_day_max_body_temperature %>%
                dplyr::rename(c("date_day" = "date_measured_ymd_date")), by = c("id_patient", "date_day")) %>%
    mutate(max_body_temp = replace_na(max_body_temp, 0))
  
  # remove `data_hospital_main` from environment
  remove(data_hospital_main, data_hospital_patient_day_max_body_temperature)
  
  # summarize `data_hospital_patient_first_day_max_body_temp`
  # determine for each day between January 1, 2020 and June 30, 2023 how many patients were admitted
  # who measured a body temperature of at least 38.5 degrees Celsius on that day
  table_patients_fever_first_day <- data_hospital_patient_first_day_max_body_temp %>%
    group_by(date_day) %>%
    summarize(fever_first_day = sum(max_body_temp >= 38.5)) %>%
    ungroup() %>%
    right_join(tibble(date_day = seq(from = ymd("2020-01-01"), to = ymd("2023-06-30"), by = "days")), by = "date_day") %>%
    mutate(fever_first_day = replace_na(fever_first_day, 0))
  
  # store results
  write_csv(x = table_patients_fever_first_day,
            file = Path_Table_Days_Fever_20200101_20230630)
  
  # remove variables from environment
  remove(data_hospital_patient_first_day_max_body_temp, table_patients_fever_first_day)
  
}


