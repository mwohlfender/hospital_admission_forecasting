
# feature engineering 002
# create data summary table: visits at emergency department
# one row: one day between 1 January 2020 and 30 June 2023 and number of visits at emergency department
# method: for each day between 01 January 2020 and 30 June 2023 count number of case ids appearing in
# `data_hospital_mov_processed` with clinic equal to "Universitätsklinik für Notfallmedizin"

if (!(file.exists(Path_Table_Days_Visits_Emergency_20200101_20230630)) | Bool_Data_Processing_Do_New) {
  
  # read processed hospital data: movements and filter to 
  data_hospital_mov_emergency <- read_csv(file = Path_Data_Hospital_Mov_Processed) %>% 
    filter(clinic == "Universitätsklinik für Notfallmedizin") %>%
    filter(date_mov_in_ymd_date >= ymd("2020-01-01")) %>%
    filter(date_mov_in_ymd_date <= ymd("2023-06-30"))
  
  # summarize to days and cases
  data_hospital_mov_emergency_days_cases <- data_hospital_mov_emergency %>%
    group_by(date_day = date_mov_in_ymd_date, id_case) %>%
    summarize(visits = n()) %>%
    ungroup()
  
  # remove `data_hospital_mov_emergency` from environment
  remove(data_hospital_mov_emergency)
  
  # summarize to days
  data_hospital_mov_emergency_days <- data_hospital_mov_emergency_days_cases %>%
    group_by(date_day) %>%
    summarize(visits = n()) %>%
    ungroup()
  
  # remove `data_hospital_mov_emergency_days_cases` from environment
  remove(data_hospital_mov_emergency_days_cases)
  
  # store results
  write_csv(x = data_hospital_mov_emergency_days,
            file = Path_Table_Days_Visits_Emergency_20200101_20230630)
  
  # remove `data_hospital_mov_emergency_days` from environment
  remove(data_hospital_mov_emergency_days)
  
}


