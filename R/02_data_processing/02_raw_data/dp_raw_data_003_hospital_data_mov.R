

# process hospital data
# data source: IDSC
# different checks and changes are carried out on raw electronic health record data,
# e.g., standardization of date formats and checks for unrealistic values

if (!(file.exists(Path_Data_Hospital_Mov_Processed)) | Bool_Data_Processing_Do_New) {
  
  # read raw hospital data: movements
  data_hospital_mov_raw <- read_csv(file = Path_Data_Hospital_Mov_Raw)
  
  # `data_hospital_mov_raw`:
  # (a) add column `date_mov_in_yw_num`: column `movement_date` in format YYYYWW (year/week) (numeric)
  # (b) add column `date_mov_in_ymd_date`: column `movement_date` in format YYYY-MM-DD (year/month/day) (date)
  # (c) add column `date_mov_in_ywwd_num`: column `movement_date` in format YYYYWWDD (year/week/day of week) (numeric)
  # (d) remove column `movement_date`
  data_hospital_mov_0 <- data_hospital_mov_raw %>% 
    mutate(date_mov_in_yw_num = 100 * lubridate::isoyear(movement_date) + 
             lubridate::isoweek(movement_date),
           date_mov_in_ymd_date = lubridate::ymd(substr(movement_date, 1, 10)),
           date_mov_in_ywwd_num = 10000 * lubridate::isoyear(movement_date) + 
             100 * lubridate::isoweek(movement_date) + 
             lubridate::wday(movement_date,  week_start = getOption("lubridate.week.start", 1))) %>%
    dplyr::select(-c("movement_date"))
  
  # remove `data_hospital_mov_raw` from the environment
  remove(data_hospital_mov_raw)
  
  # `data_hospital_mov_0`:
  # (a) add column `date_mov_out_yw_num`: column `transfer_date_next` in format YYYYWW (year/week) (numeric)
  # (b) add column `date_mov_out_ymd_date`: column `transfer_date_next` in format YYYY-MM-DD (year/month/day) (date)
  # (c) add column `date_mov_out_ywwd_num`: column `transfer_date_next` in format YYYYWWDD (year/week/day of week) (numeric)
  # (d) remove column `transfer_date_next`
  # remark: "NULL" values in column `transfer_date_next` are replaced by
  # (1) 190001 (numeric) in column `date_mov_out_yw_num`
  # (2) 1900-01-01 (date) in column `date_mov_out_ymd_date`
  # (3) 19000101 (numeric) in column `date_mov_out_ywwd_num`
  data_hospital_mov_1 <- data_hospital_mov_0 %>%
    mutate(date_mov_out_yw_num = case_when(transfer_date_next != "NULL" ~ as.character(substr(transfer_date_next, 1, 10)),
                                           TRUE ~ "1900-01-01"),
           date_mov_out_ymd_date = case_when(transfer_date_next != "NULL" ~ as.character(substr(transfer_date_next, 1, 10)),
                                             TRUE ~ "1900-01-01"),
           date_mov_out_ywwd_num = case_when(transfer_date_next != "NULL" ~ as.character(substr(transfer_date_next, 1, 10)),
                                             TRUE ~ "1900-01-01"))
  
  data_hospital_mov_2 <- data_hospital_mov_1 %>%
    mutate(date_mov_out_yw_num = 100 * lubridate::isoyear(lubridate::ymd(date_mov_out_yw_num)) +
             lubridate::isoweek(lubridate::ymd(date_mov_out_yw_num)),
           date_mov_out_ymd_date = lubridate::ymd(date_mov_out_ymd_date),
           date_mov_out_ywwd_num = 10000 * lubridate::isoyear(lubridate::ymd(date_mov_out_ywwd_num)) +
             100 * lubridate::isoweek(lubridate::ymd(date_mov_out_ywwd_num)) +
             lubridate::wday(lubridate::ymd(date_mov_out_ywwd_num), week_start = getOption("lubridate.week.start", 1))) %>%
    dplyr::select(-c("transfer_date_next"))
  
  # remove `data_hospital_mov_0` and `data_hospital_mov_1` from the environment
  remove(data_hospital_mov_0, data_hospital_mov_1)
  
  # `data_hospital_mov`:
  # rename columns: `id_patient` for patient id and `id_case` for case id
  data_hospital_mov <- data_hospital_mov_2 %>%
    dplyr::rename(c("id_patient" = "dim_patient_bk_pseudo",
                    "id_case" = "dim_fall_bk_pseudo"))
  
  # remove `data_hospital_mov_2` from the environment
  remove(data_hospital_mov_2)
  
  # store `data_hospital_mov` as a csv file
  write_csv(x = data_hospital_mov,
            file = Path_Data_Hospital_Mov_Processed)
  
  # remove `data_hospital_mov` from the environment
  remove(data_hospital_mov)
  
}


