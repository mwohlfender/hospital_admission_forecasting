

# process hospital data
# data source: IDSC
# different checks and changes are carried out on raw electronic health record data,
# e.g., standardization of date formats and checks for unrealistic values

if (!(file.exists(Path_Data_Hospital_VDat_Body_Temperature_Processed)) | Bool_Data_Processing_Do_New) {
  
  # read raw hospital data: vital data
  data_hospital_vdat_raw <- read_csv(file = Path_Data_Hospital_VDat_Raw)
  
  # `data_hospital_vdat_raw`:
  # (a) filter to rows with value "T°" in the column `measurement`
  # (b) remove column `measurement`
  # (c) rename column: `id_patient` for patient id
  data_hospital_vdat_body_temperature_0 <- data_hospital_vdat_raw %>%
    filter(measurement == "T°") %>%
    dplyr::select(-c("measurement")) %>%
    dplyr::rename(c("id_patient" = "dim_patient_bk_pseudo"))
  
  # `data_hospital_vdat_body_temperature`:
  # (a) filter to values between 35.0 and 43.0 in the column 
  data_hospital_vdat_body_temperature <- data_hospital_vdat_body_temperature_0 %>%
    filter(value_measured >= 35.0) %>%
    filter(value_measured <= 43.0)
  
  print(paste0(nrow(data_hospital_vdat_body_temperature_0) - nrow(data_hospital_vdat_body_temperature), " temperature measurements have been removed."))
  
  # `data_hospital_vdat_body_temperature`:
  # (a) add column `date_measured_string`: column `date_measured` in format YYYY-MM-DD (string)
  # (b) remove columns `date_measured` and `caseyear`
  data_hospital_vdat_body_temperature <- data_hospital_vdat_body_temperature %>%
    mutate(date_measured_string = paste0(substr(x = as.character(date_measured), start = 1, stop = 4), "-",
                                         substr(x = as.character(date_measured), start = 5, stop = 6), "-",
                                         substr(x = as.character(date_measured), start = 7, stop = 8))) %>%
    dplyr::select(-c("date_measured", "caseyear"))
  
  # `data_hospital_vdat_body_temperature`:
  # (a) add column `date_measured_yw_num`: column `date_measured_string` in format YYYYWW (numeric)
  # (b) add column `date_measured_ymd_date`: column `date_measured_string` in format YYYYWWDD (date)
  # (c) add column `date_measured_ywwd_num`: column `date_measured_string` in format YYYYWWDD (numeric)
  # (d) remove column `date_measured_string`
  data_hospital_vdat_body_temperature <- data_hospital_vdat_body_temperature %>%
    mutate(date_measured_yw_num = 100 * lubridate::isoyear(date_measured_string) +
             lubridate::isoweek(date_measured_string),
           date_measured_ymd_date = lubridate::ymd(date_measured_string),
           date_measured_ywwd_num = 10000 * lubridate::isoyear(date_measured_string) +
             100 * lubridate::isoweek(date_measured_string) +
             lubridate::wday(date_measured_string, week_start = getOption("lubridate.week.start", 1))) %>%
    dplyr::select(-c("date_measured_string"))
  
  # store `data_hospital_vdat_body_temperature` as a csv file
  write_csv(x = data_hospital_vdat_body_temperature,
            file = Path_Data_Hospital_VDat_Body_Temperature_Processed)
  
  # remove `data_hospital_vdat_raw`, `data_hospital_vdat_body_temperature_0` and `data_hospital_vdat_body_temperature` from environment
  remove(data_hospital_vdat_raw, data_hospital_vdat_body_temperature_0, data_hospital_vdat_body_temperature)
  
}


