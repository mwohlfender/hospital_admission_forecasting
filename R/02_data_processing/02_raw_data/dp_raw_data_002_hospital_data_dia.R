

# process hospital data
# data source: IDSC
# different checks and changes are carried out on raw electronic health record data,
# e.g., standardization of date formats and checks for unrealistic values

if (!(file.exists(Path_Data_Hospital_Dia_Processed)) | Bool_Data_Processing_Do_New) {
  
  # read raw hospital data: diagnoses
  data_hospital_dia_raw <- read_csv(file = Path_Data_Hospital_Dia_Raw)
  
  # `data_hospital_dia_raw`:
  # (a) filter to rows with icd10 code different from "Unknown"
  # (b) remove rows whose entry in the column `icd10` is not a capital letter
  data_hospital_dia <- data_hospital_dia_raw %>%
    filter(icd10 != "Unknown") %>%
    filter(substr(icd10,1,1) %in% LETTERS)
  
  # remove `data_hospital_dia_raw` from the environment
  remove(data_hospital_dia_raw)
  
  # check for right format pf icd10 codes,
  # i.e., starting with a capital letter followed by a two-digit number 
  nrows_data_hospital_dia_correct <- data_hospital_dia %>%
    filter(substr(icd10,1,1) %in% LETTERS) %>%
    filter(substr(icd10,2,2) %in% 0:9) %>%
    filter(substr(icd10,3,3) %in% 0:9) %>%
    nrow()
  
  print(sprintf("All icd10 codes are in suitable format: %d (1: TRUE, 0: FALSE)",
                nrows_data_hospital_dia_correct == nrow(data_hospital_dia)))
  
  # remove `nrows_data_hospital_dia_correct` from the environment
  remove(nrows_data_hospital_dia_correct)
  
  # `data_hospital_dia`:
  # (a) add column `icd10_chapter`: chapter of icd10 code
  # (b) add column `icd10_category`: category of icd10 code
  data_hospital_dia <- data_hospital_dia %>%
    mutate(icd10_chapter = substr(icd10,1,1)) %>%
    mutate(icd10_category = substr(icd10,1,3))
  
  # `data_hospital_dia`:
  # (a) add column `date_dia_yw_num`: column `dia_case_date` in format YYYYWW (year/week) (numeric)
  # (b) add column `date_dia_ymd_date`: column `dia_case_date` in format YYYY-MM-DD (year/month/day) (date)
  # (c) add column `date_dia_ywwd_num`: column `dia_case_date` in format YYYYWWDD (year/week/day of week) (numeric)
  # (d) rename columns: `id_patient` for patient id and `id_case` for case id
  data_hospital_dia <- data_hospital_dia %>% 
    mutate(date_dia_yw_num = 100 * lubridate::isoyear(dia_case_date) + 
             lubridate::isoweek(dia_case_date),
           date_dia_ymd_date = lubridate::ymd(substr(dia_case_date, 1, 10)),
           date_dia_ywwd_num = 10000 * lubridate::isoyear(dia_case_date) + 
             100 * lubridate::isoweek(dia_case_date) + 
             lubridate::wday(dia_case_date,  week_start = getOption("lubridate.week.start", 1))) %>%
    dplyr::rename(c("id_patient" = "dim_patient_bk_pseudo",
                    "id_case" = "dim_fall_bk_pseudo"))
  
  # store `data_hospital_dia` as a csv file
  write_csv(x = data_hospital_dia,
            file = Path_Data_Hospital_Dia_Processed)
  
  # remove `data_hospital_dia` from the environment
  remove(data_hospital_dia)
  
}