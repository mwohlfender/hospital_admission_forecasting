

# process hospital data
# data source: IDSC
# different checks and changes are carried out on raw electronic health record data,
# e.g., standardization of date formats and checks for unrealistic values

# main ----
if (!(file.exists(Path_Data_Hospital_Main_Processed)) | Bool_Data_Processing_Do_New) {
  
  # read raw hospital data: main
  data_hospital_main_raw <- read_csv(file = Path_Data_Hospital_Main_Raw)
  
  # determine the number of missing values in all columns of `data_hospital_main_raw`
  print("Missing values in `data_hospital_main_raw`:")
  data_hospital_main_raw %>% 
    summarise_all(~(sum(is.na(.)))) %>%
    select_if(. > 0) %>%
    print()
  
  # check that the column `dim_fall_bk_pseudo` is a unique id of `data_hospital_main_raw`
  print(sprintf("The column `dim_fall_bk_pseudo` is a unique id of `data_hospital_main_raw`: %d (1: TRUE, 0: FALSE)",
                length(unique(data_hospital_main_raw$dim_fall_bk_pseudo)) == nrow(data_hospital_main_raw)))
  
  # check that the column `age` of `data_hospital_main_raw` corresponds to age of patient at beginning of case
  print(sprintf("The column `age` of `data_hospital_main_raw` corresponds to age of patient at beginning of case: %d (1: TRUE, 0: FALSE)",
                sum((lubridate::year(data_hospital_main_raw$case_start) - data_hospital_main_raw$year_of_birth - data_hospital_main_raw$age) %in% c(0, 1))
                == nrow(data_hospital_main_raw)))
  
  # check for unrealistic years of birth
  print(sprintf("Number of cases with date of birth of patient before 1890: %d",
                sum(data_hospital_main_raw$year_of_birth < 1890)))
  
  # `data_hospital_main_raw`:
  # (a) filter to patients born 1890 or later
  # (b) add column `date_case_start_yw_num`: column `case_start` in format YYYYWW (year/week) (numeric)
  # (c) add column `date_case_end_yw_num`: column `case_end` in format YYYYWW (year/week) (numeric)
  # (d) add column `date_case_start_ymd_date`: column `case_start` in format YYYY-MM-DD (year/month/day) (date)
  # (e) add column `date_case_end_ymd_date`: column `case_end` in format YYYY-MM-DD (year/month/day) (date)
  # (f) add column `date_case_start_ywwd_num`: column `case_start` in format YYYYWWDD (year/week/day of week) (numeric)
  # (g) add column `date_case_end_ywwd_num`: column `case_end` in format YYYYWWDD (year/week/day of week) (numeric)
  data_hospital_main <- data_hospital_main_raw %>% 
    filter(year_of_birth >= 1890) %>%
    mutate(date_case_start_yw_num = 100 * lubridate::isoyear(case_start) + 
             lubridate::isoweek(case_start),
           date_case_end_yw_num = 100 * lubridate::isoyear(case_end) + 
             lubridate::isoweek(case_end),
           date_case_start_ymd_date =  lubridate::ymd(substr(case_start, 1, 10)),
           date_case_end_ymd_date =  lubridate::ymd(substr(case_end, 1, 10)),
           date_case_start_ywwd_num = 10000 * lubridate::isoyear(case_start) + 
             100 * lubridate::isoweek(case_start) + 
             lubridate::wday(case_start, week_start = getOption("lubridate.week.start", 1)),
           date_case_end_ywwd_num = 10000 * lubridate::isoyear(case_end) + 
             100 * lubridate::isoweek(case_end) + 
             lubridate::wday(case_end, week_start = getOption("lubridate.week.start", 1)))
  
  # remove `data_hospital_main_raw` from the environment
  remove(data_hospital_main_raw)
  
  # define breaks for age groups
  # `breaks_age_patients_foph`: age groups used in FOPH COVID-19 data
  # `breaks_age_patients_sent`: age groups used in FOPH Sentinella system
  breaks_age_patients_foph <- c(min(data_hospital_main$age)-1, 9, 19, 29, 39, 49, 59, 69, 79, max(data_hospital_main$age))
  breaks_age_patients_sent <- c(min(data_hospital_main$age)-1, 4, 14, 29, 64, max(data_hospital_main$age))
  
  # `data_hospital_main`:
  # (a) add column `group_age_foph`: interval of entries of `breaks_age_patients_foph` in which value of column `age` lies 
  # (value between 1 and length(`breaks_age_patients_foph`) - 1)
  # (b) add column `group_age_sent`: interval of entries of `breaks_age_patients_sent` in which value of column `age` lies 
  # (value between 1 and length(`breaks_age_patients_sent`) - 1)
  # (c) rename columns: `id_patient` for patient id and `id_case` for case id
  data_hospital_main <- data_hospital_main %>%
    mutate(group_age_foph = cut(x = data_hospital_main$age, breaks = breaks_age_patients_foph, labels = 1:(length(breaks_age_patients_foph) - 1)),
           group_age_sent = cut(x = data_hospital_main$age, breaks = breaks_age_patients_sent, labels = 1:(length(breaks_age_patients_sent) - 1))) %>%
    dplyr::rename(c("id_patient" = "dim_patient_bk_pseudo",
                    "id_case" = "dim_fall_bk_pseudo"))
  
  remove(breaks_age_patients_foph, breaks_age_patients_sent)
  
  # store `data_hospital_main` as a csv file
  write_csv(x = data_hospital_main,
            file = Path_Data_Hospital_Main_Processed)
  
  # remove `data_hospital_main` from the environment
  remove(data_hospital_main)
  
}



# dia ----
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



# mov ----
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



# vdat body temperature ----
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



# lab crp ----
if (!(file.exists(Path_Data_Hospital_Lab_CRP_Processed)) | Bool_Data_Processing_Do_New) {
  
  # read raw hospital data: laboratory values
  data_hospital_lab_raw <- read_csv(file = Path_Data_Hospital_Lab_Raw)
  
  # `data_hospital_lab_raw`:
  # (a) filter to rows with value "CRP" in the column `dim_rl_methode_kurz`
  # (b) remove column `dim_rl_methode_kurz`
  # (c) rename columns: `id_patient` for `dim_patient_bk_pseudo` and `id_case` for `dim_fall_bk_pseudo`
  data_hospital_lab_crp <- data_hospital_lab_raw %>%
    filter(dim_rl_methode_kurz == "CRP") %>%
    dplyr::select(-c("dim_rl_methode_kurz")) %>%
    dplyr::rename(c("id_patient" = "dim_patient_bk_pseudo", "id_case" = "dim_fall_bk_pseudo"))

  # `data_hospital_vdat_body_weight`:
  # (a) add column `date_measured_yw_num`: column `dim_rl_probe_spital_abnahme_datum` in format YYYYWW (numeric)
  # (b) add column `date_measured_ymd_date`: column `dim_rl_probe_spital_abnahme_datum` in format YYYYWWDD (date)
  # (c) add column `date_measured_ywwd_num`: column `dim_rl_probe_spital_abnahme_datum` in format YYYYWWDD (numeric)
  # (d) remove column `dim_rl_probe_spital_abnahme_datum`
  data_hospital_lab_crp <- data_hospital_lab_crp %>%
    mutate(date_measured_yw_num = 100 * lubridate::isoyear(dim_rl_probe_spital_abnahme_datum) +
             lubridate::isoweek(dim_rl_probe_spital_abnahme_datum),
           date_measured_ymd_date = lubridate::as_date(dim_rl_probe_spital_abnahme_datum),
           date_measured_ywwd_num = 10000 * lubridate::isoyear(dim_rl_probe_spital_abnahme_datum) +
             100 * lubridate::isoweek(dim_rl_probe_spital_abnahme_datum) +
             lubridate::wday(dim_rl_probe_spital_abnahme_datum,  week_start = getOption("lubridate.week.start", 1))) %>%
    dplyr::select(-c("dim_rl_probe_spital_abnahme_datum"))
  
  # `data_hospital_lab_crp`:
  # rename columns: `value_measured` for `fakt_rl_resultat_resultat_num` and `unit` for `fakt_rl_resultat_einheit`
  data_hospital_lab_crp <- data_hospital_lab_crp %>%
    dplyr::rename(c("value_measured" = "fakt_rl_resultat_resultat_num", "unit" = "fakt_rl_resultat_einheit"))
  
  # store `data_hospital_vdat_body_weight` as a csv file
  write_csv(x = data_hospital_lab_crp,
            file = Path_Data_Hospital_Lab_CRP_Processed)
  
  # remove `data_hospital_lab_raw` and `data_hospital_lab_crp` from environment
  remove(data_hospital_lab_raw, data_hospital_lab_crp)
  
}


