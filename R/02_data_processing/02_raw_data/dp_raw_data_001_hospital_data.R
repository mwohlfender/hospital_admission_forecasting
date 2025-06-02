

# process hospital data
# data source: IDSC
# different checks and changes are carried out on raw electronic health record data,
# e.g., standardization of date formats and checks for unrealistic values

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


