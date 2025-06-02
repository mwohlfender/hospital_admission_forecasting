

# process hospital data
# data source: IDSC
# different checks and changes are carried out on raw electronic health record data,
# e.g., standardization of date formats and checks for unrealistic values

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