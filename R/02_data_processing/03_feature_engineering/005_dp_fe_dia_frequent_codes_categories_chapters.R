
# feature engineering 005
# create data summary table: number of patients having been assigned an icd10 code, category or chapter
# that often appears in diagnoses combined with the icd10 code U07.1
# one row: one day between 1 January 2020 and 30 June 2023 and number of patients
# method: determine top five icd10 codes, categories and chapters that appear in diagnoses combined with the icd10 code U07.1,
# for each day between 01 January 2020 and 30 June 2023
# count the number of patients having been assigned one of these icd10 codes, categories and chapters

if (!(file.exists(Path_Table_Days_ICD_Freq_Codes_Cat_Chap_Covid_20200101_20230630)) | Bool_Data_Processing_Do_New) {
  
  sequence_days <- seq(from = ymd("2020-01-01"), to = ymd("2023-06-30"), by = "days")
  
  # read processed hospital data: diagnoses
  data_hospital_dia_0 <- read_csv(file = Path_Data_Hospital_Dia_Processed) %>%
    filter(date_dia_ymd_date %in% sequence_days)
  
  # `data_hospital_dia_0`: remove duplicate icd10 codes (same diagnosis on same day to same patient)
  data_hospital_dia <- data_hospital_dia_0 %>%
    mutate(id_patient_day = 10^8 * id_patient + date_dia_ywwd_num) %>%
    group_by(id_patient_day, icd10) %>%
    summarize(icd10_chapter = unique(icd10_chapter),
              icd10_category = unique(icd10_category),
              date_dia_ymd_date = unique(date_dia_ymd_date)) %>%
    ungroup()
  
  # determine values of the column `id_patient_day` of `data_hospital_dia` that appear together with the icd10 code U07.1
  list_id_patient_day_covid <- data_hospital_dia %>% 
    filter(icd10 == "U07.1") %>%
    pull(id_patient_day)
  
  # `data_hospital_dia`: filter to values of the column `id_patient_day` that appear together with the icd10 code U07.1
  data_hospital_dia_covid <- data_hospital_dia %>%
    filter(id_patient_day %in% list_id_patient_day_covid)
  
  # determine 5 most frequent icd10 codes that have been assigned to hospital stays that have been assigned the icd10 code U07.1
  table_icd_codes_with_covid <- tibble(as.data.frame(table(data_hospital_dia_covid %>% pull(icd10)))) %>%
    mutate(Var1 = as.character(Var1)) %>%
    filter(Var1 != "U07.1") %>%
    arrange(-Freq) %>%
    slice_head(n = 5)
  
  # determine 5 most frequent icd10 code categories that have been assigned to hospital stays that have been assigned the icd10 code U07.1
  table_icd_categories_with_covid <- tibble(as.data.frame(table(data_hospital_dia_covid %>% pull(icd10_category)))) %>%
    mutate(Var1 = as.character(Var1)) %>%
    filter(Var1 != "U07") %>%
    arrange(-Freq) %>%
    slice_head(n = 5)
  
  # determine 5 most frequent icd10 code chapters that have been assigned to hospital stays that have been assigned the icd10 code U07.1
  table_icd_chapters_with_covid <- tibble(as.data.frame(table(data_hospital_dia_covid %>% pull(icd10_chapter)))) %>%
    mutate(Var1 = as.character(Var1)) %>%
    arrange(-Freq) %>%
    slice_head(n = 5)
  
  # (a) determine to how many patients each icd10 code present in `table_icd_codes_with_covid$Var1` has been assigned on each day
  # (b) determine to how many patients at least one icd10 code belonging to the icd10 categories present in `table_icd_categories_with_covid$Var1` has been assigned on each day
  # (c) determine to how many patients at least one icd10 code belonging to the icd10 chapters present in `table_icd_chapters_with_covid$Var1` has been assigned on each day
  data_hospital_dia_frequent_codes_categories_chapters <- data_hospital_dia %>%
    group_by(id_patient_day) %>%
    summarize(date_dia_ymd_date = unique(date_dia_ymd_date),
              icd10_code_top001 = 0 + (sum(icd10 == table_icd_codes_with_covid$Var1[1]) > 0),
              icd10_code_top002 = 0 + (sum(icd10 == table_icd_codes_with_covid$Var1[2]) > 0),
              icd10_code_top003 = 0 + (sum(icd10 == table_icd_codes_with_covid$Var1[3]) > 0),
              icd10_code_top004 = 0 + (sum(icd10 == table_icd_codes_with_covid$Var1[4]) > 0),
              icd10_code_top005 = 0 + (sum(icd10 == table_icd_codes_with_covid$Var1[5]) > 0),
              icd10_cat_top001 = 0 + (sum(icd10_category == table_icd_categories_with_covid$Var1[1]) > 0),
              icd10_cat_top002 = 0 + (sum(icd10_category == table_icd_categories_with_covid$Var1[2]) > 0),
              icd10_cat_top003 = 0 + (sum(icd10_category == table_icd_categories_with_covid$Var1[3]) > 0),
              icd10_cat_top004 = 0 + (sum(icd10_category == table_icd_categories_with_covid$Var1[4]) > 0),
              icd10_cat_top005 = 0 + (sum(icd10_category == table_icd_categories_with_covid$Var1[5]) > 0),
              icd10_chap_top001 = 0 + (sum(icd10_chapter == table_icd_chapters_with_covid$Var1[1]) > 0),
              icd10_chap_top002 = 0 + (sum(icd10_chapter == table_icd_chapters_with_covid$Var1[2]) > 0),
              icd10_chap_top003 = 0 + (sum(icd10_chapter == table_icd_chapters_with_covid$Var1[3]) > 0),
              icd10_chap_top004 = 0 + (sum(icd10_chapter == table_icd_chapters_with_covid$Var1[4]) > 0),
              icd10_chap_top005 = 0 + (sum(icd10_chapter == table_icd_chapters_with_covid$Var1[5]) > 0)) %>%
    ungroup()
  
  
  table_days_frequent_codes_categories_chapters_0 <- data_hospital_dia_frequent_codes_categories_chapters %>%
    group_by(date_dia_ymd_date) %>%
    summarize(icd10_code_top001 = sum(icd10_code_top001),
              icd10_code_top002 = sum(icd10_code_top002),
              icd10_code_top003 = sum(icd10_code_top003),
              icd10_code_top004 = sum(icd10_code_top004),
              icd10_code_top005 = sum(icd10_code_top005),
              icd10_cat_top001 = sum(icd10_cat_top001),
              icd10_cat_top002 = sum(icd10_cat_top002),
              icd10_cat_top003 = sum(icd10_cat_top003),
              icd10_cat_top004 = sum(icd10_cat_top004),
              icd10_cat_top005 = sum(icd10_cat_top005),
              icd10_chap_top001 = sum(icd10_chap_top001),
              icd10_chap_top002 = sum(icd10_chap_top002),
              icd10_chap_top003 = sum(icd10_chap_top003),
              icd10_chap_top004 = sum(icd10_chap_top004),
              icd10_chap_top005 = sum(icd10_chap_top005)) %>%
    ungroup()
  
  
  table_days_frequent_codes_categories_chapters <- tibble(date_dia_ymd_date = sequence_days) %>%
    left_join(table_days_frequent_codes_categories_chapters_0) %>%
    replace(is.na(.), 0)
  
  
  names_table_days_frequent_codes_categories_chapters <- c("date_day",
                                                           paste0("icd10_code_001_", table_icd_codes_with_covid$Var1[1]),
                                                           paste0("icd10_code_002_", table_icd_codes_with_covid$Var1[2]),
                                                           paste0("icd10_code_003_", table_icd_codes_with_covid$Var1[3]),
                                                           paste0("icd10_code_004_", table_icd_codes_with_covid$Var1[4]),
                                                           paste0("icd10_code_005_", table_icd_codes_with_covid$Var1[5]),
                                                           paste0("icd10_cat_001_", table_icd_categories_with_covid$Var1[1]),
                                                           paste0("icd10_cat_002_", table_icd_categories_with_covid$Var1[2]),
                                                           paste0("icd10_cat_003_", table_icd_categories_with_covid$Var1[3]),
                                                           paste0("icd10_cat_004_", table_icd_categories_with_covid$Var1[4]),
                                                           paste0("icd10_cat_005_", table_icd_categories_with_covid$Var1[5]),
                                                           paste0("icd10_chap_001_", table_icd_chapters_with_covid$Var1[1]),
                                                           paste0("icd10_chap_002_", table_icd_chapters_with_covid$Var1[2]),
                                                           paste0("icd10_chap_003_", table_icd_chapters_with_covid$Var1[3]),
                                                           paste0("icd10_chap_004_", table_icd_chapters_with_covid$Var1[4]),
                                                           paste0("icd10_chap_005_", table_icd_chapters_with_covid$Var1[5]))
  
  names(table_days_frequent_codes_categories_chapters) <- names_table_days_frequent_codes_categories_chapters
  
  # store results
  write_csv(x = table_days_frequent_codes_categories_chapters,
            file = Path_Table_Days_ICD_Freq_Codes_Cat_Chap_Covid_20200101_20230630)
  
}


