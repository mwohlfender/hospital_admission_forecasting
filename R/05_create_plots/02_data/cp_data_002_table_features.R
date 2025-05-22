

# read data: variables included in the models ----
data_summary_features_numeric <- NULL

precision_round_daily_mean <- 0.1

data_hospitalizations_base <- read_csv(file = get_path_data_features(directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                                                     number_xy = "001",
                                                                     number_combination_features = "000",
                                                                     name_data_set = "base",
                                                                     option = "NONE"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_hospitalizations_base %>%
              summarize(label = "Admissions",
                        details = "COVID-19-related hospital admissions (any age)",
                        feature_set = "A-I and K-L",
                        sum = sum(x),
                        mean = custom_round(mean(x), precision_round_daily_mean),
                        min = min(x),
                        max = max(x),
                        unit = "Patients"))


data_hospitalizations_age <- read_csv(file = get_path_data_features(directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                                                    number_xy = "001",
                                                                    number_combination_features = "000",
                                                                    name_data_set = "age",
                                                                    option = "NONE"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_hospitalizations_age %>%
              summarize(label = "Age 0-4",
                        details = "COVID-19-related hospital admissions (age 0-4)",
                        feature_set = "B",
                        sum = sum(x_group_age_1),
                        mean = custom_round(mean(x_group_age_1), precision_round_daily_mean),
                        min = min(x_group_age_1),
                        max = max(x_group_age_1),
                        unit = "Patients"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_hospitalizations_age %>%
              summarize(label = "Age 5-14",
                        details = "COVID-19-related hospital admissions (age 5-14)",
                        feature_set = "B",
                        sum = sum(x_group_age_2),
                        mean = custom_round(mean(x_group_age_2), precision_round_daily_mean),
                        min = min(x_group_age_2),
                        max = max(x_group_age_2),
                        unit = "Patients"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_hospitalizations_age %>%
              summarize(label = "Age 15-29",
                        details = "COVID-19-related hospital admissions (age 15-29)",
                        feature_set = "B",
                        sum = sum(x_group_age_3),
                        mean = custom_round(mean(x_group_age_3), precision_round_daily_mean),
                        min = min(x_group_age_3),
                        max = max(x_group_age_3),
                        unit = "Patients"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_hospitalizations_age %>%
              summarize(label = "Age 30-64",
                        details = "COVID-19-related hospital admissions (age 30-64)",
                        feature_set = "B",
                        sum = sum(x_group_age_4),
                        mean = custom_round(mean(x_group_age_4), precision_round_daily_mean),
                        min = min(x_group_age_4),
                        max = max(x_group_age_4),
                        unit = "Patients"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_hospitalizations_age %>%
              summarize(label = "Age 65+",
                        details = "COVID-19-related hospital admissions (age 65+)",
                        feature_set = "B",
                        sum = sum(x_group_age_5),
                        mean = custom_round(mean(x_group_age_5), precision_round_daily_mean),
                        min = min(x_group_age_5),
                        max = max(x_group_age_5),
                        unit = "Patients"))


data_features_001 <- read_csv(file = get_path_data_features(directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                                            number_xy = "001",
                                                            number_combination_features = "001",
                                                            name_data_set = "base",
                                                            option = "NONE"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_features_001 %>%
              summarize(label = "Emergency",
                        details = "Patients seeking treatment at Bern University Hospital Emergency Department",
                        feature_set = "C, I and L",
                        sum = sum(visits_emergency),
                        mean = custom_round(mean(visits_emergency), precision_round_daily_mean),
                        min = min(visits_emergency),
                        max = max(visits_emergency),
                        unit = "Patients"))


data_features_008 <- read_csv(file = get_path_data_features(directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                                            number_xy = "001",
                                                            number_combination_features = "008",
                                                            name_data_set = "base",
                                                            option = "NONE"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_features_008 %>%
              summarize(label = "ICD10 R",
                        details = "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified",
                        feature_set = "D",
                        sum = sum(icd10_chap_001_R),
                        mean = custom_round(mean(icd10_chap_001_R), precision_round_daily_mean),
                        min = min(icd10_chap_001_R),
                        max = max(icd10_chap_001_R),
                        unit = "Patients"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_features_008 %>%
              summarize(label = "ICD10 I",
                        details = "Diseases of the circulatory system",
                        feature_set = "D",
                        sum = sum(icd10_chap_002_I),
                        mean = custom_round(mean(icd10_chap_002_I), precision_round_daily_mean),
                        min = min(icd10_chap_002_I),
                        max = max(icd10_chap_002_I),
                        unit = "Patients"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_features_008 %>%
              summarize(label = "ICD10 E",
                        details = "Endocrine, nutritional and metabolic diseases",
                        feature_set = "D",
                        sum = sum(icd10_chap_003_E),
                        mean = custom_round(mean(icd10_chap_003_E), precision_round_daily_mean),
                        min = min(icd10_chap_003_E),
                        max = max(icd10_chap_003_E),
                        unit = "Patients"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_features_008 %>%
              summarize(label = "ICD10 J",
                        details = "Diseases of the respiratory system",
                        feature_set = "D",
                        sum = sum(icd10_chap_004_J),
                        mean = custom_round(mean(icd10_chap_004_J), precision_round_daily_mean),
                        min = min(icd10_chap_004_J),
                        max = max(icd10_chap_004_J),
                        unit = "Patients"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_features_008 %>%
              summarize(label = "ICD10 Z",
                        details = "Factors influencing health status and contact with health services",
                        feature_set = "D",
                        sum = sum(icd10_chap_005_Z),
                        mean = custom_round(mean(icd10_chap_005_Z), precision_round_daily_mean),
                        min = min(icd10_chap_005_Z),
                        max = max(icd10_chap_005_Z),
                        unit = "Patients"))


data_features_007 <- read_csv(file = get_path_data_features(directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                                            number_xy = "001",
                                                            number_combination_features = "007",
                                                            name_data_set = "base",
                                                            option = "NONE"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_features_007 %>%
              summarize(label = "ICD10 E87",
                        details = "Other disorders of fluid, electrolyte and acid-base balance",
                        feature_set = "E",
                        sum = sum(icd10_cat_001_E87),
                        mean = custom_round(mean(icd10_cat_001_E87), precision_round_daily_mean),
                        min = min(icd10_cat_001_E87),
                        max = max(icd10_cat_001_E87),
                        unit = "Patients"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_features_007 %>%
              summarize(label = "ICD10 J12",
                        details = "Viral pneumonia, not elsewhere classified",
                        feature_set = "E",
                        sum = sum(icd10_cat_002_J12),
                        mean = custom_round(mean(icd10_cat_002_J12), precision_round_daily_mean),
                        min = min(icd10_cat_002_J12),
                        max = max(icd10_cat_002_J12),
                        unit = "Patients"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_features_007 %>%
              summarize(label = "ICD10 J96",
                        details = "Respiratory failure, not elsewhere classified",
                        feature_set = "E",
                        sum = sum(icd10_cat_003_J96),
                        mean = custom_round(mean(icd10_cat_003_J96), precision_round_daily_mean),
                        min = min(icd10_cat_003_J96),
                        max = max(icd10_cat_003_J96),
                        unit = "Patients"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_features_007 %>%
              summarize(label = "ICD10 I10",
                        details = "Essential (primary) hypertension",
                        feature_set = "E",
                        sum = sum(icd10_cat_004_I10),
                        mean = custom_round(mean(icd10_cat_004_I10), precision_round_daily_mean),
                        min = min(icd10_cat_004_I10),
                        max = max(icd10_cat_004_I10),
                        unit = "Patients"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_features_007 %>%
              summarize(label = "ICD10 N18",
                        details = "Chronic kidney disease",
                        feature_set = "E",
                        sum = sum(icd10_cat_005_N18),
                        mean = custom_round(mean(icd10_cat_005_N18), precision_round_daily_mean),
                        min = min(icd10_cat_005_N18),
                        max = max(icd10_cat_005_N18),
                        unit = "Patients"))


data_features_006 <- read_csv(file = get_path_data_features(directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                                            number_xy = "001",
                                                            number_combination_features = "006",
                                                            name_data_set = "base",
                                                            option = "NONE"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_features_006 %>%
              summarize(label = "ICD10 J12.8",
                        details = "Other viral pneumonia",
                        feature_set = "F, I and L",
                        sum = sum(icd10_code_001_J12.8),
                        mean = custom_round(mean(icd10_code_001_J12.8), precision_round_daily_mean),
                        min = min(icd10_code_001_J12.8),
                        max = max(icd10_code_001_J12.8),
                        unit = "Patients"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_features_006 %>%
              summarize(label = "ICD10 I10.90",
                        details = "Essential hypertension, unspecified without indication of hypertensive crisis",
                        feature_set = "F, I and L",
                        sum = sum(icd10_code_002_I10.90),
                        mean = custom_round(mean(icd10_code_002_I10.90), precision_round_daily_mean),
                        min = min(icd10_code_002_I10.90),
                        max = max(icd10_code_002_I10.90),
                        unit = "Patients"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_features_006 %>%
              summarize(label = "ICD10 J96.00",
                        details = "Acute respiratory failure, not elsewhere classified",
                        feature_set = "F, I and L",
                        sum = sum(icd10_code_003_J96.00),
                        mean = custom_round(mean(icd10_code_003_J96.00), precision_round_daily_mean),
                        min = min(icd10_code_003_J96.00),
                        max = max(icd10_code_003_J96.00),
                        unit = "Patients"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_features_006 %>%
              summarize(label = "ICD10 Z22.8",
                        details = "Carrier of other infectious diseases",
                        feature_set = "F, I and L",
                        sum = sum(icd10_code_004_Z22.8),
                        mean = custom_round(mean(icd10_code_004_Z22.8), precision_round_daily_mean),
                        min = min(icd10_code_004_Z22.8),
                        max = max(icd10_code_004_Z22.8),
                        unit = "Patients"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_features_006 %>%
              summarize(label = "ICD10 B33.8",
                        details = "Other specified viral diseases",
                        feature_set = "F, I and L",
                        sum = sum(icd10_code_005_B33.8),
                        mean = custom_round(mean(icd10_code_005_B33.8), precision_round_daily_mean),
                        min = min(icd10_code_005_B33.8),
                        max = max(icd10_code_005_B33.8),
                        unit = "Patients"))


data_features_002 <- read_csv(file = get_path_data_features(directory_data = Directory_Data_Models,
                                                            number_xy = "001",
                                                            number_combination_features = "002",
                                                            name_data_set = "base",
                                                            option = "NONE"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_features_002 %>%
              summarize(label = "Fever",
                        details = "Highest body temperature measurement on day of admission at least 38.5 degrees Celsius",
                        feature_set = "G, I and L",
                        sum = sum(fever_first_day),
                        mean = custom_round(mean(fever_first_day), precision_round_daily_mean),
                        min = min(fever_first_day),
                        max = max(fever_first_day),
                        unit = "Patients"))


data_features_005 <- read_csv(file = get_path_data_features(directory_data = Directory_Data_Models,
                                                            number_xy = "001",
                                                            number_combination_features = "005",
                                                            name_data_set = "base",
                                                            option = "NONE"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_features_005 %>%
              summarize(label = "CRP",
                        details = "Highest CRP concentration measurement on day of admission at least 50 mg/l",
                        feature_set = "H, I and L",
                        sum = sum(high_crp_first_day),
                        mean = custom_round(mean(high_crp_first_day), precision_round_daily_mean),
                        min = min(high_crp_first_day),
                        max = max(high_crp_first_day),
                        unit = "Patients"))


data_summary_features_character <- data_summary_features_numeric %>%
  mutate(sum = as.character(formatC(sum, big.mark = ",", big.interval = 3, digits = 0, format = "f")),
         mean = as.character(formatC(mean, big.mark = ",", big.interval = 3, digits = 1, format = "f")),
         min = as.character(formatC(min, big.mark = ",", big.interval = 3, digits = 0, format = "f")),
         max = as.character(formatC(max, big.mark = ",", big.interval = 3, digits = 0, format = "f")))


data_features_004 <- read_csv(file = get_path_data_features(directory_data = Directory_Data_Models,
                                                            number_xy = "001",
                                                            number_combination_features = "004",
                                                            name_data_set = "base",
                                                            option = "NONE"))

data_summary_features_numeric <- data_summary_features_numeric %>%
  bind_rows(data_features_004 %>%
              summarize(label = "Wastewater",
                        details = "SARS-CoV-2 RNA copies per 100,000 people living in the collection area of the Sensetal Laupen wastewater treatment plant and day",
                        feature_set = "J, K and L",
                        sum = sum(sars_cov2_rna),
                        mean = mean(sars_cov2_rna),
                        min = min(sars_cov2_rna),
                        max = max(sars_cov2_rna),
                        unit = "SARS-CoV-2 RNA copies / 100,000 inhabitants and day"))


data_summary_features_character <- data_summary_features_character %>%
  bind_rows(data_features_004 %>%
              summarize(label = "Wastewater",
                        details = "SARS-CoV-2 RNA copies per 100,000 people living in the collection area of the Sensetal Laupen wastewater treatment plant and day",
                        feature_set = "J, K and L",
                        sum = as.character(formatC(sum(sars_cov2_rna), format = "e", digits = 1)),
                        mean = as.character(formatC(mean(sars_cov2_rna), format = "e", digits = 1)),
                        min = as.character(formatC(min(sars_cov2_rna), format = "e", digits = 1)),
                        max = as.character(formatC(max(sars_cov2_rna), format = "e", digits = 1)),
                        unit = "SARS-CoV-2 RNA copies / 100,000 inhabitants and day"))



data_summary_features_table <- data_summary_features_character %>% dplyr::select(-c("unit"))

# create flextable: overview of average score per model and dataset for training period
table_data_summary_features <- flextable(data_summary_features_table)
table_data_summary_features <- set_header_labels(table_data_summary_features,
                                                 values = list(label = "Variable",
                                                               details = "Details",
                                                               feature_set = "Feature sets",
                                                               sum = "Sum",
                                                               mean = "Daily Mean",
                                                               min = "Daily Min",
                                                               max = "Daily Max"))

sum_wastewater <- data_summary_features_numeric$sum[25]
log_sum_wastewater <- floor(log(sum_wastewater, base = 10))

table_data_summary_features <- compose(table_data_summary_features, part = "body", i = 25, j = 4,
                                       value = as_paragraph(paste0(as.character(formatC(custom_round(sum_wastewater / 10^log_sum_wastewater,
                                                                                                     precision_round_daily_mean), format = "f", digits = 1)), "\u00D710"),
                                                            as_sup(as.character(log_sum_wastewater))))

mean_wastewater <- data_summary_features_numeric$mean[25]
log_mean_wastewater <- floor(log(mean_wastewater, base = 10))

table_data_summary_features <- compose(table_data_summary_features, part = "body", i = 25, j = 5,
                                       value = as_paragraph(paste0(as.character(formatC(custom_round(mean_wastewater / 10^log_mean_wastewater,
                                                                                                     precision_round_daily_mean), format = "f", digits = 1)), "\u00D710"),
                                                            as_sup(as.character(log_mean_wastewater))))

min_wastewater <- data_summary_features_numeric$min[25]
log_min_wastewater <- floor(log(min_wastewater, base = 10))

table_data_summary_features <- compose(table_data_summary_features, part = "body", i = 25, j = 6,
                                       value = as_paragraph(paste0(as.character(formatC(custom_round(min_wastewater / 10^log_min_wastewater,
                                                                                                     precision_round_daily_mean), format = "f", digits = 1)), "\u00D710"),
                                                            as_sup(as.character(log_min_wastewater))))

max_wastewater <- data_summary_features_numeric$max[25]
log_max_wastewater <- floor(log(max_wastewater, base = 10))

table_data_summary_features <- compose(table_data_summary_features, part = "body", i = 25, j = 7,
                                       value = as_paragraph(paste0(as.character(formatC(custom_round(max_wastewater / 10^log_max_wastewater,
                                                                                                     precision_round_daily_mean), format = "f", digits = 1)), "\u00D710"),
                                                            as_sup(as.character(log_max_wastewater))))

table_data_summary_features <- theme_vanilla(table_data_summary_features)
table_data_summary_features <- align(table_data_summary_features,
                                     j = 4:7,
                                     align = "right",
                                     part = "all")
table_data_summary_features <- width(table_data_summary_features,
                                     j = 1:ncol(data_summary_features_table),
                                     width = c(1.1, 3.9, 1.1, 0.85, 1.0, 0.85, 0.85),
                                     unit = "in")

flextable::save_as_image(x = table_data_summary_features,
                         path = paste0(Directory_Plots, Subdirectory_Plots_Tables, "table_data_summary_features.png"))

flextable::save_as_image(x = table_data_summary_features,
                         path = paste0(Directory_Plots, Subdirectory_Plots_Manuscript, "Table_1.png"))

flextable::save_as_image(x = table_data_summary_features,
                         path = paste0(Directory_Plots, Subdirectory_Plots_Presentation, "table_data_summary_features.png"))


