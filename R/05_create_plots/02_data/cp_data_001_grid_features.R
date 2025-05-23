
# create overview plot of time series used as model input


# read data: variables included in the models ----
data_features <- NULL

data_hospitalizations_base <- read_csv(file = get_path_data_features(directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                                                     number_xy = "001",
                                                                     number_combination_features = "000",
                                                                     name_data_set = "base",
                                                                     option = "NONE"))

data_features <- data_features %>% rbind(data_hospitalizations_base %>%
                                           mutate(type = "all",
                                                  color = "color_1",
                                                  value_scaled = (x - min(x)) / (1.05*(max(x) - min(x)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))


data_hospitalizations_age <- read_csv(file = get_path_data_features(directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                                                    number_xy = "001",
                                                                    number_combination_features = "000",
                                                                    name_data_set = "age",
                                                                    option = "NONE"))

data_features <- data_features %>% rbind(data_hospitalizations_age %>%
                                           mutate(type = "age_group_1",
                                                  color = "color_1",
                                                  value_scaled = (x_group_age_1 - min(x_group_age_1)) / (1.05*(max(x_group_age_1) - min(x_group_age_1)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))

data_features <- data_features %>% rbind(data_hospitalizations_age %>%
                                           mutate(type = "age_group_2",
                                                  color = "color_1",
                                                  value_scaled = (x_group_age_2 - min(x_group_age_2)) / (1.05*(max(x_group_age_2) - min(x_group_age_2)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))

data_features <- data_features %>% rbind(data_hospitalizations_age %>%
                                           mutate(type = "age_group_3",
                                                  color = "color_1",
                                                  value_scaled = (x_group_age_3 - min(x_group_age_3)) / (1.05*(max(x_group_age_3) - min(x_group_age_3)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))

data_features <- data_features %>% rbind(data_hospitalizations_age %>%
                                           mutate(type = "age_group_4",
                                                  color = "color_1",
                                                  value_scaled = (x_group_age_4 - min(x_group_age_4)) / (1.05*(max(x_group_age_4) - min(x_group_age_4)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))

data_features <- data_features %>% rbind(data_hospitalizations_age %>%
                                           mutate(type = "age_group_5",
                                                  color = "color_1",
                                                  value_scaled = (x_group_age_5 - min(x_group_age_5)) / (1.05*(max(x_group_age_5) - min(x_group_age_5)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))


data_features_001 <- read_csv(file = get_path_data_features(directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                                            number_xy = "001",
                                                            number_combination_features = "001",
                                                            name_data_set = "base",
                                                            option = "NONE"))

data_features <- data_features %>% rbind(data_features_001 %>%
                                           mutate(type = "001",
                                                  color = "color_2",
                                                  value_scaled = (visits_emergency - min(visits_emergency)) / (1.05*(max(visits_emergency) - min(visits_emergency)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))


data_features_002 <- read_csv(file = get_path_data_features(directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                                            number_xy = "001",
                                                            number_combination_features = "002",
                                                            name_data_set = "base",
                                                            option = "NONE"))

data_features <- data_features %>% rbind(data_features_002 %>%
                                           mutate(type = "002",
                                                  color = "color_2",
                                                  value_scaled = (fever_first_day - min(fever_first_day)) / (1.05*(max(fever_first_day) - min(fever_first_day)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))


data_features_004 <- read_csv(file = get_path_data_features(directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                                            number_xy = "001",
                                                            number_combination_features = "004",
                                                            name_data_set = "base",
                                                            option = "NONE"))

data_features <- data_features %>% rbind(data_features_004 %>%
                                           mutate(type = "004",
                                                  color = "color_3",
                                                  value_scaled = (sars_cov2_rna - min(sars_cov2_rna)) / (1.05*(max(sars_cov2_rna) - min(sars_cov2_rna)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))


data_features_005 <- read_csv(file = get_path_data_features(directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                                            number_xy = "001",
                                                            number_combination_features = "005",
                                                            name_data_set = "base",
                                                            option = "NONE"))

data_features <- data_features %>% rbind(data_features_005 %>%
                                           mutate(type = "005",
                                                  color = "color_2",
                                                  value_scaled = (high_crp_first_day - min(high_crp_first_day)) / (1.05*(max(high_crp_first_day) - min(high_crp_first_day)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))


data_features_006 <- read_csv(file = get_path_data_features(directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                                            number_xy = "001",
                                                            number_combination_features = "006",
                                                            name_data_set = "base",
                                                            option = "NONE"))

data_features <- data_features %>% rbind(data_features_006 %>%
                                           mutate(type = "006a",
                                                  color = "color_2",
                                                  value_scaled = (icd10_code_001_J12.8 - min(icd10_code_001_J12.8)) / (1.05*(max(icd10_code_001_J12.8) - min(icd10_code_001_J12.8)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))

data_features <- data_features %>% rbind(data_features_006 %>%
                                           mutate(type = "006b",
                                                  color = "color_2",
                                                  value_scaled = (icd10_code_002_I10.90 - min(icd10_code_002_I10.90)) / (1.05*(max(icd10_code_002_I10.90) - min(icd10_code_002_I10.90)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))

data_features <- data_features %>% rbind(data_features_006 %>%
                                           mutate(type = "006c",
                                                  color = "color_2",
                                                  value_scaled = (icd10_code_003_J96.00 - min(icd10_code_003_J96.00)) / (1.05*(max(icd10_code_003_J96.00) - min(icd10_code_003_J96.00)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))

data_features <- data_features %>% rbind(data_features_006 %>%
                                           mutate(type = "006d",
                                                  color = "color_2",
                                                  value_scaled = (icd10_code_004_Z22.8 - min(icd10_code_004_Z22.8)) / (1.05*(max(icd10_code_004_Z22.8) - min(icd10_code_004_Z22.8)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))

data_features <- data_features %>% rbind(data_features_006 %>%
                                           mutate(type = "006e",
                                                  color = "color_2",
                                                  value_scaled = (icd10_code_005_B33.8 - min(icd10_code_005_B33.8)) / (1.05*(max(icd10_code_005_B33.8) - min(icd10_code_005_B33.8)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))


data_features_007 <- read_csv(file = get_path_data_features(directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                                            number_xy = "001",
                                                            number_combination_features = "007",
                                                            name_data_set = "base",
                                                            option = "NONE"))

data_features <- data_features %>% rbind(data_features_007 %>%
                                           mutate(type = "007a",
                                                  color = "color_2",
                                                  value_scaled = (icd10_cat_001_E87 - min(icd10_cat_001_E87)) / (1.05*(max(icd10_cat_001_E87) - min(icd10_cat_001_E87)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))

data_features <- data_features %>% rbind(data_features_007 %>%
                                           mutate(type = "007b",
                                                  color = "color_2",
                                                  value_scaled = (icd10_cat_002_J12 - min(icd10_cat_002_J12)) / (1.05*(max(icd10_cat_002_J12) - min(icd10_cat_002_J12)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))

data_features <- data_features %>% rbind(data_features_007 %>%
                                           mutate(type = "007c",
                                                  color = "color_2",
                                                  value_scaled = (icd10_cat_003_J96 - min(icd10_cat_003_J96)) / (1.05*(max(icd10_cat_003_J96) - min(icd10_cat_003_J96)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))

data_features <- data_features %>% rbind(data_features_007 %>%
                                           mutate(type = "007d",
                                                  color = "color_2",
                                                  value_scaled = (icd10_cat_004_I10 - min(icd10_cat_004_I10)) / (1.05*(max(icd10_cat_004_I10) - min(icd10_cat_004_I10)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))

data_features <- data_features %>% rbind(data_features_007 %>%
                                           mutate(type = "007e",
                                                  color = "color_2",
                                                  value_scaled = (icd10_cat_005_N18 - min(icd10_cat_005_N18)) / (1.05*(max(icd10_cat_005_N18) - min(icd10_cat_005_N18)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))


data_features_008 <- read_csv(file = get_path_data_features(directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                                            number_xy = "001",
                                                            number_combination_features = "008",
                                                            name_data_set = "base",
                                                            option = "NONE"))

data_features <- data_features %>% rbind(data_features_008 %>%
                                           mutate(type = "008a",
                                                  color = "color_2",
                                                  value_scaled = (icd10_chap_001_R - min(icd10_chap_001_R)) / (1.05*(max(icd10_chap_001_R) - min(icd10_chap_001_R)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))

data_features <- data_features %>% rbind(data_features_008 %>%
                                           mutate(type = "008b",
                                                  color = "color_2",
                                                  value_scaled = (icd10_chap_002_I - min(icd10_chap_002_I)) / (1.05*(max(icd10_chap_002_I) - min(icd10_chap_002_I)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))

data_features <- data_features %>% rbind(data_features_008 %>%
                                           mutate(type = "008c",
                                                  color = "color_2",
                                                  value_scaled = (icd10_chap_003_E - min(icd10_chap_003_E)) / (1.05*(max(icd10_chap_003_E) - min(icd10_chap_003_E)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))

data_features <- data_features %>% rbind(data_features_008 %>%
                                           mutate(type = "008d",
                                                  color = "color_2",
                                                  value_scaled = (icd10_chap_004_J - min(icd10_chap_004_J)) / (1.05*(max(icd10_chap_004_J) - min(icd10_chap_004_J)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))

data_features <- data_features %>% rbind(data_features_008 %>%
                                           mutate(type = "008e",
                                                  color = "color_2",
                                                  value_scaled = (icd10_chap_005_Z - min(icd10_chap_005_Z)) / (1.05*(max(icd10_chap_005_Z) - min(icd10_chap_005_Z)))) %>%
                                           dplyr::select(c(type, date_day, value_scaled, color)))



# determine labels for plots of all time series (presented as a grid) ----
data_text_hospital_admissions <- tibble(x = rep(x = ymd("2019-06-15"), times = 6),
                               y = c("all", "age_group_1", "age_group_2", "age_group_3", "age_group_4", "age_group_5"),
                               name = c("Admissions", "Age 0-4", "Age 5-14", "Age 15-29", "Age 30-64", "Age 65+"))


data_text_extra_data <- tibble(x = rep(x = ymd("2019-06-15"), times = 19),
                         y = c("001", "002", "004", "005",
                               "006a", "006b", "006c", "006d", "006e",
                               "007a", "007b", "007c", "007d", "007e",
                               "008a", "008b", "008c", "008d", "008e"),
                         name = c("Emergency", "Fever", "Wastewater", "CRP",
                                  "ICD10 J12.8", "ICD10 I10.90", "ICD10 J96.00", "ICD10 Z22.8", "ICD10 B33.8",
                                  "ICD10 E87", "ICD10 J12", "ICD10 J96", "ICD10 I10", "ICD10 N18",
                                  "ICD10 R", "ICD10 I", "ICD10 E", "ICD10 J", "ICD10 Z"))



# create plot: COVID-19 related hospital admissions ----
plot_grid_hospital_admissions <- ggplot() +
  ggridges::geom_ridgeline(data = data_features %>% filter(color == "color_1"),
                 mapping = aes(x = date_day,
                               y = type,
                               height = value_scaled,
                               group = type,
                               fill = color),
                 color = "black") +
  scale_fill_manual(name = NULL,
                    values = c(color_1 = "firebrick3"),
                    labels = c("COVID-19 hospitalizations", "Further hospital data", "External data"),
                    guide = "none") +
  geom_text(data = data_text_hospital_admissions, mapping = aes(x = x, y = y, label = name), size = 5, nudge_y = 0.5, hjust = 0,) +
  geom_hline(yintercept = "all", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "age_group_1", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "age_group_2", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "age_group_3", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "age_group_4", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "age_group_5", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = ymd("2021-01-01"), color = "black", linewidth = 0.5, linetype = "dotted") +
  geom_vline(xintercept = ymd("2022-01-01"), color = "black", linewidth = 0.5, linetype = "dotted") +
  geom_vline(xintercept = ymd("2023-01-01"), color = "black", linewidth = 0.5, linetype = "dotted") +
  scale_x_date(name = NULL,
               limits = c(ymd("2019-06-01"), ymd("2023-07-01")),
               breaks = c(ymd("2020-01-01"), ymd("2020-04-01"), ymd("2020-07-01"), ymd("2020-10-01"),
                          ymd("2021-01-01"), ymd("2021-04-01"), ymd("2021-07-01"), ymd("2021-10-01"),
                          ymd("2022-01-01"), ymd("2022-04-01"), ymd("2022-07-01"), ymd("2022-10-01"),
                          ymd("2023-01-01"), ymd("2023-04-01"), ymd("2023-07-01")),
               labels = c("Jan \n2020", "Apr", "Jul", "Oct",
                          "Jan \n2021", "Apr", "Jul", "Oct",
                          "Jan \n2022", "Apr", "Jul", "Oct",
                          "Jan \n2023", "Apr", "Jul"),
               expand = c(0, 0)) +
  scale_y_discrete(name = NULL,
                   limits = c("age_group_5", "age_group_4", "age_group_3", "age_group_2", "age_group_1", "all"),
                   expand = c(0, 0)) +
  theme_bw() +
  theme(axis.title.x = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.ticks.x = element_line(color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "inches"),
        legend.position = "bottom")

ggsave(plot = plot_grid_hospital_admissions,
       filename = paste0(Directory_Plots, Subdirectory_Plots_Data, "plot_grid_hospital_admissions.pdf"),
       width = 7.3,
       height = 8.7, units = c("in"), bg = "white")



# create plot: extra data (emergency visits, icd10 codes, ...) ----
plot_grid_extra_data <- ggplot() +
  ggridges::geom_ridgeline(data = data_features,
                 mapping = aes(x = date_day,
                               y = type,
                               height = value_scaled,
                               group = type,
                               fill = color),
                 color = "black") +
  scale_fill_manual(name = NULL,
                    values = c(color_2 = "steelblue3", color_3 = "forestgreen"),
                    labels = c("Hospital data", "External data")) +
  geom_text(data = data_text_extra_data, mapping = aes(x = x, y = y, label = name), size = 5, nudge_y = 0.5, hjust = 0) +
  geom_hline(yintercept = "001", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "002", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "004", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "005", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "006a", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "006b", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "006c", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "006d", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "006e", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "007a", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "007b", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "007c", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "007d", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "007e", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "008a", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "008b", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "008c", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "008d", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "008e", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = ymd("2021-01-01"), color = "black", linewidth = 0.5, linetype = "dotted") +
  geom_vline(xintercept = ymd("2022-01-01"), color = "black", linewidth = 0.5, linetype = "dotted") +
  geom_vline(xintercept = ymd("2023-01-01"), color = "black", linewidth = 0.5, linetype = "dotted") +
  scale_x_date(name = NULL,
               limits = c(ymd("2019-06-01"), ymd("2023-07-01")),
               breaks = c(ymd("2020-01-01"), ymd("2020-04-01"), ymd("2020-07-01"), ymd("2020-10-01"),
                          ymd("2021-01-01"), ymd("2021-04-01"), ymd("2021-07-01"), ymd("2021-10-01"),
                          ymd("2022-01-01"), ymd("2022-04-01"), ymd("2022-07-01"), ymd("2022-10-01"),
                          ymd("2023-01-01"), ymd("2023-04-01"), ymd("2023-07-01")),
               labels = c("Jan \n2020", "Apr", "Jul", "Oct",
                          "Jan \n2021", "Apr", "Jul", "Oct",
                          "Jan \n2022", "Apr", "Jul", "Oct",
                          "Jan \n2023", "Apr", "Jul"),
               expand = c(0, 0)) +
  scale_y_discrete(name = NULL,
                   limits = c("004",
                              "005", "002",
                              "006e", "006d", "006c", "006b", "006a",
                              "007e", "007d", "007c", "007b", "007a",
                              "008e", "008d", "008c", "008b", "008a",
                              "001"),
                   expand = c(0, 0)) +
  theme_bw() +
  theme(axis.title.x = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.ticks.x = element_line(color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "inches"),
        legend.position = "bottom")

ggsave(plot = plot_grid_extra_data,
       filename = paste0(Directory_Plots, Subdirectory_Plots_Data, "plot_grid_extra_data.pdf"),
       width = 8.3,
       height = 11.7, units = c("in"), bg = "white")



# create plot: COVID-19 related hospital admissions and extra data (emergency visits, icd10 codes, ...) ----
plot_grid_hospital_admissions_extra_data <- ggplot() +
  ggridges::geom_ridgeline(data = data_features,
                 mapping = aes(x = date_day,
                               y = type,
                               height = value_scaled,
                               group = type,
                               fill = color),
                 color = "black") +
  scale_fill_manual(name = NULL,
                    values = c(color_1 = "firebrick3", color_2 = "steelblue3", color_3 = "forestgreen"),
                    labels = c("COVID-19-related hospital admissions", "Further hospital data", "External data")) +
  geom_text(data = bind_rows(data_text_hospital_admissions, data_text_extra_data), mapping = aes(x = x, y = y, label = name), size = 5, nudge_y = 0.5, hjust = 0) +
  geom_hline(yintercept = "all", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "age_group_1", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "age_group_2", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "age_group_3", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "age_group_4", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "age_group_5", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "001", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "002", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "004", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "005", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "006a", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "006b", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "006c", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "006d", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "006e", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "007a", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "007b", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "007c", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "007d", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "007e", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "008a", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "008b", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "008c", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "008d", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = "008e", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = ymd("2021-01-01"), color = "black", linewidth = 0.5, linetype = "dotted") +
  geom_vline(xintercept = ymd("2022-01-01"), color = "black", linewidth = 0.5, linetype = "dotted") +
  geom_vline(xintercept = ymd("2023-01-01"), color = "black", linewidth = 0.5, linetype = "dotted") +
  scale_x_date(name = NULL,
               limits = c(ymd("2019-06-01"), ymd("2023-07-01")),
               breaks = c(ymd("2020-01-01"), ymd("2020-04-01"), ymd("2020-07-01"), ymd("2020-10-01"),
                          ymd("2021-01-01"), ymd("2021-04-01"), ymd("2021-07-01"), ymd("2021-10-01"),
                          ymd("2022-01-01"), ymd("2022-04-01"), ymd("2022-07-01"), ymd("2022-10-01"),
                          ymd("2023-01-01"), ymd("2023-04-01"), ymd("2023-07-01")),
               labels = c("Jan \n2020", "Apr", "Jul", "Oct",
                          "Jan \n2021", "Apr", "Jul", "Oct",
                          "Jan \n2022", "Apr", "Jul", "Oct",
                          "Jan \n2023", "Apr", "Jul"),
               expand = c(0, 0)) +
  scale_y_discrete(name = NULL,
                   limits = c("004",
                              "005", "002",
                              "006e", "006d", "006c", "006b", "006a",
                              "007e", "007d", "007c", "007b", "007a",
                              "008e", "008d", "008c", "008b", "008a",
                              "001",
                              "age_group_5", "age_group_4", "age_group_3", "age_group_2", "age_group_1", "all"),
                   expand = c(0, 0)) +
  theme_bw() +
  theme(axis.title.x = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.ticks.x = element_line(color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "inches"),
        legend.position = "bottom")

ggsave(plot = plot_grid_hospital_admissions_extra_data,
       filename = paste0(Directory_Plots, Subdirectory_Plots_Data, "plot_grid_hospital_admissions_extra_data.pdf"),
       width = 7.3,
       height = 8.7, units = c("in"), bg = "white")

ggsave(plot = plot_grid_hospital_admissions_extra_data,
       filename = paste0(Directory_Plots, Subdirectory_Plots_Manuscript, "Figure_2.pdf"),
       width = 7.3,
       height = 8.7, units = c("in"), bg = "white")


