

# create overview table of feature sets
# We assign the following information to each feature set we use in our analysis:
# `number_xy`: number of combination of type of features and target variable
# `number_combination_features`: number of feature set
# `name_data_set`: name of feature set (e.g., indicating stratification)
# `label_dataset`: label that appears on figures and tables
# Furthermore, we assign a color to each feature set and determine
# whether text appearing on top of this color shall be in black or in white.

# define overview table of feature sets
table_datasets <- tribble(~number_xy, ~number_combination_features, ~name_data_set, ~label_dataset, ~label_dataset_linebreak,
                          "001", "000", "base", "A (COVID-19-related hospital admissions)", "A (COVID-19-related \nhospital admissions)",
                          "001", "000",  "age", "B (A + COVID-19-related hospital admissions by age group)", "B (A + COVID-19-related \nhospital admissions by age group)",
                          "001", "001", "base", "C (A + emergency)", "C (A + emergency)",
                          "001", "002", "base", "G (A + fever)", "G (A + fever)",
                          "001", "003", "base", "K (A + wastewater)", "K (A + wastewater)",
                          "001", "004", "base", "J (Wastewater)", "J (Wastewater)",
                          "001", "005", "base", "H (A + CRP)", "H (A + CRP)",
                          "001", "006", "base", "F (A + ICD10 codes)", "F (A + ICD10 codes)",
                          "001", "007", "base", "E (A + ICD10 categories)", "E (A + ICD10 categories)",
                          "001", "008", "base", "D (A + ICD10 chapters)", "D (A + ICD10 chapters)",
                          "001", "009", "base", "L (A + emergency + ICD10 codes + fever + CRP + wastewater)", "L (A + emergency + ICD10 codes + \nfever + CRP + wastewater)",
                          "001", "010", "base", "I (A + emergency + ICD10 codes + fever + CRP)", "I (A + emergency + ICD10 codes + \nfever + CRP)")


# assign colors
table_datasets <- table_datasets %>%
  arrange(label_dataset) %>%
  mutate(color_dataset = scales::viridis_pal(option = "F", direction = -1)(nrow(table_datasets)),
         color_dataset_font = c(rep(x = "#000000", times = custom_round_down(nrow(table_datasets)/2, 1)),
                                rep(x = "#FFFFFF", times = custom_round_up(nrow(table_datasets)/2, 1))))


# save `table_datasets` as csv file
write_csv(x = table_datasets,
          file = paste0(Directory_Parameters, Path_Overview_Table_Datasets))


