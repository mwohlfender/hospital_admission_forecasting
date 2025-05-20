
# number of combination of time series X and target variable Y
number_xy <- "001"

# primary number of data set (group of features)
number_combination_features <- "006"



# load data ----
table_parameters_kNp <- read_csv(file = paste0(Directory_Parameters, Path_Table_Parameters_kNp))

table_dates_train_test_detailed <- read_csv(file = paste0(Directory_Parameters, Path_Table_Dates_Train_Test_Detailed))

table_dates_train_test_overview <- read_csv(file = paste0(Directory_Parameters, Path_Table_Dates_Train_Test_Overview))

table_days_icd_u07_1_20200101_20230630 <- read_csv(file = paste0(Directory_Data, Path_Table_Days_ICD_U07_1_20200101_20230630_Anonymized))

table_days_icd_codes <- read_csv(file = paste0(Directory_Data, Path_Table_Days_ICD_Freq_Codes_Cat_Chap_Covid_20200101_20230630_Anonymized)) %>%
  dplyr::select(c("date_day"), starts_with("icd10_code_"))



# data set "base": basic configuration (no stratification) ----

# name of data set (determined by stratification)
name_data_set <- "base"

# list of date groups for which data files shall be created
groups_dates <- c("002", "006")

# filter `table_parameters_kNp`
table_parameters_kNp_filtered <- table_parameters_kNp

# filter `table_dates_train_test_detailed`
table_dates_train_test_detailed_filtered <- table_dates_train_test_detailed %>% filter(group %in% groups_dates)

# first day (day d - `p`) of which data is used in either features matrix or target variable vector 
min_date <- table_dates_train_test_overview %>% filter(group %in% groups_dates) %>% pull(date_min) %>% min()

# last day (day d + `k` + `N` - 1) of which data is used in either features matrix or target variable vector 
max_date <- table_dates_train_test_overview %>% filter(group %in% groups_dates) %>% pull(date_max) %>% max()

# sequence of days between `min_date` and `max_date`
sequence_days <- seq(min_date, max_date, "day")


# join `sequence_days` and `table_days_icd_u07_1_20200101_20230630`
data_features_006_base <- tibble(date_day = sequence_days) %>%
  left_join(table_days_icd_u07_1_20200101_20230630 %>% dplyr::select(c("date_day", "icd10_code_U07_1")), by = "date_day") %>%
  mutate_if(is.numeric , replace_na, replace = 0) %>%
  dplyr::rename(c("x" = "icd10_code_U07_1"))

# join `data_features_006_base` and `table_days_icd_codes`
data_features_006_base <- data_features_006_base %>%
  left_join(table_days_icd_codes, by = "date_day")


# create data sets and store them
data_preparation_series_001(data_features = data_features_006_base,
                            number_combination_features = number_combination_features,
                            name_data_set = name_data_set,
                            table_parameters_kNp = table_parameters_kNp_filtered,
                            table_dates_train_test = table_dates_train_test_detailed_filtered,
                            remove_x = FALSE,
                            overwrite = FALSE)


# `data_features_006_base`: add variable y (= y_{0,N} for all N present in `table_parameters_kNp`)
# y_{0,N}(t) = x(t) + x(t+1) + x(t+2) + ... + x(t+N-1)
list_N <- table_parameters_kNp %>% pull(N) %>% unique()

for (ii in 1:length(list_N)) {
  
  name_new_column <- paste0("y_", str_pad(list_N[ii], 2, pad = "0"))
  
  data_features_006_base <- data_features_006_base %>%
    dplyr::mutate(y=x) %>%
    timetk::tk_augment_leads(.value=y,
                             .lags=-seq(from = 0, to = list_N[ii] - 1, by = 1)) %>%
    dplyr::rename_with(.cols=any_of("y_lag0"),
                       .fn=function(x) {if ("y_lag0" %in% names(.)) {return("y_lead0")}
                         else {return(character())}}) %>%
    dplyr::mutate(y = rowSums(dplyr::select(., starts_with("y_lead")))) %>%
    dplyr::select(-starts_with("y_lead")) %>%
    dplyr::rename_with(~c(name_new_column), c("y"))
  
}


# store `data_features_006_base`
path_data_features_006_base <- get_path_data_features(directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                                      number_xy = number_xy,
                                                      number_combination_features = number_combination_features,
                                                      name_data_set = name_data_set)

write_csv(x = data_features_006_base,
          file = path_data_features_006_base)


