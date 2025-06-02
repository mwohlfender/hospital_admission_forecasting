



# read table of parameters (k, N and p)
table_parameters_kNp <- read_csv(file = paste0(Directory_Parameters, Path_Table_Parameters_kNp)) %>%
  dplyr::rename(c("number_combination_kNp" = "number"))

# read overview table of dates (date_min_train, date_max_train, date_min_test and date_max_test)
table_dates_train_test_overview <- read_csv(file = paste0(Directory_Parameters, Path_Table_Dates_Train_Test_Overview)) %>%
  dplyr::rename(c("group_dates_train_test" = "group"))


# read all forecasts (all models, feature sets, ...)
results_all_models_test_0 <- NULL

paths_results_all_models_test <- list.files(path = paste0(Directory_Ubelix, "000_A_process_results"),
                                            pattern = glob2rx(paste0("results_forecast_*_XY_001_*_PD_*_test.csv")), recursive = TRUE, full.names = TRUE)

for (path_result_model in paths_results_all_models_test) {
  
  result_model_ii <- read_csv(path_result_model)
  
  results_all_models_test_0 <- bind_rows(results_all_models_test_0, result_model_ii)
  
}


# determine values in column `number_grid_combinations_kNp_dates_train_test` of `results_all_models_test_0`
# (combinations of parameters k, N and p and groups of train-test splits)
list_numbers_grid_combinations_kNp_dates_train_test <- results_all_models_test_0 %>%
  pull(number_grid_combinations_kNp_dates_train_test) %>%
  unique() %>%
  sort()


# read grids of combinations of parameters k, N and p and groups of train-test splits
grids_combinations_kNp_dates_train_test <- NULL

for (number_grid_combinations_kNp_dates_train_test in list_numbers_grid_combinations_kNp_dates_train_test) {
  
  grid_combinations_kNp_dates_train_test_temp <- read_csv(file = paste0(Directory_Parameters, Path_Grid_Combinations_kNp_Dates_Train_Test,
                                                                        number_grid_combinations_kNp_dates_train_test, ".csv"))
  
  grid_combinations_kNp_dates_train_test_temp <- grid_combinations_kNp_dates_train_test_temp %>%
    mutate(number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test) %>%
    left_join(table_parameters_kNp, by = "number_combination_kNp")
  
  grids_combinations_kNp_dates_train_test <- bind_rows(grids_combinations_kNp_dates_train_test, grid_combinations_kNp_dates_train_test_temp)
  
}


# determine overview tables of forecasting setups (defined by parameters k, N and p)
# for each combinations of parameters k, N and p and groups of train-test splits determine:
# (a) number of values for parameter k
# (b) minimal value for parameter k
# (c) maximal value for parameter k
# (d) number of values for parameter N
# (e) minimal value for parameter N
# (f) maximal value for parameter N
# (g) number of values for parameter p
# (h) minimal value for parameter p
# (i) maximal value for parameter p
table_overview_forecasting_setups <- grids_combinations_kNp_dates_train_test %>%
  group_by(number_grid_combinations_kNp_dates_train_test) %>%
  summarize(n_k = length(unique(k)),
            min_k = min(k),
            max_k = max(k),
            n_N = length(unique(N)),
            min_N = min(N),
            max_N = max(N),
            n_p = length(unique(p)),
            min_p = min(p),
            max_p = max(p))


# `results_all_models_test_0`:
# (a) add row `date_max` (last day of study period) from `table_dates_train_test_overview`
# (b) add row `max_k` (maximal value for parameter k) from `table_overview_forecasting_setups`
# (c) remove rows: in the column `date_day` we only want days starting from which the following `max_k`+`N` days are part of the study period
results_all_models_test <- results_all_models_test_0 %>%
  left_join(table_dates_train_test_overview %>%
              dplyr::select(c("group_dates_train_test", "date_max")), by = "group_dates_train_test") %>%
  left_join(table_overview_forecasting_setups %>%
              dplyr::select(c("number_grid_combinations_kNp_dates_train_test", "max_k")), by = "number_grid_combinations_kNp_dates_train_test") %>%
  filter(date_day <= date_max-(max_k+N)+1) %>%
  dplyr::select(-c("date_max", "max_k"))


# load data of COVID-19 hospitalizations
path_data_base <- get_path_features_target(number_xy = "001",
                                           number_combination_features = "000",
                                           name_data_set = "base",
                                           option_path = "long_directory_filename")

data_base <- read_csv(file = path_data_base) %>%
  na.omit()



for (number_grid_combinations_kNp_dates_train_test_0 in list_numbers_grid_combinations_kNp_dates_train_test) {
  
  # filter `results_all_models_test` to rows corresponding to the combination of parameters k, N and p and groups of train-test splits `number_grid_combinations_kNp_dates_train_test_0`
  results_all_models_test_filtered <- results_all_models_test %>%
    filter(number_grid_combinations_kNp_dates_train_test == number_grid_combinations_kNp_dates_train_test_0)
  
  # determine values for parameter k appearing in `results_all_models_test_filtered`
  list_k_temp <- results_all_models_test_filtered %>% pull(k) %>% unique() %>% sort()
  
  # determine value for parameter N appearing in `results_all_models_test_filtered`
  N_temp <- results_all_models_test_filtered %>% pull(N) %>% unique()
  
  # summarize `results_all_models_test_filtered` to determine the day 0 and parameters defining the forecasting setup
  results_all_models_test_filtered_summarized <- results_all_models_test_filtered %>%
    group_by(date_day, type_model, number_xy, number_combination_features, name_data_set, group_dates_train_test, number_hyp_par_grid, N, p) %>%
    summarize() %>%
    ungroup()
  
  
  # iteratively add columns to `results_all_models_test_filtered_summarized`
  # (a) value of column `date_day` shifted by k days for all values for parameter k appearing in `results_all_models_test_filtered`
  # (b) forecast k days ahead for all values for parameter k appearing in `results_all_models_test_filtered`
  # (c) number of combination of parameters k. N and p with which the forecasts were obtained
  # (d) true value k days ahead for all values for parameter k appearing in `results_all_models_test_filtered`
  # (e) RMSE between forecasts up to k days ahead and true values for all but the smallest value for parameter k appearing in `results_all_models_test_filtered`
  for (k_temp in list_k_temp) {
    
    results_all_models_test_filtered_k_temp <- results_all_models_test_filtered %>%
      filter(k == k_temp)
    
    results_all_models_test_filtered_summarized <- results_all_models_test_filtered_summarized %>%
      left_join(results_all_models_test_filtered_k_temp %>%
                  dplyr::select(-c("number_grid_combinations_kNp_dates_train_test", "number_dates_train_test", "k")),
                by = c("date_day", "type_model", "number_xy", "number_combination_features", "name_data_set",
                       "group_dates_train_test", "number_hyp_par_grid", "N", "p")) %>%
      left_join(data_base %>%
                  dplyr::select(c("date_day", starts_with(paste0("y_", str_pad(N_temp, 2, pad = "0"))))) %>%
                  dplyr::rename_with(~paste0("y_true_", str_pad(k_temp, ceiling(log(x = max(list_k_temp)+1, base = 10)), pad = "0")),
                                     paste0("y_", str_pad(N_temp, 2, pad = "0"))),
                by = c("date_day_shift_k" = "date_day")) %>%
      dplyr::rename_with(~c(paste0("date_day_shift_k_", str_pad(k_temp, ceiling(log(x = max(list_k_temp)+1, base = 10)), pad = "0")),
                            paste0("y_forecast_", str_pad(k_temp, ceiling(log(x = max(list_k_temp)+1, base = 10)), pad = "0")),
                            paste0("number_combination_kNp_", str_pad(k_temp, ceiling(log(x = max(list_k_temp)+1, base = 10)), pad = "0"))),
                         c(date_day_shift_k, y, number_combination_kNp))
    
    if (k_temp != list_k_temp[1]) {
      
      results_all_models_test_filtered_summarized <- results_all_models_test_filtered_summarized %>%
        rowwise() %>%
        mutate(rmse = Metrics::rmse(actual = c_across(starts_with("y_true_")),
                                    predicted = c_across(starts_with("y_forecast_")))) %>%
        dplyr::rename_with(~c(paste0("rmse_", str_pad(k_temp, 2, pad = "0"))), c(rmse))
      
    }
    
  }
  
  results_all_models_test_filtered_rmse <- results_all_models_test_filtered %>%
    left_join(results_all_models_test_filtered_summarized %>%
                dplyr::select(c("date_day", "type_model", "number_xy", "number_combination_features", "name_data_set",
                                "group_dates_train_test", "number_hyp_par_grid", "p", starts_with("rmse_"))),
              by = c("date_day", "type_model", "number_xy", "number_combination_features", "name_data_set",
                     "group_dates_train_test", "number_hyp_par_grid", "p"))
  
  path_results_all_models_test_filtered_rmse <- get_path_results_joined_forecasts(directory_results = paste0(Directory_Results, "all/"),
                                                                                  number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test_0)
  
  write_csv(x = results_all_models_test_filtered_rmse,
            file = path_results_all_models_test_filtered_rmse)
  
}


