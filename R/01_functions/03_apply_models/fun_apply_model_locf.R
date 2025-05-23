
# function `apply_model_locf`: apply last observation carried forward model



apply_model_locf <- function(number_xy,
                             number_grid_combinations_kNp_dates_train_test,
                             directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                             directory_results = Directory_Results,
                             do_new = Bool_Apply_Models_Do_New) {
  
  path_results_summary_locf <- get_path_results_processed_summary(directory_results = directory_results,
                                                                  type_model = "locf",
                                                                  number_xy = number_xy,
                                                                  number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test)
  
  path_results_forecasts_locf_train <- get_path_results_forecasts(directory_results = directory_results,
                                                                  type_model = "locf",
                                                                  number_xy = number_xy,
                                                                  number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test,
                                                                  type_period = "train")
  
  path_results_forecasts_locf_test <- get_path_results_forecasts(directory_results = directory_results,
                                                                 type_model = "locf",
                                                                 number_xy = number_xy,
                                                                 number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test,
                                                                 type_period = "test")
  
  if (!(file.exists(path_results_summary_locf)) | !(file.exists(path_results_forecasts_locf_train)) | !(file.exists(path_results_forecasts_locf_test)) | do_new) {
    
    # read data: days and target variable
    data_features_000_base <- read_csv(file = get_path_data_features(directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                                                     number_xy = number_xy,
                                                                     number_combination_features = "000",
                                                                     name_data_set = "base"))
    
    table_parameters_kNp <- read_csv(file = Path_Table_Parameters_kNp)
    
    table_dates_train_test_detailed <- read_csv(file = Path_Table_Dates_Train_Test_Detailed)
    
    grid_combinations_kNp_dates_train_test <- read_csv(file = paste0(Path_Grid_Combinations_kNp_Dates_Train_Test,
                                                                     number_grid_combinations_kNp_dates_train_test, ".csv"))
    
    
    # join `grid_combinations_kNp_dates_train_test` and `table_parameters_kNp`
    results_summary_locf <- grid_combinations_kNp_dates_train_test %>%
      left_join(table_parameters_kNp %>%
                  dplyr::rename(c("number_combination_kNp" = "number")), by = "number_combination_kNp") %>%
      mutate(number_xy = number_xy) %>%
      dplyr::select(c("number_xy", "number_combination_kNp", "k", "N", "p", "group_dates_train_test", "number_dates_train_test")) %>%
      mutate(rmse_locf_train = 0,
             rmse_locf_test = 0)
    
    
    list_N <- results_summary_locf %>% pull(N) %>% unique()
    
    data_features_000_base <- data_features_000_base %>%
      timetk::tk_augment_leads(.value = paste0("y_", str_pad(list_N, 2, pad = "0")),
                               .lags = list_N,
                               .names = paste0("y_", str_pad(list_N, 2, pad = "0"), "_locf")) %>%
      mutate_if(is.numeric, replace_na, replace = 0)
    
    forecasts_locf_train <- NULL
    forecasts_locf_test <- NULL
    
    for (ii in 1:nrow(results_summary_locf)) {
      
      target_train_ii <- read_csv(file = get_path_target_train(directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                                               number_xy = number_xy,
                                                               number_combination_features = "000",
                                                               name_data_set = "base",
                                                               number_combination_kNp = results_summary_locf$number_combination_kNp[ii],
                                                               group_dates_train_test = results_summary_locf$group_dates_train_test[ii],
                                                               number_dates_train_test = results_summary_locf$number_dates_train_test[ii]))
      
      forecasts_locf_train_ii <- target_train_ii %>%
        dplyr::select(c("date_day", "date_day_shift_k")) %>%
        left_join(data_features_000_base, by = "date_day") %>%
        dplyr::select(c("date_day", "date_day_shift_k", y = paste0("y_", str_pad(results_summary_locf$N[ii], 2, pad = "0"), "_locf"))) %>%
        mutate(type_model = "locf",
               number_xy = number_xy,
               number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test,
               number_combination_kNp = results_summary_locf$number_combination_kNp[ii],
               group_dates_train_test = results_summary_locf$group_dates_train_test[ii],
               number_dates_train_test = results_summary_locf$number_dates_train_test[ii],
               k = results_summary_locf$k[ii],
               N = results_summary_locf$N[ii],
               p = results_summary_locf$p[ii])
      
      forecasts_locf_train <- forecasts_locf_train %>% bind_rows(forecasts_locf_train_ii)
      
      results_summary_locf$rmse_locf_train[ii] <- Metrics::rmse(actual = target_train_ii$y, predicted = forecasts_locf_train_ii$y)
      
      
      target_test_ii <-  read_csv(file = get_path_target_test(directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                                              number_xy = number_xy,
                                                              number_combination_features = "000",
                                                              name_data_set = "base",
                                                              number_combination_kNp = results_summary_locf$number_combination_kNp[ii],
                                                              group_dates_train_test = results_summary_locf$group_dates_train_test[ii],
                                                              number_dates_train_test = results_summary_locf$number_dates_train_test[ii]))
      
      forecasts_locf_test_ii <- target_test_ii %>%
        dplyr::select(c("date_day", "date_day_shift_k")) %>%
        left_join(data_features_000_base, by = "date_day") %>%
        dplyr::select(c("date_day", "date_day_shift_k", y = paste0("y_", str_pad(results_summary_locf$N[ii], 2, pad = "0"), "_locf"))) %>%
        mutate(type_model = "locf",
               number_xy = number_xy,
               number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test,
               number_combination_kNp = results_summary_locf$number_combination_kNp[ii],
               group_dates_train_test = results_summary_locf$group_dates_train_test[ii],
               number_dates_train_test = results_summary_locf$number_dates_train_test[ii],
               k = results_summary_locf$k[ii],
               N = results_summary_locf$N[ii],
               p = results_summary_locf$p[ii])
      
      forecasts_locf_test <- forecasts_locf_test %>% bind_rows(forecasts_locf_test_ii)
      
      results_summary_locf$rmse_locf_test[ii] <- Metrics::rmse(actual = target_test_ii$y, predicted = forecasts_locf_test_ii$y)
      
    }
    
    write_csv(x = results_summary_locf,
              file = path_results_summary_locf)
    
    write_csv(x = forecasts_locf_train,
              file = path_results_forecasts_locf_train)
    
    write_csv(x = forecasts_locf_test,
              file = path_results_forecasts_locf_test)

  }
  
}


