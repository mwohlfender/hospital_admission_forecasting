

process_results_rnn <- function(number_job_array,
                                directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                directory_parameters = Directory_Parameters,
                                directory_results_raw = Directory_Results,
                                directory_results_processed = Directory_Results,
                                option_paths_data = "NONE",
                                option_paths_results_raw = "long_directory",
                                do_new = FALSE) {
  
  # read parameter grid of job array
  grid_combinations_data_kNp_dates_hyp <- read_csv(file =  paste0(Directory_Parameters, Path_Grid_Combinations_Data_kNp_Dates_Hyp, number_job_array, ".csv"))
  
  
  # determine parameters
  number_xy <- grid_combinations_data_kNp_dates_hyp %>%
    pull(number_xy) %>%
    unique() %>%
    first()
  
  number_combination_features <- grid_combinations_data_kNp_dates_hyp %>%
    pull(number_combination_features) %>%
    unique() %>%
    first()
  
  name_data_set <- grid_combinations_data_kNp_dates_hyp %>%
    pull(name_data_set) %>%
    unique() %>%
    first()
  
  number_grid_combinations_kNp_dates_train_test <- grid_combinations_data_kNp_dates_hyp %>%
    pull(number_grid_combinations_kNp_dates_train_test) %>%
    unique() %>%
    first()
  
  number_hyp_par_grid <- grid_combinations_data_kNp_dates_hyp %>%
    pull(number_hyp_par_grid) %>%
    unique() %>%
    first()
  
  
  # define directories where output shall be stored
  directory_results_processed_general <- get_path_results_raw(type_model = "rnn",
                                                              number_xy = number_xy,
                                                              number_combination_features = number_combination_features,
                                                              name_data_set = name_data_set,
                                                              number_combination_kNp = "NONE",
                                                              group_dates_train_test = "NONE",
                                                              number_dates_train_test = "NONE",
                                                              number_hyp_par_grid = "NONE",
                                                              number_hyp_par_subgrid = "NONE",
                                                              directory_results = directory_results_processed,
                                                              type_period = "NONE",
                                                              option = "long_directory")
  
  directory_results_processed_train <- get_path_results_raw(type_model = "rnn",
                                                            number_xy = number_xy,
                                                            number_combination_features = number_combination_features,
                                                            name_data_set = name_data_set,
                                                            number_combination_kNp = "NONE",
                                                            group_dates_train_test = "NONE",
                                                            number_dates_train_test = "NONE",
                                                            number_hyp_par_grid = "NONE",
                                                            number_hyp_par_subgrid = "NONE",
                                                            directory_results = directory_results_processed,
                                                            type_period = "train",
                                                            option = "long_directory")
  
  directory_results_processed_test <- get_path_results_raw(type_model = "rnn",
                                                           number_xy = number_xy,
                                                           number_combination_features = number_combination_features,
                                                           name_data_set = name_data_set,
                                                           number_combination_kNp = "NONE",
                                                           group_dates_train_test = "NONE",
                                                           number_dates_train_test = "NONE",
                                                           number_hyp_par_grid = "NONE",
                                                           number_hyp_par_subgrid = "NONE",
                                                           directory_results = directory_results_processed,
                                                           type_period = "test",
                                                           option = "long_directory")
  
  # create directories to store output (if they do not already exist)
  if (!(file.exists(directory_results_processed_general))) {
    
    dir.create(file.path(directory_results_processed_general), recursive = TRUE)
    
  }
  
  if (!(file.exists(directory_results_processed_train))) {
    
    dir.create(file.path(directory_results_processed_train), recursive = TRUE)
    
  }
  
  if (!(file.exists(directory_results_processed_test))) {
    
    dir.create(file.path(directory_results_processed_test), recursive = TRUE)
    
  }
  
  
  # determine paths of output
  path_results_rnn_forecast_train <- get_path_results_forecasts(directory_results = directory_results_processed,
                                                                type_model = "rnn",
                                                                number_xy = number_xy,
                                                                number_combination_features = number_combination_features,
                                                                name_data_set = name_data_set,
                                                                number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test,
                                                                number_hyp_par_grid = number_hyp_par_grid,
                                                                type_period = "train")
  
  path_results_rnn_forecast_test <- get_path_results_forecasts(directory_results = directory_results_processed,
                                                               type_model = "rnn",
                                                               number_xy = number_xy,
                                                               number_combination_features = number_combination_features,
                                                               name_data_set = name_data_set,
                                                               number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test,
                                                               number_hyp_par_grid = number_hyp_par_grid,
                                                               type_period = "test")
  
  path_results_rnn_detailed <- get_path_results_processed_detailed(directory_results = directory_results_processed,
                                                                   type_model = "rnn",
                                                                   number_xy = number_xy,
                                                                   number_combination_features = number_combination_features,
                                                                   name_data_set = name_data_set,
                                                                   number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test,
                                                                   number_hyp_par_grid = number_hyp_par_grid)
  
  path_results_rnn_summary <- get_path_results_processed_summary(directory_results = directory_results_processed,
                                                                 type_model = "rnn",
                                                                 number_xy = number_xy,
                                                                 number_combination_features = number_combination_features,
                                                                 name_data_set = name_data_set,
                                                                 number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test,
                                                                 number_hyp_par_grid = number_hyp_par_grid)
  
  
  if (!(file.exists(path_results_rnn_forecast_train)) | !(file.exists(path_results_rnn_forecast_test)) |
      !(file.exists(path_results_rnn_detailed)) | !(file.exists(path_results_rnn_summary)) | do_new) {
    
    # read parameter grids
    grid_combinations_kNp_dates_train_test <- read_csv(file = paste0(directory_parameters,
                                                                     Path_Grid_Combinations_kNp_Dates_Train_Test, number_grid_combinations_kNp_dates_train_test, ".csv"))
    
    table_parameters_kNp <- read_csv(file = paste0(directory_parameters, Path_Table_Parameters_kNp)) %>%
      dplyr::rename(c("number_combination_kNp" = "number"))
    
    
    # files containing results
    directory_results_raw_model <- get_path_results_raw(type_model = "rnn",
                                                  number_xy = number_xy,
                                                  number_combination_features = number_combination_features,
                                                  name_data_set = name_data_set,
                                                  number_combination_kNp = "NONE",
                                                  group_dates_train_test = "NONE",
                                                  number_dates_train_test = "NONE",
                                                  number_hyp_par_grid = "NONE",
                                                  number_hyp_par_subgrid = "NONE",
                                                  directory_results = directory_results_raw,
                                                  type_period = "NONE",
                                                  option = option_paths_results_raw )
    
    files_raw_results <- dir(path = directory_results_raw_model)
    
    
    results_rnn_detailed <- NULL
    results_rnn_summary <- NULL
    results_rnn_forecast_train <- NULL
    results_rnn_forecast_test <- NULL
    
    
    for (ii in 1:nrow(grid_combinations_kNp_dates_train_test)) {
      
      print(paste0("ii = ", ii))
      
      pattern_file_names_results_ii <- get_path_results_raw(type_model = "rnn",
                                                            number_xy = number_xy,
                                                            number_combination_features = number_combination_features,
                                                            name_data_set = name_data_set,
                                                            number_combination_kNp = grid_combinations_kNp_dates_train_test$number_combination_kNp[ii],
                                                            group_dates_train_test = grid_combinations_kNp_dates_train_test$group_dates_train_test[ii],
                                                            number_dates_train_test = grid_combinations_kNp_dates_train_test$number_dates_train_test[ii],
                                                            number_hyp_par_grid = number_hyp_par_grid,
                                                            directory_results = directory_results_raw,
                                                            ending = "",
                                                            option = "filename")
      
      file_names_results_ii <- files_raw_results[grepl(pattern = pattern_file_names_results_ii,
                                                       x = files_raw_results, fixed = TRUE)]
      
      if (length(file_names_results_ii) > 0) {
        
        path_target_train_ii <- get_path_target_train(directory_data = directory_data,
                                                      number_xy = number_xy,
                                                      number_combination_features = number_combination_features,
                                                      name_data_set = name_data_set,
                                                      number_combination_kNp = grid_combinations_kNp_dates_train_test$number_combination_kNp[ii],
                                                      group_dates_train_test = grid_combinations_kNp_dates_train_test$group_dates_train_test[ii],
                                                      number_dates_train_test = grid_combinations_kNp_dates_train_test$number_dates_train_test[ii],
                                                      option = option_paths_data)
        
        target_train_ii <- read_csv(file = path_target_train_ii)
        
        path_target_test_ii <- get_path_target_test(directory_data = directory_data,
                                                    number_xy = number_xy,
                                                    number_combination_features = number_combination_features,
                                                    name_data_set = name_data_set,
                                                    number_combination_kNp = grid_combinations_kNp_dates_train_test$number_combination_kNp[ii],
                                                    group_dates_train_test = grid_combinations_kNp_dates_train_test$group_dates_train_test[ii],
                                                    number_dates_train_test = grid_combinations_kNp_dates_train_test$number_dates_train_test[ii],
                                                    option = option_paths_data)
        
        target_test_ii <- read_csv(file = path_target_test_ii)
        
        
        results_rnn_ii_0 <- NULL
        
        for (file_name in file_names_results_ii) {
          
          results_temp <- tibble(data.table::fread(paste0(directory_results_raw_model, file_name),
                                       header = TRUE,
                                       colClasses = c("number_xy" = "character",
                                                      "number_combination_features" = "character",
                                                      "number_combination_kNp" = "character",
                                                      "group_dates_train_test" = "character",
                                                      "number_dates_train_test" = "character",
                                                      "number_hyp_par_grid_rnn" = "character")))
          
          results_rnn_ii_0 <- bind_rows(results_rnn_ii_0, results_temp)
          
        }
        
        
        # determine names of hyperparameters
        names_hyp_params <- results_rnn_ii_0 %>%
          dplyr::select(-c("number_xy", "number_combination_features", "name_data_set", "number_combination_kNp", "group_dates_train_test",
                           "number_dates_train_test", "number_hyp_par_grid_rnn", "number_hyp_par_subgrid_rnn", "iteration")) %>%
          dplyr::select(-starts_with("rmse_")) %>%
          dplyr::select(-starts_with("pred_day_")) %>%
          names()
        
        
        # determine columns of `results_rnn_ii_0` to group by
        columns_to_group_by <- results_rnn_ii_0 %>%
          dplyr::select(-c("iteration", "number_hyp_par_subgrid_rnn")) %>%
          dplyr::select(-starts_with("rmse_")) %>%
          dplyr::select(-starts_with("pred_day_")) %>%
          names()
        
        
        # group `results_rnn_ii_0` by columns specified in `columns_to_group_by` and determine mean estimate
        results_rnn_ii_mean <- results_rnn_ii_0 %>%
          group_by(across(all_of(columns_to_group_by))) %>%
          summarize(across(starts_with("pred_day_"), \(x) mean(x, na.rm = TRUE))) %>%
          ungroup() %>%
          mutate(rmse_train = 0, rmse_test = 0)
        
        
        # join `results_rnn_ii_mean` and `table_parameters_kNp`
        results_rnn_ii <- results_rnn_ii_mean %>%
          left_join(table_parameters_kNp, by = "number_combination_kNp") %>%
          dplyr::select(c("number_xy", "number_combination_features", "name_data_set", "number_combination_kNp", "k", "N", "p",
                          "group_dates_train_test", "number_dates_train_test", 
                          "rmse_train", "rmse_test",
                          "number_hyp_par_grid_rnn", all_of(names_hyp_params)),
                        starts_with("pred_day_train_"), starts_with("pred_day_test_"))
        
        
        # determine rmse for training and testing period
        for (jj in 1:nrow(results_rnn_ii)) {
          
          forecast_train_ii_jj <- as.numeric((results_rnn_ii %>% dplyr::select(starts_with("pred_day_train_")))[jj,])
          
          results_rnn_ii$rmse_train[jj] <- Metrics::rmse(actual = target_train_ii %>% pull(y), predicted = forecast_train_ii_jj)
          
          
          forecast_test_ii_jj <- as.numeric((results_rnn_ii %>% dplyr::select(starts_with("pred_day_test_")))[jj,])
          
          results_rnn_ii$rmse_test[jj] <- Metrics::rmse(actual = target_test_ii %>% pull(y), predicted = forecast_test_ii_jj)
          
        }
        
        
        # filter `results_rnn_ii` to row with minimal rmse during testing period 
        results_rnn_summary_ii <- results_rnn_ii %>%
          filter(rmse_test == min(rmse_test)) %>%
          slice_head(n = 1)
        
        
        # determine forecast for training period from `results_rnn_summary_ii`
        results_rnn_forecast_train_ii <- tibble(y = as.numeric(results_rnn_summary_ii %>% dplyr::select(starts_with("pred_day_train_")))) %>%
          mutate(date_day = target_train_ii %>% pull(date_day),
                 date_day_shift_k = target_train_ii %>% pull(date_day_shift_k)) %>%
          dplyr::select(c("date_day", "date_day_shift_k", "y"))
        
        results_rnn_forecast_train_ii <- results_rnn_forecast_train_ii %>%
          mutate(type_model = "rnn",
                 number_xy = number_xy,
                 number_combination_features = number_combination_features,
                 name_data_set = name_data_set,
                 number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test,
                 number_combination_kNp = grid_combinations_kNp_dates_train_test$number_combination_kNp[ii],
                 group_dates_train_test = grid_combinations_kNp_dates_train_test$group_dates_train_test[ii],
                 number_dates_train_test = grid_combinations_kNp_dates_train_test$number_dates_train_test[ii],
                 number_hyp_par_grid = number_hyp_par_grid)
        
        results_rnn_forecast_train <- bind_rows(results_rnn_forecast_train, results_rnn_forecast_train_ii)
        
        
        # determine forecast for testing period from `results_rnn_summary_ii`
        results_rnn_forecast_test_ii <- tibble(y = as.numeric(results_rnn_summary_ii %>% dplyr::select(starts_with("pred_day_test_")))) %>%
          mutate(date_day = target_test_ii %>% pull(date_day),
                 date_day_shift_k = target_test_ii %>% pull(date_day_shift_k)) %>%
          dplyr::select(c("date_day", "date_day_shift_k", "y"))
        
        results_rnn_forecast_test_ii <- results_rnn_forecast_test_ii %>%
          mutate(type_model = "rnn",
                 number_xy = number_xy,
                 number_combination_features = number_combination_features,
                 name_data_set = name_data_set,
                 number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test,
                 number_combination_kNp = grid_combinations_kNp_dates_train_test$number_combination_kNp[ii],
                 group_dates_train_test = grid_combinations_kNp_dates_train_test$group_dates_train_test[ii],
                 number_dates_train_test = grid_combinations_kNp_dates_train_test$number_dates_train_test[ii],
                 number_hyp_par_grid = number_hyp_par_grid)
        
        results_rnn_forecast_test <- bind_rows(results_rnn_forecast_test, results_rnn_forecast_test_ii)
        
        
        # remove forecasts from `results_rnn_ii`
        results_rnn_no_forecasts_ii <- results_rnn_ii %>%
          dplyr::select(-starts_with("pred_day_"))
        
        # add `results_rnn_no_forecasts_ii` to `results_rnn_detailed`
        results_rnn_detailed <- bind_rows(results_rnn_detailed, results_rnn_no_forecasts_ii)
        
        
        # remove forecasts from `results_rnn_summary_ii`
        results_rnn_summary_no_forecasts_ii <- results_rnn_summary_ii %>%
          dplyr::select(-starts_with("pred_day_"))
        
        # add `results_rnn_summary_no_forecasts_ii` to `results_rnn_summary`
        results_rnn_summary <- bind_rows(results_rnn_summary, results_rnn_summary_no_forecasts_ii)
        
      }
      
    }
    
    
    # join `results_lr_forecast_train` and `table_parameters_kNp`
    results_rnn_forecast_train <- results_rnn_forecast_train %>%
      left_join(table_parameters_kNp, by = "number_combination_kNp")
    
    
    # join `results_lr_forecast_test` and `table_parameters_kNp`
    results_rnn_forecast_test <- results_rnn_forecast_test %>%
      left_join(table_parameters_kNp, by = "number_combination_kNp")
    
    
    # store results
    write_csv(results_rnn_forecast_train,
              file = path_results_rnn_forecast_train)
    
    write_csv(results_rnn_forecast_test,
              file = path_results_rnn_forecast_test)
    
    write_csv(results_rnn_detailed,
              file = path_results_rnn_detailed)
    
    write_csv(results_rnn_summary,
              file = path_results_rnn_summary)
    
  }
  
}


