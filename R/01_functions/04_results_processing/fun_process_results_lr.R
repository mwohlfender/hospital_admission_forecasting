

process_results_lr <- function(number_job_array,
                               directory_data,
                               directory_parameters,
                               directory_results_raw,
                               directory_results_processed = Directory_Results,
                               option_paths_data = "long",
                               option_paths_results_raw = "long",
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
  
  
  # determine directories of output
  directory_results_forecasts_lr_train <- get_path_results_forecasts(directory_results = directory_results_processed,
                                                                     type_model = "lr",
                                                                     number_xy = number_xy,
                                                                     number_combination_features = number_combination_features,
                                                                     name_data_set = name_data_set,
                                                                     type_period = "train",
                                                                     option = "directory")
  
  directory_results_forecasts_lr_test <- get_path_results_forecasts(directory_results = directory_results_processed,
                                                                    type_model = "lr",
                                                                    number_xy = number_xy,
                                                                    number_combination_features = number_combination_features,
                                                                    name_data_set = name_data_set,
                                                                    type_period = "test",
                                                                    option = "directory")
  
  # create directories to store output (if they do not already exist)
  if (!(file.exists(directory_results_forecasts_lr_train))) {
    
    dir.create(file.path(directory_results_forecasts_lr_train), recursive = TRUE)
    
  }
  
  if (!(file.exists(directory_results_forecasts_lr_test))) {
    
    dir.create(file.path(directory_results_forecasts_lr_test), recursive = TRUE)
    
  }
  
  
  # determine paths of output
  path_results_lr_forecast_train <- get_path_results_forecasts(directory_results = directory_results_processed,
                                                               type_model = "lr",
                                                               number_xy = number_xy,
                                                               number_combination_features = number_combination_features,
                                                               name_data_set = name_data_set,
                                                               number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test,
                                                               number_hyp_par_grid = number_hyp_par_grid,
                                                               type_period = "train",
                                                               option = "directory_filename")
  
  path_results_lr_forecast_test <- get_path_results_forecasts(directory_results = directory_results_processed,
                                                              type_model = "lr",
                                                              number_xy = number_xy,
                                                              number_combination_features = number_combination_features,
                                                              name_data_set = name_data_set,
                                                              number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test,
                                                              number_hyp_par_grid = number_hyp_par_grid,
                                                              type_period = "test",
                                                              option = "directory_filename")
  
  path_results_lr_detailed <- get_path_results_processed_detailed(directory_results = directory_results_processed,
                                                                  type_model = "lr",
                                                                  number_xy = number_xy,
                                                                  number_combination_features = number_combination_features,
                                                                  name_data_set = name_data_set,
                                                                  number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test,
                                                                  number_hyp_par_grid = number_hyp_par_grid)
  
  path_results_lr_summary <- get_path_results_processed_summary(directory_results = directory_results_processed,
                                                                type_model = "lr",
                                                                number_xy = number_xy,
                                                                number_combination_features = number_combination_features,
                                                                name_data_set = name_data_set,
                                                                number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test,
                                                                number_hyp_par_grid = number_hyp_par_grid)
  
  
  if (!(file.exists(path_results_lr_forecast_train)) | !(file.exists(path_results_lr_forecast_test)) |
      !(file.exists(path_results_lr_detailed)) | !(file.exists(path_results_lr_summary)) | do_new) {
    
    # read parameter grids
    grid_combinations_kNp_dates_train_test <- read_csv(file = paste0(directory_parameters,
                                                                     Path_Grid_Combinations_kNp_Dates_Train_Test, number_grid_combinations_kNp_dates_train_test, ".csv"))
    
    table_parameters_kNp <- read_csv(file = paste0(directory_parameters, Path_Table_Parameters_kNp)) %>%
      dplyr::rename(c("number_combination_kNp" = "number"))
    
    
    # files containing results
    directory_results_raw_model_train <- get_path_results_raw(type_model = "lr",
                                                              number_xy = number_xy,
                                                              number_combination_features = number_combination_features,
                                                              name_data_set = name_data_set,
                                                              number_combination_kNp = "NONE",
                                                              group_dates_train_test = "NONE",
                                                              number_dates_train_test = "NONE",
                                                              number_hyp_par_grid = "NONE",
                                                              number_hyp_par_subgrid = "NONE",
                                                              directory_results = directory_results_raw,
                                                              type_period = "train",
                                                              option = paste0(option_paths_results_raw, "_directory"))
    
    files_raw_results_train <- dir(path = directory_results_raw_model_train)
    
    directory_results_raw_model_test <- get_path_results_raw(type_model = "lr",
                                                             number_xy = number_xy,
                                                             number_combination_features = number_combination_features,
                                                             name_data_set = name_data_set,
                                                             number_combination_kNp = "NONE",
                                                             group_dates_train_test = "NONE",
                                                             number_dates_train_test = "NONE",
                                                             number_hyp_par_grid = "NONE",
                                                             number_hyp_par_subgrid = "NONE",
                                                             directory_results = directory_results_raw,
                                                             type_period = "test",
                                                             option = paste0(option_paths_results_raw, "_directory"))
    
    files_raw_results_test <- dir(path = directory_results_raw_model_test)
    
    
    results_lr_detailed <- NULL
    results_lr_summary <- NULL
    results_lr_forecast_train <- NULL
    results_lr_forecast_test <- NULL
    
    
    for (ii in 1:nrow(grid_combinations_kNp_dates_train_test)) {
      
      print(paste0("ii = ", ii))
      
      pattern_file_names_results_train_ii <- get_path_results_raw(type_model = "lr",
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
      
      files_raw_results_train_ii <- files_raw_results_train[grepl(pattern = pattern_file_names_results_train_ii,
                                                                  x = files_raw_results_train, fixed = TRUE)]
      
      
      pattern_file_names_results_test_ii <- get_path_results_raw(type_model = "lr",
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
      
      files_raw_results_test_ii <- files_raw_results_test[grepl(pattern = pattern_file_names_results_test_ii,
                                                                x = files_raw_results_test, fixed = TRUE)]
      
      
      if ((length(files_raw_results_train_ii) > 0) & (length(files_raw_results_train_ii) == length(files_raw_results_test_ii))) {
        
        target_train_ii <- read_csv(file = get_path_features_target(number_xy = number_xy,
                                                                    number_combination_features = number_combination_features,
                                                                    name_data_set = name_data_set,
                                                                    number_combination_kNp = grid_combinations_kNp_dates_train_test$number_combination_kNp[ii],
                                                                    group_dates_train_test = grid_combinations_kNp_dates_train_test$group_dates_train_test[ii],
                                                                    number_dates_train_test = grid_combinations_kNp_dates_train_test$number_dates_train_test[ii],
                                                                    option_output = "target_train",
                                                                    directory_data = directory_data,
                                                                    option_path = paste0(option_paths_data, "_directory_filename")))
        
        target_test_ii <- read_csv(file = get_path_features_target(number_xy = number_xy,
                                                                   number_combination_features = number_combination_features,
                                                                   name_data_set = name_data_set,
                                                                   number_combination_kNp = grid_combinations_kNp_dates_train_test$number_combination_kNp[ii],
                                                                   group_dates_train_test = grid_combinations_kNp_dates_train_test$group_dates_train_test[ii],
                                                                   number_dates_train_test = grid_combinations_kNp_dates_train_test$number_dates_train_test[ii],
                                                                   option_output = "target_test",
                                                                   directory_data = directory_data,
                                                                   option_path = paste0(option_paths_data, "_directory_filename")))
        
        
        results_lr_ii_train_0 <- NULL
        
        for (jj in 1:length(files_raw_results_train_ii)) {
          
          results_temp_train <- tibble(data.table::fread(paste0(directory_results_raw_model_train, files_raw_results_train_ii[jj]),
                                                         header = TRUE,
                                                         colClasses = c("number_xy" = "character",
                                                                        "number_combination_features" = "character",
                                                                        "number_combination_kNp" = "character",
                                                                        "group_dates_train_test" = "character",
                                                                        "number_dates_train_test" = "character",
                                                                        "number_hyp_par_grid_lr" = "character")))
          
          results_lr_ii_train_0 <- bind_rows(results_lr_ii_train_0, results_temp_train)
          
        }
        
        results_lr_ii_test_0 <- NULL
        
        for (jj in 1:length(files_raw_results_test_ii)) {
          
          results_temp_test <- tibble(data.table::fread(paste0(directory_results_raw_model_test, files_raw_results_test_ii[jj]),
                                                        header = TRUE,
                                                        colClasses = c("number_xy" = "character",
                                                                       "number_combination_features" = "character",
                                                                       "number_combination_kNp" = "character",
                                                                       "group_dates_train_test" = "character",
                                                                       "number_dates_train_test" = "character",
                                                                       "number_hyp_par_grid_lr" = "character")))
          
          results_lr_ii_test_0 <- bind_rows(results_lr_ii_test_0, results_temp_test)
          
        }
        
        results_lr_ii_0 <- results_lr_ii_train_0 %>%
          left_join(results_lr_ii_test_0, by = intersect(names(results_lr_ii_train_0), names(results_lr_ii_test_0)))
        
        
        # determine names of hyperparameters
        names_hyp_params <- results_lr_ii_0 %>%
          dplyr::select(-c("number_xy", "number_combination_features", "name_data_set", "number_combination_kNp", "group_dates_train_test",
                           "number_dates_train_test", "number_hyp_par_grid_lr", "number_hyp_par_subgrid_lr", "iteration")) %>%
          dplyr::select(-starts_with("rmse_")) %>%
          dplyr::select(-starts_with("y_pred_")) %>%
          names()
        
        
        # determine columns of `results_lr_ii_0` to group by
        columns_to_group_by <- results_lr_ii_0 %>%
          dplyr::select(-c("iteration", "number_hyp_par_subgrid_lr")) %>%
          dplyr::select(-starts_with("rmse_")) %>%
          dplyr::select(-starts_with("y_pred_")) %>%
          names()
        
        
        # group `results_lr_ii_0` by columns specified in `columns_to_group_by` and determine mean estimate
        results_lr_ii_mean <- results_lr_ii_0 %>%
          group_by(across(all_of(columns_to_group_by))) %>%
          summarize(across(starts_with("y_pred_"), \(x) mean(x, na.rm = TRUE))) %>%
          ungroup()
        
        
        # join `results_lr_ii_mean` and `table_parameters_kNp`
        results_lr_ii <- results_lr_ii_mean %>%
          left_join(table_parameters_kNp, by = "number_combination_kNp") %>%
          mutate(rmse_train = 0,
                 rmse_test = 0) %>%
          dplyr::select(c("number_xy", "number_combination_features", "name_data_set", "number_combination_kNp", "k", "N", "p",
                          "group_dates_train_test", "number_dates_train_test", "number_hyp_par_grid_lr", "rmse_train", "rmse_test", all_of(names_hyp_params)),
                        starts_with("y_pred_"))
        
        
        # determine rmse for training and testing period
        for (jj in 1:nrow(results_lr_ii)) {
          
          forecast_train_ii_jj <- as.numeric((results_lr_ii %>% dplyr::select(starts_with("y_pred_lr_train_")))[jj,])
          
          results_lr_ii$rmse_train[jj] <- Metrics::rmse(actual = target_train_ii %>% pull(y), predicted = forecast_train_ii_jj)
          
          
          forecast_test_ii_jj <- as.numeric((results_lr_ii %>% dplyr::select(starts_with("y_pred_lr_test_")))[jj,])
          
          results_lr_ii$rmse_test[jj] <- Metrics::rmse(actual = target_test_ii %>% pull(y), predicted = forecast_test_ii_jj)
          
        }
        
        
        # filter `results_nn_ii` to row with minimal rmse during testing period 
        results_lr_summary_ii <- results_lr_ii %>%
          filter(rmse_test == min(rmse_test)) %>%
          slice_head(n = 1)
        
        
        # determine forecast for training period from `results_lr_summary_ii`
        results_lr_forecast_train_ii <- tibble(y = results_lr_summary_ii %>% dplyr::select(starts_with("y_pred_lr_train_")) %>% as.numeric()) %>%
          mutate(date_day = target_train_ii %>% pull(date_day),
                 date_day_shift_k = target_train_ii %>% pull(date_day_shift_k)) %>%
          dplyr::select(c("date_day", "date_day_shift_k", "y"))
        
        results_lr_forecast_train_ii <- results_lr_forecast_train_ii %>%
          mutate(type_model = "lr",
                 number_xy = number_xy,
                 number_combination_features = number_combination_features,
                 name_data_set = name_data_set,
                 number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test,
                 number_combination_kNp = grid_combinations_kNp_dates_train_test$number_combination_kNp[ii],
                 group_dates_train_test = grid_combinations_kNp_dates_train_test$group_dates_train_test[ii],
                 number_dates_train_test = grid_combinations_kNp_dates_train_test$number_dates_train_test[ii],
                 number_hyp_par_grid = number_hyp_par_grid)
        
        results_lr_forecast_train <- bind_rows(results_lr_forecast_train, results_lr_forecast_train_ii)
        
        
        # determine forecast for testing period from `results_lr_summary_ii`
        results_lr_forecast_test_ii <- tibble(y = results_lr_summary_ii %>% dplyr::select(starts_with("y_pred_lr_test_")) %>% as.numeric()) %>%
          mutate(date_day = target_test_ii %>% pull(date_day),
                 date_day_shift_k = target_test_ii %>% pull(date_day_shift_k)) %>%
          dplyr::select(c("date_day", "date_day_shift_k", "y"))
        
        results_lr_forecast_test_ii <- results_lr_forecast_test_ii %>%
          mutate(type_model = "lr",
                 number_xy = number_xy,
                 number_combination_features = number_combination_features,
                 name_data_set = name_data_set,
                 number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test,
                 number_combination_kNp = grid_combinations_kNp_dates_train_test$number_combination_kNp[ii],
                 group_dates_train_test = grid_combinations_kNp_dates_train_test$group_dates_train_test[ii],
                 number_dates_train_test = grid_combinations_kNp_dates_train_test$number_dates_train_test[ii],
                 number_hyp_par_grid = number_hyp_par_grid)
        
        results_lr_forecast_test <- bind_rows(results_lr_forecast_test, results_lr_forecast_test_ii)
        
        
        # remove forecasts from `results_lr_ii`
        results_lr_no_forecasts_ii <- results_lr_ii %>%
          dplyr::select(-starts_with("y_pred_"))
        
        # add `results_lr_no_forecasts_ii` to `results_lr_detailed`
        results_lr_detailed <- bind_rows(results_lr_detailed, results_lr_no_forecasts_ii)
        
        
        # remove forecasts from `results_lr_summary_ii`
        results_lr_summary_no_forecasts_ii <- results_lr_summary_ii %>%
          dplyr::select(-starts_with("y_pred_"))
        
        # add `results_lr_summary_no_forecasts_ii` to `results_lr_summary`
        results_lr_summary <- bind_rows(results_lr_summary, results_lr_summary_no_forecasts_ii)
        
      }
      
    }
    
    
    # join `results_lr_forecast_train` and `table_parameters_kNp`
    results_lr_forecast_train <- results_lr_forecast_train %>%
      left_join(table_parameters_kNp, by = "number_combination_kNp")
    
    
    # join `results_lr_forecast_test` and `table_parameters_kNp`
    results_lr_forecast_test <- results_lr_forecast_test %>%
      left_join(table_parameters_kNp, by = "number_combination_kNp")
    
    
    # store results
    write_csv(results_lr_forecast_train,
              file = path_results_lr_forecast_train)
    
    write_csv(results_lr_forecast_test,
              file = path_results_lr_forecast_test)
    
    write_csv(results_lr_detailed,
              file = path_results_lr_detailed)
    
    write_csv(results_lr_summary,
              file = path_results_lr_summary)
    
  }
  
}


