
# function `apply_model_xgb`:

# input:

# 1) What data shall be used?
# `number_xy`: index of combination of data and target variable
# `number_combination_features`: index of combination of features
# `name_data_set`: name of data set (determined by stratification)
# `number_combination_kNp`: index for combination of parameters k, N and p
# `group_dates_train_test`: primary index for train-test split
# `number_dates_train_test`: secondary index for train-test split

# 2) What is the setup for the XGBoost model?
# `number_hyp_par_grid_xgb`: number of grid of combinations of hyper parameters
# `number_hyp_par_subgrid_xgb`: number of subgrid of combinations of hyper parameters
# `bool_pred_train`: Shall predictions on training data be stored?
# `bool_pred_test`: Shall predictions on testing data be stored?

# 3) Where is the data stored and where shall results be stored?

apply_model_xgb <- function(number_xy,
                            number_combination_features,
                            name_data_set,
                            number_combination_kNp,
                            group_dates_train_test,
                            number_dates_train_test,
                            number_hyp_par_grid_xgb,
                            number_hyp_par_subgrid_xgb,
                            bool_summarize_results = FALSE,
                            bool_pred_train = FALSE,
                            bool_pred_test = FALSE,
                            directory_parameters,
                            directory_data,
                            option_paths_data = "short",
                            directory_results,
                            option_path_results = "short",
                            do_new = FALSE) {
  
  # determine directory of output
  directory_output <- get_path_results_raw(type_model = "xgb",
                                           number_xy = number_xy,
                                           number_combination_features = number_combination_features,
                                           name_data_set = name_data_set,
                                           number_combination_kNp = number_combination_kNp,
                                           group_dates_train_test = group_dates_train_test,
                                           number_dates_train_test = number_dates_train_test,
                                           number_hyp_par_grid = number_hyp_par_grid_xgb,
                                           number_hyp_par_subgrid = number_hyp_par_subgrid_xgb,
                                           directory_results = directory_results,
                                           option = paste0(option_path_results, "_directory"))
  
  if (!(dir.exists(directory_output))) {
    
    dir.create(file.path(directory_output), recursive = TRUE)
    
  }
  
  # determine path of output
  path_output <- get_path_results_raw(type_model = "xgb",
                                      number_xy = number_xy,
                                      number_combination_features = number_combination_features,
                                      name_data_set = name_data_set,
                                      number_combination_kNp = number_combination_kNp,
                                      group_dates_train_test = group_dates_train_test,
                                      number_dates_train_test = number_dates_train_test,
                                      number_hyp_par_grid = number_hyp_par_grid_xgb,
                                      number_hyp_par_subgrid = number_hyp_par_subgrid_xgb,
                                      directory_results = directory_results,
                                      option = paste0(option_path_results, "_directory_filename"))
  
  
  if (!(file.exists(path_output)) | do_new) {
    
    # read data ----
    
    # read parameter grid for hyper parameter search
    if (number_hyp_par_subgrid_xgb == 0) {
      
      grid_hyp_params_xgb <- read_csv(paste0(directory_parameters, Subdirectory_Parameters_Hyperparameters,
                                             "xgb/", number_hyp_par_grid_xgb, "/hyp_par_xgb_", number_hyp_par_grid_xgb, "_param_grid.csv"))
      
    } else {
      
      grid_hyp_params_xgb <- read_csv(paste0(directory_parameters, Subdirectory_Parameters_Hyperparameters,
                                             "xgb/", number_hyp_par_grid_xgb, "/hyp_par_xgb_", number_hyp_par_grid_xgb, "_param_grid_", number_hyp_par_subgrid_xgb,".csv"))
      
    }
    
    # determine how many different combinations of hyper parameters will be tested
    n_par_combs_xgb <- nrow(grid_hyp_params_xgb)
    
    # define tibble to store parameters (`number_xy`, `number_combination_features`, `name_data_set`, `number_combination_kNp`, `group_dates_train_test`, `number_dates_train_test`, `number_hyp_par_grid_xgb` and `number_hyp_par_subgrid_xgb`)
    parameters_grid <- tibble(number_xy = rep(x = number_xy, times = n_par_combs_xgb),
                              number_combination_features = rep(x = number_combination_features, times = n_par_combs_xgb),
                              name_data_set = rep(x = name_data_set, times = n_par_combs_xgb),
                              number_combination_kNp = rep(x = number_combination_kNp, times = n_par_combs_xgb),
                              group_dates_train_test = rep(x = group_dates_train_test, times = n_par_combs_xgb),
                              number_dates_train_test = rep(x = number_dates_train_test, times = n_par_combs_xgb),
                              number_hyp_par_grid_xgb = rep(x = number_hyp_par_grid_xgb, times = n_par_combs_xgb),
                              number_hyp_par_subgrid_xgb = rep(x = number_hyp_par_subgrid_xgb, times = n_par_combs_xgb))
    
    # read training data
    data_train <- read_csv(file = get_path_features_target(number_xy = number_xy,
                                                           number_combination_features = number_combination_features,
                                                           name_data_set = name_data_set,
                                                           number_combination_kNp = number_combination_kNp,
                                                           group_dates_train_test = group_dates_train_test,
                                                           number_dates_train_test = number_dates_train_test,
                                                           option_output = "features_train",
                                                           directory_data = directory_data,
                                                           option_path = paste0(option_paths_data, "_directory_filename")))
    
    target_train <- read_csv(file = get_path_features_target(number_xy = number_xy,
                                                             number_combination_features = number_combination_features,
                                                             name_data_set = name_data_set,
                                                             number_combination_kNp = number_combination_kNp,
                                                             group_dates_train_test = group_dates_train_test,
                                                             number_dates_train_test = number_dates_train_test,
                                                             option_output = "target_train",
                                                             directory_data = directory_data,
                                                             option_path = paste0(option_paths_data, "_directory_filename")))
    
    # read testing data
    data_test <- read_csv(file = get_path_features_target(number_xy = number_xy,
                                                          number_combination_features = number_combination_features,
                                                          name_data_set = name_data_set,
                                                          number_combination_kNp = number_combination_kNp,
                                                          group_dates_train_test = group_dates_train_test,
                                                          number_dates_train_test = number_dates_train_test,
                                                          option_output = "features_test",
                                                          directory_data = directory_data,
                                                          option_path = paste0(option_paths_data, "_directory_filename")))
    
    target_test <- read_csv(file = get_path_features_target(number_xy = number_xy,
                                                            number_combination_features = number_combination_features,
                                                            name_data_set = name_data_set,
                                                            number_combination_kNp = number_combination_kNp,
                                                            group_dates_train_test = group_dates_train_test,
                                                            number_dates_train_test = number_dates_train_test,
                                                            option_output = "target_test",
                                                            directory_data = directory_data,
                                                            option_path = paste0(option_paths_data, "_directory_filename")))
    
    
    # format data for XGBoost ----
    data_train_xgb <- xgboost::xgb.DMatrix(data=data.matrix(data_train), label=data.matrix(target_train$y))
    data_test_xgb <- xgboost::xgb.DMatrix(data=data.matrix(data_test))
    
    
    # define tibble where results of parameter estimation with different combinations of hyper parameters will be stored ----
    if (bool_pred_train) {
      
      names_res_xgb_train <- unlist(lapply(X = 1:n_par_combs_xgb,
                                           FUN = function(x) paste0("predictions_train_", str_pad(x, ceiling(log(min(n_par_combs_xgb,2), base=10)), pad = "0"))))
      
      predictions_xgb_train <- as_tibble(matrix(data = 0, nrow = nrow(data_train), ncol = n_par_combs_xgb, dimnames = list(NULL, names_res_xgb_train)))
      
    }
    
    if (bool_pred_test) {
      
      names_res_xgb_test <- unlist(lapply(X = 1:n_par_combs_xgb,
                                          FUN = function(x) paste0("predictions_test_", str_pad(x, ceiling(log(min(n_par_combs_xgb,2), base=10)), pad = "0"))))
      
      predictions_xgb_test <- as_tibble(matrix(data = 0, nrow = nrow(data_test), ncol = n_par_combs_xgb, dimnames = list(NULL, names_res_xgb_test)))
      
    }
    
    # define tibble where rmse of parameter estimation with different combinations of hyper parameters will be stored ----
    xgb_rmse <- tibble(rmse_train = rep(x = 0, times = n_par_combs_xgb),
                       rmse_test = rep(x = 0, times = n_par_combs_xgb))
    
    
    # run parameter estimation with different combinations of hyper parameters ---- 
    for (ii in 1:n_par_combs_xgb) {
      
      if (ii %% 100 == 0) {
        
        print(ii)
        
      }
      
      # parameters
      hyp_params_xgb <- list(eta = grid_hyp_params_xgb$eta[ii],
                             gamma = grid_hyp_params_xgb$gamma[ii],
                             max_depth = grid_hyp_params_xgb$max_depth[ii],
                             min_child_weight = grid_hyp_params_xgb$min_child_weight[ii],
                             max_delta_step = grid_hyp_params_xgb$max_delta_step[ii],
                             subsample = grid_hyp_params_xgb$subsample[ii],
                             colsample_bytree = grid_hyp_params_xgb$colsample_bytree[ii],
                             colsample_bylevel = grid_hyp_params_xgb$colsample_bylevel[ii],
                             colsample_bynode = grid_hyp_params_xgb$colsample_bynode[ii],
                             lambda = grid_hyp_params_xgb$lambda[ii],
                             alpha = grid_hyp_params_xgb$alpha[ii])
      
      # train model
      model_xgb <- xgboost::xgboost(data = data_train_xgb,
                                    params = hyp_params_xgb,
                                    nrounds = grid_hyp_params_xgb$nrounds[ii],
                                    verbose = 0)
      
      # predict on training data
      prediction_train <- stats::predict(model_xgb, data_train_xgb)
      
      # store results
      if (bool_pred_train) {
        
        predictions_xgb_train[,ii] <- prediction_train
        
      }
      
      # calculate rmse
      xgb_rmse$rmse_train[ii] <-  Metrics::rmse(actual = target_train$y, predicted = prediction_train)
      
      # predict on testing data
      prediction_test <- stats::predict(model_xgb, data_test_xgb)
      
      # store results
      if (bool_pred_test) {
        
        predictions_xgb_test[,ii] <- prediction_test
        
      }
      
      # calculate rmse
      xgb_rmse$rmse_test[ii] <-  Metrics::rmse(actual = target_test$y, predicted = prediction_test)
      
    }
    
    
    # define tibble `output` (`parameters_grid`, `grid_hyp_params_xgb`, `xgb_rmse` and predictions (if stored)) ----
    output <- bind_cols(parameters_grid, grid_hyp_params_xgb, xgb_rmse)
    
    if (bool_pred_train) {
      
      length_training_period <- nrow(data_train)
      
      names_output_train <- unlist(lapply(X = 1:length_training_period,
                                          FUN = function(x) paste0("pred_day_train_", str_pad(x, ceiling(log(length_training_period, base=10)), pad = "0"))))
      
      output <- bind_cols(output, as_tibble(matrix(data = t(predictions_xgb_train), nrow = n_par_combs_xgb, ncol = length_training_period, dimnames = list(NULL, names_output_train))))
      
    } 
    
    if (bool_pred_test) {
      
      length_testing_period <- nrow(data_test)
      
      names_output_test <- unlist(lapply(X = 1:length_testing_period,
                                         FUN = function(x) paste0("pred_day_test_", str_pad(x, ceiling(log(length_testing_period, base=10)), pad = "0"))))
      
      output <- bind_cols(output, as_tibble(matrix(data = t(predictions_xgb_test), nrow = n_par_combs_xgb, ncol = length_testing_period, dimnames = list(NULL, names_output_test))))
      
    }
    
    if (bool_summarize_results & (bool_pred_train | bool_pred_test)) {
      
      # determine columns of `output` to group by (all columns except those containing forecasts or root mean square errors)
      columns_to_group_by <- output %>%
        dplyr::select(-c("iteration")) %>%
        dplyr::select(-starts_with("rmse_")) %>%
        dplyr::select(-starts_with("pred_day_")) %>%
        names()
      
      # group `results_xgb_ii_0` by columns specified in `columns_to_group_by` and determine mean estimate
      output_summarized <- output %>%
        group_by(across(all_of(columns_to_group_by))) %>%
        summarize(across(starts_with("pred_day_"), \(x) mean(x, na.rm = TRUE))) %>%
        ungroup() %>%
        mutate(iteration = 0, rmse_train = 0, rmse_test = 0)
      
      for (ii in 1:nrow(output_summarized)) {
        
        forecast_train_ii <- as.numeric((output_summarized %>% dplyr::select(starts_with("pred_day_train_")))[ii,])
        
        output_summarized$rmse_train[ii] <- Metrics::rmse(actual = target_train %>% pull(y), predicted = forecast_train_ii)
        
        forecast_test_ii <- as.numeric((output_summarized %>% dplyr::select(starts_with("pred_day_test_")))[ii,])
        
        output_summarized$rmse_test[ii] <- Metrics::rmse(actual = target_test %>% pull(y), predicted = forecast_test_ii)
        
      }
      
      output <- output_summarized %>%
        dplyr::select(all_of(columns_to_group_by), "iteration", starts_with("rmse_"), starts_with("pred_day_"))
      
    }
    
    # write `output` to csv file ----
    write_csv(x = output,
              file = path_output)
    
  }
  
}


