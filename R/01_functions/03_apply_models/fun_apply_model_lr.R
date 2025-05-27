
# function `apply_model_lr`:

# input:

# `number_xy`: index of combination of features and target variable
# `number_combination_features`: index of combination of features
# `name_data_set`: name of data set (determined by stratification)
# `number_combination_kNp`: index for combination of parameters k, N and p
# `group_dates_train_test`: primary index for train-test split
# `number_dates_train_test`: secondary index for train-test split
# `number_hyp_par_grid_lr`:
# `number_hyp_par_subgrid_lr`:
# `directory_parameters`:
# `directory_data`: general path to feature matrix and target variable vector for training and testing period
# `directory_results`: general path to folder where results are stored
# `option_path`:
# `do_new:` whether or not calculations shall be repeated even though the results files already exist

apply_model_lr <- function(number_xy,
                           number_combination_features,
                           name_data_set,
                           number_combination_kNp,
                           group_dates_train_test,
                           number_dates_train_test,
                           number_hyp_par_grid_lr,
                           number_hyp_par_subgrid_lr,
                           directory_parameters = Directory_Parameters,
                           directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                           directory_results = Directory_Results,
                           option_path = "long",
                           do_new = FALSE) {
  
  # determine directories of output
  directory_output_results_train <- get_path_results_raw(type_model = "lr",
                                                         number_xy = number_xy,
                                                         number_combination_features = number_combination_features,
                                                         name_data_set = name_data_set,
                                                         directory_results = directory_results,
                                                         type_period = "train",
                                                         option = paste0(option_path, "_directory"))
  
  if (!(dir.exists(directory_output_results_train))) {
    
    dir.create(file.path(directory_output_results_train), recursive = TRUE)
    
  }
  
  directory_output_results_test <- get_path_results_raw(type_model = "lr",
                                                        number_xy = number_xy,
                                                        number_combination_features = number_combination_features,
                                                        name_data_set = name_data_set,
                                                        directory_results = directory_results,
                                                        type_period = "test",
                                                        option = paste0(option_path, "_directory"))
  
  if (!(dir.exists(directory_output_results_test))) {
    
    dir.create(file.path(directory_output_results_test), recursive = TRUE)
    
  }
  
  directory_output_coefficients <- get_path_results_lr_coefficients(number_xy = number_xy,
                                                                    number_combination_features = number_combination_features,
                                                                    name_data_set = name_data_set,
                                                                    directory_results = directory_results,
                                                                    option = paste0(option_path, "_directory"))
  
  if (!(dir.exists(directory_output_coefficients))) {
    
    dir.create(file.path(directory_output_coefficients), recursive = TRUE)
    
  }
  
  
  # determine paths of output
  path_output_results_train <- get_path_results_raw(type_model = "lr",
                                                    number_xy = number_xy,
                                                    number_combination_features = number_combination_features,
                                                    name_data_set = name_data_set,
                                                    number_combination_kNp = number_combination_kNp,
                                                    group_dates_train_test = group_dates_train_test,
                                                    number_dates_train_test = number_dates_train_test,
                                                    number_hyp_par_grid = number_hyp_par_grid_lr,
                                                    number_hyp_par_subgrid = number_hyp_par_subgrid_lr,
                                                    directory_results = directory_results,
                                                    type_period = "train",
                                                    ending = ".csv",
                                                    option = paste0(option_path, "_directory_filename"))
  
  path_output_results_test <- get_path_results_raw(type_model = "lr",
                                                   number_xy = number_xy,
                                                   number_combination_features = number_combination_features,
                                                   name_data_set = name_data_set,
                                                   number_combination_kNp = number_combination_kNp,
                                                   group_dates_train_test = group_dates_train_test,
                                                   number_dates_train_test = number_dates_train_test,
                                                   number_hyp_par_grid = number_hyp_par_grid_lr,
                                                   number_hyp_par_subgrid = number_hyp_par_subgrid_lr,
                                                   directory_results = directory_results,
                                                   type_period = "test",
                                                   ending = ".csv",
                                                   option = paste0(option_path, "_directory_filename"))
  
  
  if (!(file.exists(path_output_results_train)) | !(file.exists(path_output_results_test)) | do_new) {
    
    # determine paths of data
    path_data_train <- get_path_data_train(directory_data = directory_data,
                                           number_xy = number_xy,
                                           number_combination_features = number_combination_features,
                                           name_data_set = name_data_set,
                                           number_combination_kNp = number_combination_kNp,
                                           group_dates_train_test = group_dates_train_test,
                                           number_dates_train_test = number_dates_train_test)
    
    path_data_test <- get_path_data_test(directory_data = directory_data,
                                         number_xy = number_xy,
                                         number_combination_features = number_combination_features,
                                         name_data_set = name_data_set,
                                         number_combination_kNp = number_combination_kNp,
                                         group_dates_train_test = group_dates_train_test,
                                         number_dates_train_test = number_dates_train_test)
    
    path_target_train <- get_path_target_train(directory_data = directory_data,
                                               number_xy = number_xy,
                                               number_combination_features = number_combination_features,
                                               name_data_set = name_data_set,
                                               number_combination_kNp = number_combination_kNp,
                                               group_dates_train_test = group_dates_train_test,
                                               number_dates_train_test = number_dates_train_test)
    
    path_target_test <- get_path_target_test(directory_data = directory_data,
                                             number_xy = number_xy,
                                             number_combination_features = number_combination_features,
                                             name_data_set = name_data_set,
                                             number_combination_kNp = number_combination_kNp,
                                             group_dates_train_test = group_dates_train_test,
                                             number_dates_train_test = number_dates_train_test)
    
    # read training data
    data_train <- read_csv(file = path_data_train)
    target_train <- read_csv(file = path_target_train)
    
    length_training_period <- nrow(data_train)
    
    # read testing data
    data_test <- read_csv(file = path_data_test)
    target_test <- read_csv(file = path_target_test)
    
    length_testing_period <- nrow(data_test)
    
    # read parameter grid for hyperparameter search
    if (number_hyp_par_subgrid_lr == 0) {
      
      grid_hyp_params_lr <- read_csv(paste0(directory_parameters, Subdirectory_Parameters_Hyperparameters, "lr/", number_hyp_par_grid_lr, "/",
                                            "hyp_par_lr_", number_hyp_par_grid_lr, "_param_grid.csv"))
      
    } else {
      
      grid_hyp_params_lr <- read_csv(paste0(directory_parameters, Subdirectory_Parameters_Hyperparameters, "lr/", number_hyp_par_grid_lr, "/",
                                            "hyp_par_lr_", number_hyp_par_grid_lr, "_param_grid_", number_hyp_par_subgrid_lr,".csv"))
      
    }
    
    # determine how many different combinations of hyperparameters will be tested
    n_par_combs_lr <- nrow(grid_hyp_params_lr)
    
    # define tibble to store parameters (`number_xy`, `number_combination_features`, `name_data_set`, `number_combination_kNp`,
    # `group_dates_train_test`, `number_dates_train_test`, `number_hyp_par_grid_lr` and `number_hyp_par_subgrid_lr`)
    parameters_grid <- tibble(number_xy = rep(x = number_xy, times = n_par_combs_lr),
                              number_combination_features = rep(x = number_combination_features, times = n_par_combs_lr),
                              name_data_set = rep(x = name_data_set, times = n_par_combs_lr),
                              number_combination_kNp = rep(x = number_combination_kNp, times = n_par_combs_lr),
                              group_dates_train_test = rep(x = group_dates_train_test, times = n_par_combs_lr),
                              number_dates_train_test = rep(x = number_dates_train_test, times = n_par_combs_lr),
                              number_hyp_par_grid_lr = rep(x = number_hyp_par_grid_lr, times = n_par_combs_lr),
                              number_hyp_par_subgrid_lr = rep(x = number_hyp_par_subgrid_lr, times = n_par_combs_lr))
    
    predictions_lr_train <- NULL
    predictions_lr_test <- NULL 
    
    # apply model with different combinations of hyperparameters
    for (ii in 1:n_par_combs_lr) {
      
      # train model
      model_lin_reg_ii <- lm(formula = y ~ ., data = cbind(y = target_train$y, data_train))
      
      # predict: training period
      pred_lin_reg_pred_train_ii <- tibble(data.frame(stats::predict(model_lin_reg_ii, newdata = data_train, interval = "predict", level = 0.80)))
      
      pred_lin_reg_conf_train_ii <- tibble(data.frame(stats::predict(model_lin_reg_ii, newdata = data_train, interval = "confidence", level = 0.80)))
      
     
      names_output_train_y_pred_lr <- unlist(lapply(X = 1:length_training_period,
                                          FUN = function(x) paste0("y_pred_lr_train_", str_pad(x, ceiling(log(length_training_period, base = 10)), pad = "0"))))
      
      names_output_train_y_pred_lr_pred_lwr <- unlist(lapply(X = 1:length_training_period,
                                                    FUN = function(x) paste0("y_pred_lr_pred_lwr_train_", str_pad(x, ceiling(log(length_training_period, base = 10)), pad = "0"))))
      
      names_output_train_y_pred_lr_pred_upr <- unlist(lapply(X = 1:length_training_period,
                                                    FUN = function(x) paste0("y_pred_lr_pred_upr_train_", str_pad(x, ceiling(log(length_training_period, base = 10)), pad = "0"))))
      
      names_output_train_y_pred_lr_conf_lwr <- unlist(lapply(X = 1:length_training_period,
                                                    FUN = function(x) paste0("y_pred_lr_conf_lwr_train_", str_pad(x, ceiling(log(length_training_period, base = 10)), pad = "0"))))
      
      names_output_train_y_pred_lr_conf_upr <- unlist(lapply(X = 1:length_training_period,
                                                    FUN = function(x) paste0("y_pred_lr_conf_upr_train_", str_pad(x, ceiling(log(length_training_period, base = 10)), pad = "0"))))
      
      predictions_lr_train_ii <- bind_cols(as_tibble(matrix(data = c(pred_lin_reg_pred_train_ii$fit,pred_lin_reg_pred_train_ii$lwr, pred_lin_reg_pred_train_ii$upr,
                                                                     pred_lin_reg_conf_train_ii$lwr, pred_lin_reg_conf_train_ii$upr),
                                 nrow = 1, ncol = 5*length_training_period,
                                 dimnames = list(NULL, c(names_output_train_y_pred_lr, names_output_train_y_pred_lr_pred_lwr, names_output_train_y_pred_lr_pred_upr,
                                                         names_output_train_y_pred_lr_conf_lwr, names_output_train_y_pred_lr_conf_upr)))))
      
      predictions_lr_train <- predictions_lr_train %>%
        bind_rows(predictions_lr_train_ii)

      
      # predict: testing period
      pred_lin_reg_pred_test_ii <- tibble(data.frame(stats::predict(model_lin_reg_ii, newdata = data_test, interval = "predict", level = 0.80)))
      
      pred_lin_reg_conf_test_ii <- tibble(data.frame(stats::predict(model_lin_reg_ii, newdata = data_test, interval = "confidence", level = 0.80)))
      
      
      names_output_test_y_pred_lr <- unlist(lapply(X = 1:length_testing_period,
                                                    FUN = function(x) paste0("y_pred_lr_test_", str_pad(x, ceiling(log(length_testing_period, base = 10)), pad = "0"))))
      
      names_output_test_y_pred_lr_pred_lwr <- unlist(lapply(X = 1:length_testing_period,
                                                             FUN = function(x) paste0("y_pred_lr_pred_lwr_test_", str_pad(x, ceiling(log(length_testing_period, base = 10)), pad = "0"))))
      
      names_output_test_y_pred_lr_pred_upr <- unlist(lapply(X = 1:length_testing_period,
                                                             FUN = function(x) paste0("y_pred_lr_pred_upr_test_", str_pad(x, ceiling(log(length_testing_period, base = 10)), pad = "0"))))
      
      names_output_test_y_pred_lr_conf_lwr <- unlist(lapply(X = 1:length_testing_period,
                                                             FUN = function(x) paste0("y_pred_lr_conf_lwr_test_", str_pad(x, ceiling(log(length_testing_period, base = 10)), pad = "0"))))
      
      names_output_test_y_pred_lr_conf_upr <- unlist(lapply(X = 1:length_testing_period,
                                                             FUN = function(x) paste0("y_pred_lr_conf_upr_test_", str_pad(x, ceiling(log(length_testing_period, base = 10)), pad = "0"))))
      
      predictions_lr_test_ii <- bind_cols(as_tibble(matrix(data = c(pred_lin_reg_pred_test_ii$fit, pred_lin_reg_pred_test_ii$lwr, pred_lin_reg_pred_test_ii$upr,
                                                                     pred_lin_reg_conf_test_ii$lwr, pred_lin_reg_conf_test_ii$upr),
                                                            nrow = 1, ncol = 5*length_testing_period,
                                                            dimnames = list(NULL, c(names_output_test_y_pred_lr, names_output_test_y_pred_lr_pred_lwr, names_output_test_y_pred_lr_pred_upr,
                                                                                    names_output_test_y_pred_lr_conf_lwr, names_output_test_y_pred_lr_conf_upr)))))
      
      predictions_lr_test <- predictions_lr_test %>%
        bind_rows(predictions_lr_test_ii)
      
      # determine path where coefficients of model shall be stored
      path_output_coefficients_ii <- get_path_results_lr_coefficients(number_xy = number_xy,
                                                                      number_combination_features = number_combination_features,
                                                                      name_data_set = name_data_set,
                                                                      number_combination_kNp = number_combination_kNp,
                                                                      group_dates_train_test = group_dates_train_test,
                                                                      number_dates_train_test = number_dates_train_test,
                                                                      number_hyp_par_grid = number_hyp_par_grid_lr,
                                                                      number_hyp_par_subgrid = number_hyp_par_subgrid_lr,
                                                                      directory_results = directory_results,
                                                                      option = paste0(option_path, "_directory_filename"),
                                                                      ending = paste0("_", as.character(ii), ".txt"))
      
      # save coefficients
      sink(path_output_coefficients_ii)
      print(summary(model_lin_reg_ii))
      sink()
      
    }
    
    # join `parameters_grid` and `predictions_lr_train`
    predictions_lr_train <- parameters_grid %>%
      bind_cols(grid_hyp_params_lr) %>%
      bind_cols(predictions_lr_train)
    
    # join `parameters_grid` and `predictions_lr_test`
    predictions_lr_test <- parameters_grid %>%
      bind_cols(grid_hyp_params_lr) %>%
      bind_cols(predictions_lr_test)
    
    # save results
    write_csv(x = predictions_lr_train,
              file = path_output_results_train)
    
    write_csv(x = predictions_lr_test,
              file = path_output_results_test)
    
  }
  
}


