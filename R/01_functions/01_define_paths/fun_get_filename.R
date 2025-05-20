
# definition of file names
# two functions standardizing file names, e.g., of data and results files,
# depending on model, feature set and parameters (k, N, p, train-test splits and hyperparameters)


# `get_file_name_model_data_params_short`:
# purpose: name of files containing information of one model, one feature set and multiple combinations of parameters
# inputs:
# `type_model`: model
# `number_xy`: number of combination of type of features and target variable
# `number_combination_features`: number of feature set
# `name_data_set`: name of feature set (e.g., indicating stratification)
# `number_grid_combinations_kNp_dates_train_test`: number of grid of combinations of parameters k, N and p and train-test splits
# `number_hyp_par_grid`: number of grid of combinations of hyperparameter
# output:
# `type_model`_XY_`number_xy`_`number_combination_features`_`name_data_set`_PD_`number_grid_combinations_kNp_dates_train_test`_H_`number_hyp_par_grid`
get_file_name_model_data_params_short <- function(type_model = "NONE",
                                                  number_xy = "NONE",
                                                  number_combination_features = "NONE",
                                                  name_data_set = "NONE",
                                                  number_grid_combinations_kNp_dates_train_test = "NONE",
                                                  number_hyp_par_grid = "NONE") {
  
  output <- ""
  
  if (type_model != "NONE") {
    
    output <- type_model
    
  }
  
  if (number_xy != "NONE") {
    
    output <- paste0(output, "_XY_", number_xy)
    
  }
  
  if (number_combination_features != "NONE") {
    
    output <- paste0(output, "_", number_combination_features)
    
  }
  
  if (name_data_set != "NONE") {
    
    output <- paste0(output, "_", name_data_set)
    
  }
  
  if (number_grid_combinations_kNp_dates_train_test != "NONE") {
    
    output <- paste0(output, "_PD_", number_grid_combinations_kNp_dates_train_test)
    
  }
  
  if (number_hyp_par_grid != "NONE") {
    
    output <- paste0(output, "_H_", number_hyp_par_grid)
    
  }
  
  if (substr(x = output, start = 1, stop = 1) == "_") {
    
    output <- substr(output, start = 2, stop = nchar(output))
    
  }
  
  return(output)
  
}


# `get_file_name_model_data_params_long`:
# purpose: name of files containing information of one model, one feature set and one combination of parameters
# inputs:
# `type_model`: model
# `number_xy`: number of combination of type of features and target variable
# `number_combination_features`: number of feature set
# `name_data_set`: name of feature set (e.g., indicating stratification)
# `number_combination_kNp`: number of grid of combinations of parameters k, N and p
# `group_dates_train_test`: number of group of train-test splits
# `number_dates_train_test`: number of train-test split (within group of train-test splits `group_dates_train_test`)
# `number_hyp_par_grid`: number of grid of combinations of hyperparameter
# `number_hyp_par_subgrid`: number of subgrid of grid of combinations of hyperparameter
# output:
# `type_model`_XY_`number_xy`_`number_combination_features`_`name_data_set`_P_`number_combination_kNp`_
# D_`group_dates_train_test`_`number_dates_train_test`_H_`number_hyp_par_grid`_`number_hyp_par_subgrid`
get_file_name_model_data_params_long <- function(type_model = "NONE",
                                                 number_xy = "NONE",
                                                 number_combination_features = "NONE",
                                                 name_data_set = "NONE",
                                                 number_combination_kNp = "NONE",
                                                 group_dates_train_test = "NONE",
                                                 number_dates_train_test = "NONE",
                                                 number_hyp_par_grid = "NONE",
                                                 number_hyp_par_subgrid = "NONE") {
  
  output <- ""
  
  if (type_model != "NONE") {
    
    output <- type_model
    
  }
  
  if (number_xy != "NONE") {
    
    output <- paste0(output, "_XY_", number_xy)
    
  }
  
  if (number_combination_features != "NONE") {
    
    output <- paste0(output, "_", number_combination_features)
    
  }
  
  if (name_data_set != "NONE") {
    
    output <- paste0(output, "_", name_data_set)
    
  }
  
  if (number_combination_kNp != "NONE") {
    
    output <- paste0(output, "_P_", number_combination_kNp)
    
  }
  
  if (group_dates_train_test != "NONE") {
    
    output <- paste0(output, "_D_", group_dates_train_test)
    
  }
  
  if (number_dates_train_test != "NONE") {
    
    output <- paste0(output, "_", number_dates_train_test)
    
  }
  
  if (number_hyp_par_grid != "NONE") {
    
    output <- paste0(output, "_H_", number_hyp_par_grid)
    
  }
  
  if (number_hyp_par_subgrid != "NONE") {
    
    output <- paste0(output, "_", number_hyp_par_subgrid)
    
  }
  
  if (substr(x = output, start = 1, stop = 1) == "_") {
    
    output <- substr(output, start = 2, stop = nchar(output))
    
  }
  
  return(output)
  
}


