

# `option`: Either `filename`, `long_directory`, `long_directory_filename`, `short_directory` or `short_directory_filename`.
# Default: `filename`

get_path_results_raw <- function(type_model,
                                 number_xy,
                                 number_combination_features,
                                 name_data_set,
                                 number_combination_kNp = "NONE",
                                 group_dates_train_test = "NONE",
                                 number_dates_train_test = "NONE",
                                 number_hyp_par_grid = "NONE",
                                 number_hyp_par_subgrid = "NONE",
                                 directory_results = Directory_Results,
                                 type_period = "NONE",
                                 ending = ".csv",
                                 option = "filename") {
  
  if (option == "long_directory") {
    
    if (type_period != "NONE") {
      
      if (type_period == "train") {
        
        output <- paste0(directory_results, type_model, "/", number_xy, "/", number_combination_features, "/", name_data_set, "/train/")
        
      } else {
        
        output <- paste0(directory_results, type_model, "/", number_xy, "/", number_combination_features, "/", name_data_set, "/test/")
        
      }
      
    } else {
      
      output <- paste0(directory_results, type_model, "/", number_xy, "/", number_combination_features, "/", name_data_set, "/")
      
    }
    
  } else if (option == "short_directory") {
    
    output <- directory_results
    
  } else {
    
    output_filename_0 <- get_file_name_model_data_params_long(type_model = type_model,
                                                              number_xy = number_xy,
                                                              number_combination_features = number_combination_features,
                                                              name_data_set = name_data_set,
                                                              number_combination_kNp = number_combination_kNp,
                                                              group_dates_train_test = group_dates_train_test,
                                                              number_dates_train_test = number_dates_train_test,
                                                              number_hyp_par_grid = number_hyp_par_grid,
                                                              number_hyp_par_subgrid = number_hyp_par_subgrid)
    
    if (type_period != "NONE") {
      
      if (type_period == "train") {
        
        output_filename_0 <- paste0(output_filename_0, "_train")
        
      } else {
        
        output_filename_0 <- paste0(output_filename_0, "_test")
        
      }
      
    }
    
    output_filename <- paste0("results_", output_filename_0, ending)
    
    if (option == "long_directory_filename") {
      
      if (type_period != "NONE") {
        
        if (type_period == "train") {
          
          output <- paste0(directory_results, type_model, "/", number_xy, "/", number_combination_features, "/", name_data_set, "/train/", output_filename)
          
        } else {
          
          output <- paste0(directory_results, type_model, "/", number_xy, "/", number_combination_features, "/", name_data_set, "/test/", output_filename)
          
        }
        
      } else {
        
        output <- paste0(directory_results, type_model, "/", number_xy, "/", number_combination_features, "/", name_data_set, "/", output_filename)
        
      }
      
    } else if (option == "short_directory_filename") {
      
      output <- paste0(directory_results, output_filename)
      
    } else {
      
      output <- output_filename
      
    }
    
  }
  
  return(output)
  
}


# `option`: Either `filename`, `long_directory`, `long_directory_filename`, `short_directory` or `short_directory_filename`.
# Default: `filename`

get_path_results_lr_coefficients <- function(number_xy,
                                             number_combination_features,
                                             name_data_set,
                                             number_combination_kNp = "NONE",
                                             group_dates_train_test = "NONE",
                                             number_dates_train_test = "NONE",
                                             number_hyp_par_grid = "NONE",
                                             number_hyp_par_subgrid = "NONE",
                                             directory_results = Directory_Results,
                                             option = "filename",
                                             ending = ".txt") {
  
  if (option == "long_directory") {
    
    output <- paste0(directory_results, "lr/", number_xy, "/", number_combination_features, "/", name_data_set, "/coefficients/")
    
  } else if (option == "short_directory") {
    
    output <- paste0(directory_results, "coefficients/")
    
  } else {
    
    output_filename_0 <- get_file_name_model_data_params_long(type_model = "lr",
                                                              number_xy = number_xy,
                                                              number_combination_features = number_combination_features,
                                                              name_data_set = name_data_set,
                                                              number_combination_kNp = number_combination_kNp,
                                                              group_dates_train_test = group_dates_train_test,
                                                              number_dates_train_test = number_dates_train_test,
                                                              number_hyp_par_grid = number_hyp_par_grid,
                                                              number_hyp_par_subgrid = number_hyp_par_subgrid)
    
    output_filename <- paste0("coefficients_", output_filename_0, ending)
    
    if (option == "long_directory_filename") {
      
      output <- paste0(directory_results, "lr/", number_xy, "/", number_combination_features, "/", name_data_set, "/coefficients/", output_filename)
      
    } else if (option == "short_directory_filename") {
      
      output <- paste0(directory_results, "coefficients/", output_filename)
      
    } else {
      
      output <- output_filename
      
    }
    
  }
  
  return(output)
  
}


get_path_results_forecasts <- function(type_model,
                                       number_xy,
                                       number_combination_features = "NONE",
                                       name_data_set = "NONE",
                                       number_grid_combinations_kNp_dates_train_test = "NONE",
                                       number_hyp_par_grid = "NONE",
                                       type_period = "test",
                                       directory_results = Directory_Results) {
  
  output_filename <- get_file_name_model_data_params_short(type_model = type_model,
                                                           number_xy = number_xy,
                                                           number_combination_features = number_combination_features,
                                                           name_data_set = name_data_set,
                                                           number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test,
                                                           number_hyp_par_grid = number_hyp_par_grid)
  
  output <- paste0(directory_results,
                   type_model, "/",
                   number_xy, "/")
  
  if (type_model != "locf") {
    
    output <- paste0(output,
                     number_combination_features, "/",
                     name_data_set, "/")
    
  }
  
  if (type_period == "train") {
    
    output <- paste0(output,
                     "train/",
                     "results_forecast_", output_filename, "_train.csv")
    
  } else {
    
    output <- paste0(output,
                     "test/",
                     "results_forecast_", output_filename, "_test.csv")
    
  }
  
  return(output)
  
}


get_path_results_joined_forecasts <- function(type_model,
                                              number_xy,
                                              number_grid_combinations_kNp_dates_train_test,
                                              type_period = "test",
                                              directory_results = Directory_Results) {
  
  output_filename <- get_file_name_model_data_params_short(type_model = type_model,
                                                           number_xy = number_xy,
                                                           number_combination_features = "NONE",
                                                           name_data_set = "NONE",
                                                           number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test,
                                                           number_hyp_par_grid = "NONE")
  
  output <- paste0(directory_results,
                   type_model, "/",
                   number_xy, "/",
                   "results_joined_forecasts_", output_filename, "_", type_period, ".csv")
  
  return(output)
  
}


get_path_results_processed_detailed <- function(type_model,
                                                number_xy,
                                                number_combination_features,
                                                name_data_set,
                                                number_grid_combinations_kNp_dates_train_test,
                                                number_hyp_par_grid,
                                                directory_results = Directory_Results) {
  
  output_filename <- get_file_name_model_data_params_short(type_model = type_model,
                                                           number_xy = number_xy,
                                                           number_combination_features = number_combination_features,
                                                           name_data_set = name_data_set,
                                                           number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test,
                                                           number_hyp_par_grid = number_hyp_par_grid)
  
  output <- paste0(directory_results,
                   type_model, "/",
                   number_xy, "/",
                   number_combination_features, "/",
                   name_data_set, "/",
                   "results_detailed_", output_filename, ".csv")
  
  return(output)
  
}


get_path_results_processed_summary <- function(type_model,
                                               number_xy,
                                               number_combination_features = "NONE",
                                               name_data_set = "NONE",
                                               number_grid_combinations_kNp_dates_train_test,
                                               number_hyp_par_grid = "NONE",
                                               directory_results = Directory_Results) {
  
  output_filename <- get_file_name_model_data_params_short(type_model = type_model,
                                                           number_xy = number_xy,
                                                           number_combination_features = number_combination_features,
                                                           name_data_set = name_data_set,
                                                           number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test,
                                                           number_hyp_par_grid = number_hyp_par_grid)
  
  if (type_model == "locf") {
    
    output <- paste0(directory_results,
                     type_model, "/",
                     number_xy, "/",
                     "results_summary_", output_filename, ".csv")
    
  } else {
    
    output <- paste0(directory_results,
                     type_model, "/",
                     number_xy, "/",
                     number_combination_features, "/",
                     name_data_set, "/",
                     "results_summary_", output_filename, ".csv")
    
  }
  
  return(output)
  
}


