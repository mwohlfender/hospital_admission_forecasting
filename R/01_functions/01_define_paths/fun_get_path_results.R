

get_path_results_ubelix_raw <- function(directory_ubelix = Directory_Ubelix,
                                        number_job_array = "NONE",
                                        type_model,
                                        number_xy,
                                        number_combination_features,
                                        name_data_set,
                                        number_combination_kNp,
                                        group_dates_train_test,
                                        number_dates_train_test,
                                        number_hyp_par_grid,
                                        number_hyp_par_subgrid,
                                        ending = ".csv",
                                        option = "NONE") {
  
  if (option != "directory") {
    
    output_filename_0 <- get_file_name_model_data_params_long(type_model = type_model,
                                                              number_xy = number_xy,
                                                              number_combination_features = number_combination_features,
                                                              name_data_set = name_data_set,
                                                              number_combination_kNp = number_combination_kNp,
                                                              group_dates_train_test = group_dates_train_test,
                                                              number_dates_train_test = number_dates_train_test,
                                                              number_hyp_par_grid = number_hyp_par_grid,
                                                              number_hyp_par_subgrid = number_hyp_par_subgrid)
    
    output_filename <- paste0("results_", output_filename_0, ending)
    
  }
  
  if (option != "filename" & option != "short_directory_filename") {
    
    output_directory <- paste0(directory_ubelix, number_job_array, "/results/")
    
  }
  
  if (option == "filename") {
    
    output <- output_filename
    
  } else if (option == "directory") {
    
    output <- output_directory
    
  } else if (option == "short_directory_filename") {
    
    output <- paste0(directory_ubelix, output_filename)
    
  } else {
    
    output <- paste0(output_directory, output_filename)
    
  }
  
  return(output)
  
}


get_path_results_processed_detailed <- function(directory_results = Directory_Results,
                                                type_model,
                                                number_xy,
                                                number_combination_features,
                                                name_data_set,
                                                number_grid_combinations_kNp_dates_train_test,
                                                number_hyp_par_grid) {
  
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


get_path_results_processed_summary <- function(directory_results = Directory_Results,
                                               type_model,
                                               number_xy,
                                               number_combination_features = "NONE",
                                               name_data_set = "NONE",
                                               number_grid_combinations_kNp_dates_train_test,
                                               number_hyp_par_grid = "NONE") {
  
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


get_path_results_forecasts <- function(directory_results = Directory_Results,
                                       type_model,
                                       number_xy,
                                       number_combination_features = "NONE",
                                       name_data_set = "NONE",
                                       number_grid_combinations_kNp_dates_train_test = "NONE",
                                       number_hyp_par_grid = "NONE",
                                       metric = "NONE",
                                       type_period = "test") {
  
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
    
    if (metric != "NONE") {
      
      output <- paste0(output,
                       "train/",
                       "results_forecast_", output_filename, "_", metric, "_train.csv")
      
    } else {
      
      output <- paste0(output,
                       "train/",
                       "results_forecast_", output_filename, "_train.csv")
      
    }
    
  } else {
    
    if (metric != "NONE") {
      
      output <- paste0(output,
                       "test/",
                       "results_forecast_", output_filename, "_", metric, "_test.csv")
      
    } else {
      
      output <- paste0(output,
                       "test/",
                       "results_forecast_", output_filename, "_test.csv")
      
    }
    
  }
  
  return(output)
  
}


get_path_results_joined_forecasts <- function(directory_results = Directory_Results,
                                              type_model,
                                              number_xy,
                                              number_grid_combinations_kNp_dates_train_test,
                                              type_period = "test") {
  
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


get_path_results_detailed_lr_coefficients <- function(directory_results = Directory_Results,
                                                      number_xy,
                                                      number_combination_features,
                                                      name_data_set,
                                                      number_combination_kNp,
                                                      group_dates_train_test,
                                                      number_dates_train_test) {
  
  output_filename <- get_file_name_model_data_params_long(type_model = "lr",
                                                          number_xy = number_xy,
                                                          number_combination_features = number_combination_features,
                                                          name_data_set = name_data_set,
                                                          number_combination_kNp = number_combination_kNp,
                                                          group_dates_train_test = group_dates_train_test,
                                                          number_dates_train_test = number_dates_train_test,
                                                          number_hyp_par_grid = "NONE")
  
  output <- paste0(directory_results,
                   "lr/",
                   number_xy, "/",
                   number_combination_features, "/",
                   name_data_set, "/",
                   "coefficients/",
                   "coefficients_", output_filename, ".txt")
  
  return(output)
  
}


