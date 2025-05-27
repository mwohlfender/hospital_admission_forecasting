

get_path_data_train <- function(number_xy,
                                number_combination_features,
                                name_data_set,
                                number_combination_kNp,
                                group_dates_train_test,
                                number_dates_train_test,
                                directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                option = "NONE") {
  
  if (option == "filename") {
    
    output_filename_0 <- get_file_name_model_data_params_long(type_model = "NONE",
                                                              number_xy = number_xy,
                                                              number_combination_features = number_combination_features,
                                                              name_data_set = name_data_set,
                                                              number_combination_kNp = number_combination_kNp,
                                                              group_dates_train_test = group_dates_train_test,
                                                              number_dates_train_test = number_dates_train_test,
                                                              number_hyp_par_grid = "NONE",
                                                              number_hyp_par_subgrid = "NONE")
    
    output_filename <- paste0("data_train_", output_filename_0, ".csv")
    
    return(output_filename)
    
  } else if (option == "directory") {
    
    output_directory <- paste0(number_xy, "/", number_combination_features, "/", name_data_set, "/")
    
    output <- paste0(directory_data,
                     output_directory)
    
    return(output)
    
  } else if (option == "short_directory_filename") {
    
    output_filename_0 <- get_file_name_model_data_params_long(type_model = "NONE",
                                                              number_xy = number_xy,
                                                              number_combination_features = number_combination_features,
                                                              name_data_set = name_data_set,
                                                              number_combination_kNp = number_combination_kNp,
                                                              group_dates_train_test = group_dates_train_test,
                                                              number_dates_train_test = number_dates_train_test,
                                                              number_hyp_par_grid = "NONE",
                                                              number_hyp_par_subgrid = "NONE")
    
    output_filename <- paste0("data_train_", output_filename_0, ".csv")
    
    output <- paste0(directory_data,
                     output_filename)
    
    return(output)
    
  } else {
    
    output_directory <- paste0(number_xy, "/", number_combination_features, "/", name_data_set, "/")
    
    output_filename_0 <- get_file_name_model_data_params_long(type_model = "NONE",
                                                              number_xy = number_xy,
                                                              number_combination_features = number_combination_features,
                                                              name_data_set = name_data_set,
                                                              number_combination_kNp = number_combination_kNp,
                                                              group_dates_train_test = group_dates_train_test,
                                                              number_dates_train_test = number_dates_train_test,
                                                              number_hyp_par_grid = "NONE",
                                                              number_hyp_par_subgrid = "NONE")
    
    output_filename <- paste0("data_train_", output_filename_0, ".csv")
    
    output <- paste0(directory_data,
                     output_directory,
                     output_filename)
    
    return(output)
    
  }
  
}


get_path_data_test <- function(number_xy,
                               number_combination_features,
                               name_data_set,
                               number_combination_kNp,
                               group_dates_train_test,
                               number_dates_train_test,
                               directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                               option = "NONE") {
  
  if (option == "filename") {
    
    output_filename_0 <- get_file_name_model_data_params_long(type_model = "NONE",
                                                              number_xy = number_xy,
                                                              number_combination_features = number_combination_features,
                                                              name_data_set = name_data_set,
                                                              number_combination_kNp = number_combination_kNp,
                                                              group_dates_train_test = group_dates_train_test,
                                                              number_dates_train_test = number_dates_train_test,
                                                              number_hyp_par_grid = "NONE",
                                                              number_hyp_par_subgrid = "NONE")
    
    output_filename <- paste0("data_test_", output_filename_0, ".csv")
    
    return(output_filename)
    
  } else if (option == "directory") {
    
    output_directory <- paste0(number_xy, "/", number_combination_features, "/", name_data_set, "/")
    
    output <- paste0(directory_data,
                     output_directory)
    
    return(output)
    
  } else if (option == "short_directory_filename") {
    
    output_filename_0 <- get_file_name_model_data_params_long(type_model = "NONE",
                                                              number_xy = number_xy,
                                                              number_combination_features = number_combination_features,
                                                              name_data_set = name_data_set,
                                                              number_combination_kNp = number_combination_kNp,
                                                              group_dates_train_test = group_dates_train_test,
                                                              number_dates_train_test = number_dates_train_test,
                                                              number_hyp_par_grid = "NONE",
                                                              number_hyp_par_subgrid = "NONE")
    
    output_filename <- paste0("data_test_", output_filename_0, ".csv")
    
    output <- paste0(directory_data,
                     output_filename)
    
    return(output)
    
  } else {
    
    output_directory <- paste0(number_xy, "/", number_combination_features, "/", name_data_set, "/")
    
    output_filename_0 <- get_file_name_model_data_params_long(type_model = "NONE",
                                                              number_xy = number_xy,
                                                              number_combination_features = number_combination_features,
                                                              name_data_set = name_data_set,
                                                              number_combination_kNp = number_combination_kNp,
                                                              group_dates_train_test = group_dates_train_test,
                                                              number_dates_train_test = number_dates_train_test,
                                                              number_hyp_par_grid = "NONE",
                                                              number_hyp_par_subgrid = "NONE")
    
    output_filename <- paste0("data_test_", output_filename_0, ".csv")
    
    output <- paste0(directory_data,
                     output_directory,
                     output_filename)
    
    return(output)
    
  }
  
}


get_path_data_features <- function(number_xy,
                                   number_combination_features,
                                   name_data_set,
                                   directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                   option = "NONE") {
  
  if (option == "filename") {
    
    output_filename_0 <- get_file_name_model_data_params_short(type_model = "NONE",
                                                               number_xy = number_xy,
                                                               number_combination_features = number_combination_features,
                                                               name_data_set = name_data_set,
                                                               number_grid_combinations_kNp_dates_train_test = "NONE",
                                                               number_hyp_par_grid = "NONE")
    
    output_filename <- paste0("data_features_", output_filename_0, ".csv")
    
    return(output_filename)
    
  } else if (option == "directory") {
    
    output_directory <- paste0(number_xy, "/", number_combination_features, "/", name_data_set, "/")
    
    output <- paste0(directory_data,
                     output_directory)
    
    return(output)
    
  } else if (option == "short_directory_filename") {
    
    output_filename_0 <- get_file_name_model_data_params_short(type_model = "NONE",
                                                               number_xy = number_xy,
                                                               number_combination_features = number_combination_features,
                                                               name_data_set = name_data_set,
                                                               number_grid_combinations_kNp_dates_train_test = "NONE",
                                                               number_hyp_par_grid = "NONE")
    
    output_filename <- paste0("data_features_", output_filename_0, ".csv")
    
    output <- paste0(directory_data,
                     output_filename)
    
    return(output)
    
  } else {
    
    output_directory <- paste0(number_xy, "/", number_combination_features, "/", name_data_set, "/")
    
    output_filename_0 <- get_file_name_model_data_params_short(type_model = "NONE",
                                                               number_xy = number_xy,
                                                               number_combination_features = number_combination_features,
                                                               name_data_set = name_data_set,
                                                               number_grid_combinations_kNp_dates_train_test = "NONE",
                                                               number_hyp_par_grid = "NONE")
    
    output_filename <- paste0("data_features_", output_filename_0, ".csv")
    
    output <- paste0(directory_data,
                     output_directory,
                     output_filename)
    
    return(output)
    
  }
  
}


get_path_target_train <- function(number_xy,
                                  number_combination_features,
                                  name_data_set,
                                  number_combination_kNp,
                                  group_dates_train_test,
                                  number_dates_train_test,
                                  directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                  option = "NONE") {
  
  if (option == "filename") {
    
    output_filename_0 <- get_file_name_model_data_params_long(type_model = "NONE",
                                                              number_xy = number_xy,
                                                              number_combination_features = number_combination_features,
                                                              name_data_set = name_data_set,
                                                              number_combination_kNp = number_combination_kNp,
                                                              group_dates_train_test = group_dates_train_test,
                                                              number_dates_train_test = number_dates_train_test,
                                                              number_hyp_par_grid = "NONE",
                                                              number_hyp_par_subgrid = "NONE")
    
    output_filename <- paste0("target_train_", output_filename_0, ".csv")
    
    return(output_filename)
    
  } else if (option == "directory") {
    
    output_directory <- paste0(number_xy, "/", number_combination_features, "/", name_data_set, "/")
    
    output <- paste0(directory_data,
                     output_directory)
    
    return(output)
    
  } else if (option == "short_directory_filename") {
    
    output_filename_0 <- get_file_name_model_data_params_long(type_model = "NONE",
                                                              number_xy = number_xy,
                                                              number_combination_features = number_combination_features,
                                                              name_data_set = name_data_set,
                                                              number_combination_kNp = number_combination_kNp,
                                                              group_dates_train_test = group_dates_train_test,
                                                              number_dates_train_test = number_dates_train_test,
                                                              number_hyp_par_grid = "NONE",
                                                              number_hyp_par_subgrid = "NONE")
    
    output_filename <- paste0("target_train_", output_filename_0, ".csv")
    
    output <- paste0(directory_data,
                     output_filename)
    
    return(output)
    
  } else {
    
    output_directory <- paste0(number_xy, "/", number_combination_features, "/", name_data_set, "/")
    
    output_filename_0 <- get_file_name_model_data_params_long(type_model = "NONE",
                                                              number_xy = number_xy,
                                                              number_combination_features = number_combination_features,
                                                              name_data_set = name_data_set,
                                                              number_combination_kNp = number_combination_kNp,
                                                              group_dates_train_test = group_dates_train_test,
                                                              number_dates_train_test = number_dates_train_test,
                                                              number_hyp_par_grid = "NONE",
                                                              number_hyp_par_subgrid = "NONE")
    
    output_filename <- paste0("target_train_", output_filename_0, ".csv")
    
    output <- paste0(directory_data,
                     output_directory,
                     output_filename)
    
    return(output)
    
  }
  
}


get_path_target_test <- function(number_xy,
                                 number_combination_features,
                                 name_data_set,
                                 number_combination_kNp,
                                 group_dates_train_test,
                                 number_dates_train_test,
                                 directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                 option = "NONE") {
  
  if (option == "filename") {
    
    output_filename_0 <- get_file_name_model_data_params_long(type_model = "NONE",
                                                              number_xy = number_xy,
                                                              number_combination_features = number_combination_features,
                                                              name_data_set = name_data_set,
                                                              number_combination_kNp = number_combination_kNp,
                                                              group_dates_train_test = group_dates_train_test,
                                                              number_dates_train_test = number_dates_train_test,
                                                              number_hyp_par_grid = "NONE",
                                                              number_hyp_par_subgrid = "NONE")
    
    output_filename <- paste0("target_test_", output_filename_0, ".csv")
    
    return(output_filename)
    
  } else if (option == "directory") {
    
    output_directory <- paste0(number_xy, "/", number_combination_features, "/", name_data_set, "/")
    
    output <- paste0(directory_data,
                     output_directory)
    
    return(output)
    
  } else if (option == "short_directory_filename") {
    
    output_filename_0 <- get_file_name_model_data_params_long(type_model = "NONE",
                                                              number_xy = number_xy,
                                                              number_combination_features = number_combination_features,
                                                              name_data_set = name_data_set,
                                                              number_combination_kNp = number_combination_kNp,
                                                              group_dates_train_test = group_dates_train_test,
                                                              number_dates_train_test = number_dates_train_test,
                                                              number_hyp_par_grid = "NONE",
                                                              number_hyp_par_subgrid = "NONE")
    
    output_filename <- paste0("target_test_", output_filename_0, ".csv")
    
    output <- paste0(directory_data,
                     output_filename)
    
    return(output)
    
  } else {
    
    output_directory <- paste0(number_xy, "/", number_combination_features, "/", name_data_set, "/")
    
    output_filename_0 <- get_file_name_model_data_params_long(type_model = "NONE",
                                                              number_xy = number_xy,
                                                              number_combination_features = number_combination_features,
                                                              name_data_set = name_data_set,
                                                              number_combination_kNp = number_combination_kNp,
                                                              group_dates_train_test = group_dates_train_test,
                                                              number_dates_train_test = number_dates_train_test,
                                                              number_hyp_par_grid = "NONE",
                                                              number_hyp_par_subgrid = "NONE")
    
    output_filename <- paste0("target_test_", output_filename_0, ".csv")
    
    output <- paste0(directory_data,
                     output_directory,
                     output_filename)
    
    return(output)
    
  }
  
}


