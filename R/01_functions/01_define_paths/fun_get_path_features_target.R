

# `option_output`: Either "features_target", "features_train", "features_test", "target_train" or "target_test"
# Default: "features_target"

# `option_path`: Either "filename", "long_directory", "long_directory_filename", "short_directory" or "short_directory_filename".
# Default: "filename"

get_path_features_target <- function(number_xy,
                                     number_combination_features,
                                     name_data_set,
                                     number_combination_kNp = "NONE",
                                     group_dates_train_test = "NONE",
                                     number_dates_train_test = "NONE",
                                     option_output = "features_target",
                                     directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                     option_path = "filename") {
  
  
  if (option_output %in% c("features_train", "features_test", "target_train", "target_test")) {
    
    output_filename_0 <- get_file_name_model_data_params_long(type_model = "NONE",
                                                              number_xy = number_xy,
                                                              number_combination_features = number_combination_features,
                                                              name_data_set = name_data_set,
                                                              number_combination_kNp = number_combination_kNp,
                                                              group_dates_train_test = group_dates_train_test,
                                                              number_dates_train_test = number_dates_train_test,
                                                              number_hyp_par_grid = "NONE",
                                                              number_hyp_par_subgrid = "NONE")
    
  } else {
    
    output_filename_0 <- get_file_name_model_data_params_short(type_model = "NONE",
                                                               number_xy = number_xy,
                                                               number_combination_features = number_combination_features,
                                                               name_data_set = name_data_set,
                                                               number_grid_combinations_kNp_dates_train_test = "NONE",
                                                               number_hyp_par_grid = "NONE")
    
  }
  
  
  if (option_output == "features_train") {
    
    output_filename <- paste0("features_train_", output_filename_0, ".csv")
    
    output_directory <- paste0("features/train/")
    
  } else if (option_output == "features_test") {
    
    output_filename <- paste0("features_test_", output_filename_0, ".csv")
    
    output_directory <- paste0("features/test/")
    
  } else if (option_output == "target_train") {
    
    output_filename <- paste0("target_train_", output_filename_0, ".csv")
    
    output_directory <- paste0("target/train/")
    
  } else if (option_output == "target_test") {
    
    output_filename <- paste0("target_test_", output_filename_0, ".csv")
    
    output_directory <- paste0("target/test/")
    
  } else {
    
    output_filename <- paste0("features_target_", output_filename_0, ".csv")
    
    output_directory <- ""
    
  }
  
  
  if (option_path == "long_directory") {
    
    output <- paste0(directory_data, number_xy, "/", number_combination_features, "/", name_data_set, "/", output_directory)
    
  } else if (option_path == "long_directory_filename") {
    
    output <- paste0(directory_data, number_xy, "/", number_combination_features, "/", name_data_set, "/", output_directory, output_filename)
    
  } else if (option_path == "short_directory") {
    
    output <- paste0(directory_data, output_directory)
    
  } else if (option_path == "short_directory_filename") {
    
    output <- paste0(directory_data, output_directory, output_filename)
    
  } else {
    
    output <- output_filename
    
  }
  
  return(output)
  
}


