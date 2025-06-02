

computing_job_copy_data <- function(directory_computing_jobs = Directory_Computing_Jobs,
                                    number_job_array,
                                    type_model,
                                    directory_data = Subdirectory_Jobs_Data,
                                    directory_parameters = Subdirectory_Jobs_Parameters,
                                    directory_results = Subdirectory_Jobs_Results,
                                    overwrite_files = FALSE) {
  
  # read parameter grid of job array
  grid_combinations_data_kNp_dates_hyp <- read_csv(file = paste0(Directory_Parameters, Path_Grid_Combinations_Data_kNp_Dates_Hyp, number_job_array, ".csv"))
  
  
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
  
  # folder where grid of hyperparameters is stored
  current_folder_hyper_parameters <- paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, type_model, "/")
  
  # folders where data will be copied to
  new_folder <- paste0(directory_computing_jobs, number_job_array, "/")
  new_folder_data <- paste0(new_folder, directory_data)
  new_folder_data_features_train <- paste0(new_folder, directory_data, "features/train/")
  new_folder_data_features_test <- paste0(new_folder, directory_data, "features/test/")
  new_folder_data_target_train <- paste0(new_folder, directory_data, "target/train/")
  new_folder_data_target_test <- paste0(new_folder, directory_data, "target/test/")
  new_folder_parameters <- paste0(new_folder, directory_parameters)
  new_folder_parameters_hyperparameters <- paste0(new_folder_parameters, Subdirectory_Parameters_Hyperparameters, type_model, "/", number_hyp_par_grid, "/")
  new_folder_parameters_job_setup <- paste0(new_folder_parameters, Subdirectory_Parameters_Job_Setup)
  new_folder_results <- paste0(new_folder, directory_results)
  new_folder_results_train <- paste0(new_folder, directory_results, "train/")
  new_folder_results_test <- paste0(new_folder, directory_results, "test/")
  
  # create directories to store output (if they do not already exist)
  if (!(file.exists(paths = new_folder))) {
    
    dir.create(path = file.path(new_folder))
    
  }
  
  if (!(file.exists(paths = new_folder_data))) {
    
    dir.create(path = file.path(new_folder_data))
    
  }
  
  if (!(file.exists(paths = new_folder_data_features_train))) {
    
    dir.create(path = file.path(new_folder_data_features_train), recursive = TRUE)
    
  }
  
  if (!(file.exists(paths = new_folder_data_features_test))) {
    
    dir.create(path = file.path(new_folder_data_features_test), recursive = TRUE)
    
  }
  
  if (!(file.exists(paths = new_folder_data_target_train))) {
    
    dir.create(path = file.path(new_folder_data_target_train), recursive = TRUE)
    
  }
  
  if (!(file.exists(paths = new_folder_data_target_test))) {
    
    dir.create(path = file.path(new_folder_data_target_test), recursive = TRUE)
    
  }
  
  if (!(file.exists(paths = paste0(new_folder, "outfiles/")))) {
    
    dir.create(path = file.path(paste0(new_folder, "outfiles/")))
    
  }
  
  if (!(file.exists(paths = new_folder_parameters))) {
    
    dir.create(path = file.path(new_folder_parameters))
    
  }
  
  if (!(file.exists(paths = new_folder_parameters_hyperparameters))) {
    
    dir.create(path = file.path(new_folder_parameters_hyperparameters), recursive = TRUE)
    
  }
  
  if (!(file.exists(paths = new_folder_parameters_job_setup))) {
    
    dir.create(path = file.path(new_folder_parameters_job_setup))
    
  }
  
  if (!(file.exists(paths = paste0(new_folder, "python/functions/")))) {
    
    dir.create(path = file.path(paste0(new_folder, "python/functions/")), recursive = TRUE)
    
  }
  
  if (!(file.exists(paths = paste0(new_folder, "R/functions/")))) {
    
    dir.create(path = file.path(paste0(new_folder, "R/functions/")), recursive = TRUE)
    
  }
  
  if (!(file.exists(paths = new_folder_results))) {
    
    dir.create(path = file.path(new_folder_results))
    
  }
  
  if (!(file.exists(paths = new_folder_results_train))) {
    
    dir.create(path = file.path(new_folder_results_train))
    
  }
  
  if (!(file.exists(paths = new_folder_results_test))) {
    
    dir.create(path = file.path(new_folder_results_test))
    
  }
  
  
  # copy parameter files
  
  # models
  file.copy(from = paste0(Directory_Parameters, Subdirectory_Parameters_Models),
            to = new_folder_parameters,
            overwrite = overwrite_files,
            recursive = TRUE)
  
  # datasets
  file.copy(from = paste0(Directory_Parameters, Subdirectory_Parameters_Datasets),
            to = new_folder_parameters,
            overwrite = overwrite_files,
            recursive = TRUE)
  
  # parameters kNp and dates
  file.copy(from = paste0(Directory_Parameters, Subdirectory_Parameters_kNp_Dates_Train_Test),
            to = new_folder_parameters,
            overwrite = overwrite_files,
            recursive = TRUE)
  
  file.remove(grep(list.files(path = paste0(new_folder_parameters, Subdirectory_Parameters_kNp_Dates_Train_Test, Subsubdirectory_Parameters_kNp_Dates_Train_Test_Grids), full.names = TRUE),
                   pattern = paste0("grid_combinations_kNp_dates_train_test_", number_grid_combinations_kNp_dates_train_test), invert = TRUE, value = TRUE))
  
  # copy `grid_combinations_data_kNp_dates_hyp`
  file.copy(from = paste0(Directory_Parameters, Path_Grid_Combinations_Data_kNp_Dates_Hyp, number_job_array, ".csv"),
            to = paste0(new_folder_parameters, Subdirectory_Parameters_Job_Setup),
            overwrite = overwrite_files)
  
  # copy list of indices for parallel jobs
  file.copy(from = paste0(Directory_Parameters, Path_Grid_Combinations_Data_kNp_Dates_Hyp, "indices_", number_job_array, ".txt"),
            to = paste0(directory_computing_jobs, number_job_array, "/"),
            overwrite = overwrite_files)
  
  
  # copy grid of hyperparameters
  if (0 %in% unique(grid_combinations_data_kNp_dates_hyp$number_hyp_par_subgrid)) {
    
    file.copy(from = paste0(current_folder_hyper_parameters, number_hyp_par_grid, "/",
                            "hyp_par_", type_model, "_", number_hyp_par_grid, "_param_grid.csv"),
              to = new_folder_parameters_hyperparameters,
              overwrite = overwrite_files)
    
  } else {
    
    if (overwrite_files) {
      
      file.remove(list.files(path = new_folder_parameters_hyperparameters, pattern = "hyp_par_", full.names = TRUE))
      
    }
    
    for (ii in unique(grid_combinations_data_kNp_dates_hyp$number_hyp_par_subgrid)) {
      
      file.copy(from = paste0(current_folder_hyper_parameters, number_hyp_par_grid, "/",
                              "hyp_par_", type_model, "_", number_hyp_par_grid,
                              "_param_grid_", ii,".csv"),
                to = new_folder_parameters_hyperparameters,
                overwrite = overwrite_files)
      
    }
    
  }
  
  
  # copy training / testing features and target variable
  file.copy(from = get_path_features_target(number_xy = number_xy,
                                            number_combination_features = number_combination_features,
                                            name_data_set = name_data_set,
                                            option_output = "features_target",
                                            directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                            option_path = "long_directory_filename"),
            to = new_folder_data,
            overwrite = overwrite_files)
  
  
  for (ii in 1:nrow(grid_combinations_data_kNp_dates_hyp)) {
    
    file.copy(from = get_path_features_target(number_xy = number_xy,
                                              number_combination_features = number_combination_features,
                                              name_data_set = name_data_set,
                                              number_combination_kNp = grid_combinations_data_kNp_dates_hyp$number_combination_kNp[ii],
                                              group_dates_train_test = grid_combinations_data_kNp_dates_hyp$group_dates_train_test[ii],
                                              number_dates_train_test = grid_combinations_data_kNp_dates_hyp$number_dates_train_test[ii],
                                              option_output = "features_train",
                                              directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                              option_path = "long_directory_filename"),
              to = paste0(new_folder_data, "features/train/"),
              overwrite = overwrite_files)
    
    file.copy(from = get_path_features_target(number_xy = number_xy,
                                              number_combination_features = number_combination_features,
                                              name_data_set = name_data_set,
                                              number_combination_kNp = grid_combinations_data_kNp_dates_hyp$number_combination_kNp[ii],
                                              group_dates_train_test = grid_combinations_data_kNp_dates_hyp$group_dates_train_test[ii],
                                              number_dates_train_test = grid_combinations_data_kNp_dates_hyp$number_dates_train_test[ii],
                                              option_output = "features_test",
                                              directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                              option_path = "long_directory_filename"),
              to = paste0(new_folder_data, "features/test/"),
              overwrite = overwrite_files)
    
    file.copy(from = get_path_features_target(number_xy = number_xy,
                                              number_combination_features = number_combination_features,
                                              name_data_set = name_data_set,
                                              number_combination_kNp = grid_combinations_data_kNp_dates_hyp$number_combination_kNp[ii],
                                              group_dates_train_test = grid_combinations_data_kNp_dates_hyp$group_dates_train_test[ii],
                                              number_dates_train_test = grid_combinations_data_kNp_dates_hyp$number_dates_train_test[ii],
                                              option_output = "target_train",
                                              directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                              option_path = "long_directory_filename"),
              to = paste0(new_folder_data, "target/train/"),
              overwrite = overwrite_files)
    
    file.copy(from = get_path_features_target(number_xy = number_xy,
                                              number_combination_features = number_combination_features,
                                              name_data_set = name_data_set,
                                              number_combination_kNp = grid_combinations_data_kNp_dates_hyp$number_combination_kNp[ii],
                                              group_dates_train_test = grid_combinations_data_kNp_dates_hyp$group_dates_train_test[ii],
                                              number_dates_train_test = grid_combinations_data_kNp_dates_hyp$number_dates_train_test[ii],
                                              option_output = "target_test",
                                              directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                              option_path = "long_directory_filename"),
              to = paste0(new_folder_data, "target/test/"),
              overwrite = overwrite_files)
    
  }
  
}


