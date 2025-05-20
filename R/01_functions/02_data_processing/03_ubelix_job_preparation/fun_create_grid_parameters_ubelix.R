
# create a grid of parameter values used by an array of computing jobs run on UBELIX

# `number_job_array`: Number of array of computing jobs
# `model`: Model
# `number_xy`: Index of target variable
# `number_combination_features`: Index of combination of features
# `name_data_set`: Name of data set (determined by stratification)
# `number_grid_combinations_kNp_dates_train_test`: Index of combination of parameters (k, N and p) and train-test splits
# `number_hyp_par_grid`: Index of hyperparameter grid
# `use_hyp_par_subgrids`: Indicator whether hyperparameter subgrids shall be used (results in more but smaller jobs in array)
# `do_new`: Indicator whether existing output file shall be overwritten

# Output:
# (a) Grid of combinations of parameters (k, N and p) and train-test splits supplemented by information which model, data and hyperparameters shall be used,
# is stored in `Directory_Parameters`/`Path_Grid_Combinations_Data_kNp_Dates_Hyp``number_job_array`.csv.
# The grid has twelve columns: number_xy, number_combination_features, name_data_set, number_grid_combinations_kNp_dates_train_test, 
# number_combination_kNp, k, N, p, group_dates_train_test, number_dates_train_test, number_hyp_par_grid and number_hyp_par_subgrid
# (b) List of indices of jobs in array, is stored in `Directory_Parameters`/`Path_Grid_Combinations_Data_kNp_Dates_Hyp`indices_`number_job_array`.txt.
create_grid_parameters_ubelix <- function(number_job_array,
                                          model,
                                          number_xy,
                                          number_combination_features,
                                          name_data_set,
                                          number_grid_combinations_kNp_dates_train_test,
                                          number_hyp_par_grid,
                                          use_hyp_par_subgrids,
                                          do_new = Bool_Define_Parameters_Do_New) {
  
  # determine paths of output
  path_output_grid_combinations_data_kNp_dates_hyp <- paste0(Directory_Parameters, Path_Grid_Combinations_Data_kNp_Dates_Hyp, number_job_array, ".csv")
  path_output_indices <- paste0(Directory_Parameters, Path_Grid_Combinations_Data_kNp_Dates_Hyp, "indices_", number_job_array, ".txt")
  
  if (!(file.exists(path_output_grid_combinations_data_kNp_dates_hyp)) | !(file.exists(path_output_indices)) | do_new) {
    
    grid_combinations_kNp_dates_train_test <- read_csv(file = paste0(Directory_Parameters,
                                                                     Path_Grid_Combinations_kNp_Dates_Train_Test,
                                                                     number_grid_combinations_kNp_dates_train_test, ".csv"))
    
    table_parameters_kNp <- read_csv(file = paste0(Directory_Parameters, Path_Table_Parameters_kNp))
    
    # modify `grid_combinations_kNp_dates_train_test`:
    # (a) left join `table_parameters_kNp`
    # (b) add columns `number_xy`, `number_combination_features`, `name_data_set`, `number_grid_combinations_kNp_dates_train_test` and `number_hyp_par_grid`
    grid_combinations_data_kNp_dates_hyp_0 <- grid_combinations_kNp_dates_train_test %>%
      left_join(table_parameters_kNp %>% dplyr::rename(c("number_combination_kNp" = "number"))) %>%
      mutate(number_xy = number_xy,
             number_combination_features = number_combination_features,
             name_data_set = name_data_set,
             number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test,
             number_hyp_par_grid = number_hyp_par_grid) %>%
      dplyr::select(c("number_xy",
                      "number_combination_features",
                      "name_data_set",
                      "number_grid_combinations_kNp_dates_train_test",
                      "number_combination_kNp",
                      "k",
                      "N",
                      "p",
                      "group_dates_train_test",
                      "number_dates_train_test",
                      "number_hyp_par_grid")) %>%
      dplyr::arrange(number_combination_kNp,
                     group_dates_train_test,
                     number_dates_train_test)
    
    if (use_hyp_par_subgrids) {
      
      n_hyp_par_subgrids <- length(list.files(path = paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, model, "/", number_hyp_par_grid),
                                              pattern = paste0("hyp_par_", model, "_", number_hyp_par_grid, "_param_grid_")))
      
      if (n_hyp_par_subgrids >= 1) {
        
        grid_combinations_data_kNp_dates_hyp <- grid_combinations_data_kNp_dates_hyp_0 %>%
          dplyr::slice(rep(1:n(), each = n_hyp_par_subgrids)) %>%
          mutate(number_hyp_par_subgrid = rep(1:n_hyp_par_subgrids, times = nrow(grid_combinations_data_kNp_dates_hyp_0)))
        
      } else {
        
        grid_combinations_data_kNp_dates_hyp <- grid_combinations_data_kNp_dates_hyp_0  %>%
          mutate(number_hyp_par_subgrid = 0)
        
      }
      
    } else {
      
      grid_combinations_data_kNp_dates_hyp <- grid_combinations_data_kNp_dates_hyp_0  %>%
        mutate(number_hyp_par_subgrid = 0)
      
    }
    
    write_csv(x = grid_combinations_data_kNp_dates_hyp,
              file = path_output_grid_combinations_data_kNp_dates_hyp)
    
    write.table(c(1:nrow(grid_combinations_data_kNp_dates_hyp)),
                file = path_output_indices,
                sep = "",
                row.names = FALSE,
                col.names = FALSE)
    
  }

}


