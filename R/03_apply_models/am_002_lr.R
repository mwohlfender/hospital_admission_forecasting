

# apply linear regression model

list_jobs_apply_lr <- c("005", "006", "007", "008", "009",
                        "010", "011", "012", "013", "014",
                        "015", "016", "017", "018", "019",
                        "020", "021", "022", "023", "024",
                        "025")


for (ii in 1:length(list_jobs_apply_lr)) {
  
  grid_combinations_data_kNp_dates_hyp_ii <- read_csv(file = paste0(Directory_Parameters, Path_Grid_Combinations_Data_kNp_Dates_Hyp, list_jobs_apply_lr[ii], ".csv"))
  
  for (jj in 1:nrow(grid_combinations_data_kNp_dates_hyp_ii)) {
    
    apply_model_lr(number_xy = grid_combinations_data_kNp_dates_hyp_ii$number_xy[jj],
                   number_combination_features = grid_combinations_data_kNp_dates_hyp_ii$number_combination_features[jj],
                   name_data_set = grid_combinations_data_kNp_dates_hyp_ii$name_data_set[jj],
                   number_combination_kNp = grid_combinations_data_kNp_dates_hyp_ii$number_combination_kNp[jj],
                   group_dates_train_test = grid_combinations_data_kNp_dates_hyp_ii$group_dates_train_test[jj],
                   number_dates_train_test = grid_combinations_data_kNp_dates_hyp_ii$number_dates_train_test[jj],
                   number_hyp_par_grid_lr = grid_combinations_data_kNp_dates_hyp_ii$number_hyp_par_grid[jj],
                   number_hyp_par_subgrid_lr = grid_combinations_data_kNp_dates_hyp_ii$number_hyp_par_subgrid[jj],
                   directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                   directory_results = Directory_Results,
                   option_path = "long",
                   do_new = Bool_Apply_Models_Do_New)
    
  }
  
}


