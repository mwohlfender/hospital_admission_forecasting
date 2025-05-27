

# apply XGBoost model

# NEEDS TO BE ADAPTED
number_job_array <- "026"
index <- 1


# read parameters
parameters_data_kNp_dates_hyp <- read_csv(file = paste0(Directory_Parameters, Path_Grid_Combinations_Data_kNp_Dates_Hyp, number_job_array, ".csv"))[index,]


# apply function `apply_model_xgb`
apply_model_xgb(number_xy = parameters_data_kNp_dates_hyp$number_xy,
                number_combination_features = parameters_data_kNp_dates_hyp$number_combination_features,
                name_data_set = parameters_data_kNp_dates_hyp$name_data_set,
                number_combination_kNp = parameters_data_kNp_dates_hyp$number_combination_kNp,
                group_dates_train_test = parameters_data_kNp_dates_hyp$group_dates_train_test,
                number_dates_train_test = parameters_data_kNp_dates_hyp$number_dates_train_test,
                number_hyp_par_grid_xgb = parameters_data_kNp_dates_hyp$number_hyp_par_grid,
                number_hyp_par_subgrid_xgb = parameters_data_kNp_dates_hyp$number_hyp_par_subgrid,
                bool_pred_train = TRUE,
                bool_pred_test = TRUE,
                directory_parameters = Directory_Parameters,
                directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                directory_results = Directory_Results,
                option_path_results = "long")


