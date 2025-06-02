

# apply XGBoost model

# This R-script is run on the high performance computing cluster of the University of Bern, UBELIX.


# load libraries ----
library(dplyr)
library(readr)
library(stats)
library(stringr)
library(xgboost)



# NEEDS TO BE ADAPTED
number_job_array <- "001"

# set working directory (on UBELIX) (NEEDS TO BE ADAPTED)
setwd(dir = paste0("PATH", number_job_array))

# read input (determines which parameters will be used)
index <- as.numeric(commandArgs(trailingOnly = TRUE)[1])


# source `setup_global_variables.R`
source("R/setup_global_variables.R")


# source R functions
path_script_R <- "R/functions/"
files_R <- dir(path = path_script_R, recursive = TRUE)
lapply(X = files_R, FUN = function(x) {source(paste0(path_script_R, x), echo = FALSE)})


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
                directory_data = Directory_Data,
                directory_results = Directory_Results,
                option_paths_data = "short_directory_filename",
                option_path_results = "short")


