


# define parameters ----
path_scripts_R_parameters <- "R/02_data_processing/01_parameters/"
files_scripts_R_parameters <- dir(path = path_scripts_R_parameters, recursive = TRUE)
lapply(X = files_scripts_R_parameters, FUN = function(x) {source(paste0(path_scripts_R_parameters, x), echo = FALSE)})


# process raw data ----
path_scripts_R_raw_data <- "R/02_data_processing/02_raw_data/"
files_scripts_R_raw_data <- dir(path = path_scripts_R_raw_data, recursive = TRUE)
lapply(X = files_scripts_R_raw_data, FUN = function(x) {source(paste0(path_scripts_R_raw_data, x), echo = FALSE)})


# compute features ----
path_scripts_R_feature_engineering <- "R/02_data_processing/03_feature_engineering/"
files_scripts_R_feature_engineering <- dir(path = path_scripts_R_feature_engineering, recursive = TRUE)
lapply(X = files_scripts_R_feature_engineering, FUN = function(x) {source(paste0(path_scripts_R_feature_engineering, x), echo = FALSE)})


# prepare feature matrix and target variable vector for different feature sets and forecasting scenarios ----
path_scripts_R_data_prep <- "R/02_data_processing/04_data_preparation/"
files_scripts_R_data_prep <- dir(path = path_scripts_R_data_prep, recursive = TRUE)
lapply(X = files_scripts_R_data_prep, FUN = function(x) {source(paste0(path_scripts_R_data_prep, x), echo = FALSE)})


# prepare computation jobs ----
path_scripts_R_computing_jobs <- "R/02_data_processing/05_computing_jobs_preparation/"
files_scripts_R_computing_jobs <- dir(path = path_scripts_R_computing_jobs, recursive = TRUE)
lapply(X = files_scripts_R_computing_jobs, FUN = function(x) {source(paste0(path_scripts_R_computing_jobs, x), echo = FALSE)})


