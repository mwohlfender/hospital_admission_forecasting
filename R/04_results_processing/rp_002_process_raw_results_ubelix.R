

# process raw results 

# This R-script is run on the high performance computing cluster of the University of Bern, UBELIX.


# load libraries
library(data.table)
library(dplyr)
library(readr)
library(stats)
library(stringr)



# set working directory (on UBELIX) (NEEDS TO BE ADAPTED)
setwd(dir = "DIRECTORY/")

# define path to directory of computation jobs (on UBELIX) (NEEDS TO BE ADAPTED)
directory_ubelix <- "DIRECTORY/"

# read input (determines which parameters will be used)
index <- as.numeric(commandArgs(trailingOnly = TRUE)[1])

# determine whether existing files of processed results shall be replaced (processing of results done again from scratch)
do_new_process_jobs <- FALSE


# source `setup_global_variables.R`
source("R/setup_global_variables.R")

# source R functions
path_script_R <- "R/functions/"
files_R <- dir(path = path_script_R, recursive = TRUE)
lapply(X = files_R, FUN = function(x) {source(paste0(path_script_R, x), echo = FALSE)})


# read table of computing jobs whose results shall be processed
table_jobs_process_results <- read_csv(file = paste0(Directory_Parameters, Path_Table_Jobs_Results_Processing))



if (table_jobs_process_results$type_model[index] == "lr") {
  
  process_results_lr(number_job_array = table_jobs_process_results$number_job_array[index],
                     directory_data = paste0(directory_ubelix, table_jobs_process_results$number_job_array[index], "/", Directory_Data),
                     directory_parameters = paste0(directory_ubelix, table_jobs_process_results$number_job_array[index], "/", Directory_Parameters),
                     directory_results_raw = paste0(directory_ubelix, table_jobs_process_results$number_job_array[index], "/", Directory_Results),
                     directory_results_processed = Directory_Results,
                     option_paths_data = "short_directory_filename",
                     option_paths_results_raw = "short_directory",
                     do_new = do_new_process_jobs)
  
}


if (table_jobs_process_results$type_model[index] == "lstm") {
  
  process_results_lstm(number_job_array = table_jobs_process_results$number_job_array[index],
                       directory_data = paste0(directory_ubelix, table_jobs_process_results$number_job_array[index], "/", Directory_Data),
                       directory_parameters = paste0(directory_ubelix, table_jobs_process_results$number_job_array[index], "/", Directory_Parameters),
                       directory_results_raw = paste0(directory_ubelix, table_jobs_process_results$number_job_array[index], "/", Directory_Results),
                       directory_results_processed = Directory_Results,
                       option_paths_data = "short_directory_filename",
                       option_paths_results_raw = "short_directory",
                       do_new = do_new_process_jobs)
  
}

if (table_jobs_process_results$type_model[index] == "rnn") {
  
  process_results_rnn(number_job_array = table_jobs_process_results$number_job_array[index],
                      directory_data = paste0(directory_ubelix, table_jobs_process_results$number_job_array[index], "/", Directory_Data),
                      directory_parameters = paste0(directory_ubelix, table_jobs_process_results$number_job_array[index], "/", Directory_Parameters),
                      directory_results_raw = paste0(directory_ubelix, table_jobs_process_results$number_job_array[index], "/", Directory_Results),
                      directory_results_processed = Directory_Results,
                      option_paths_data = "short_directory_filename",
                      option_paths_results_raw = "short_directory",
                      do_new = do_new_process_jobs)
  
}

if (table_jobs_process_results$type_model[index] == "xgb") {
  
  process_results_xgb(number_job_array = table_jobs_process_results$number_job_array[index],
                      directory_data = paste0(directory_ubelix, table_jobs_process_results$number_job_array[index], "/", Directory_Data),
                      directory_parameters = paste0(directory_ubelix, table_jobs_process_results$number_job_array[index], "/", Directory_Parameters),
                      directory_results_raw = paste0(directory_ubelix, table_jobs_process_results$number_job_array[index], "/", Directory_Results),
                      directory_results_processed = Directory_Results,
                      option_paths_data = "short_directory_filename",
                      option_paths_results_raw = "short_directory",
                      do_new = do_new_process_jobs)
  
}


