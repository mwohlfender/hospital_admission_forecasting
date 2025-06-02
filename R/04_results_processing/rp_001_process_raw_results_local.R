

# process raw results 

# read table of computing jobs whose results shall be processed
table_jobs_process_results <- read_csv(file = paste0(Directory_Parameters, Path_Table_Jobs_Results_Processing))


# process results of jobs
for (ii in 1:nrow(table_jobs_process_results)) {
  
  if (table_jobs_process_results$type_model[ii] == "lr") {
    
    process_results_lr(number_job_array = table_jobs_process_results$number_job_array[ii],
                       directory_data = paste0(Directory_Computing_Jobs, table_jobs_process_results$number_job_array[ii], "/", Subdirectory_Jobs_Data),
                       directory_parameters = paste0(Directory_Computing_Jobs, table_jobs_process_results$number_job_array[ii], "/", Subdirectory_Jobs_Parameters),
                       directory_results_raw = paste0(Directory_Computing_Jobs, table_jobs_process_results$number_job_array[ii], "/", Subdirectory_Jobs_Results),
                       directory_results_processed = Directory_Results,
                       option_paths_data = "short",
                       option_paths_results_raw = "short",
                       do_new = Bool_Results_Processing_Do_New)
    
  }
  
  
  if (table_jobs_process_results$type_model[ii] == "lstm") {
    
    process_results_lstm(number_job_array = table_jobs_process_results$number_job_array[ii],
                         directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                         directory_parameters = paste0(Directory_Computing_Jobs, table_jobs_process_results$number_job_array[ii], "/", Subdirectory_Jobs_Parameters),
                         directory_results_raw = paste0(Directory_Computing_Jobs, table_jobs_process_results$number_job_array[ii], "/", Subdirectory_Jobs_Results),
                         directory_results_processed = Directory_Results,
                         option_paths_data = "short",
                         option_paths_results_raw = "short",
                         do_new = Bool_Results_Processing_Do_New)
    
  }
  
  
  if (table_jobs_process_results$type_model[ii] == "rnn") {
    
    process_results_rnn(number_job_array = table_jobs_process_results$number_job_array[ii],
                        directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                        directory_parameters = paste0(Directory_Computing_Jobs, table_jobs_process_results$number_job_array[ii], "/", Subdirectory_Jobs_Parameters),
                        directory_results_raw = paste0(Directory_Computing_Jobs, table_jobs_process_results$number_job_array[ii], "/", Subdirectory_Jobs_Results),
                        directory_results_processed = Directory_Results,
                        option_paths_data = "short",
                        option_paths_results_raw = "short",
                        do_new = Bool_Results_Processing_Do_New)
    
  }
  
  
  if (table_jobs_process_results$type_model[ii] == "xgb") {
    
    process_results_xgb(number_job_array = table_jobs_process_results$number_job_array[ii],
                        directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                        directory_parameters = paste0(Directory_Computing_Jobs, table_jobs_process_results$number_job_array[ii], "/", Subdirectory_Jobs_Parameters),
                        directory_results_raw = paste0(Directory_Computing_Jobs, table_jobs_process_results$number_job_array[ii], "/", Subdirectory_Jobs_Results),
                        directory_results_processed = Directory_Results,
                        option_paths_data = "short",
                        option_paths_results_raw = "short",
                        do_new = Bool_Results_Processing_Do_New)
    
  }
  
}


