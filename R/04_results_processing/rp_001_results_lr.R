

# process results of linear regression model

list_jobs_process_results_lr <- c("005", "006", "007", "008", "009",
                                  "010", "011", "012", "013", "014",
                                  "015", "016", "017", "018", "019",
                                  "020", "021", "022", "023", "024",
                                  "025")


for (ii in 1:nrow(list_jobs_process_results_lr)) {
  
  process_results_lr(number_job_array = list_jobs_process_results_lr[ii],
                     directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                     directory_parameters = Directory_Parameters,
                     directory_results_raw = Directory_Results,
                     directory_results_processed = Directory_Results,
                     option_paths_data = "NONE",
                     option_paths_results_raw = "long_directory",
                     do_new = Bool_Results_Processing_Do_New)
  
}


