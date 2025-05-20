

# define which parts of the analysis shall be done from scratch ----
Bool_Define_Parameters_Do_New <- FALSE
Bool_Data_Processing_Do_New <- FALSE
Bool_Apply_Models_Do_New <- FALSE
Bool_Results_Processing_Do_New <- FALSE
Bool_Create_Plots_Do_New <- FALSE



# define additional R functions ----

# rounding functions
custom_round_down <- function(x, accuracy) {
  
  if (accuracy == 0) {
    
    return(0)
    
  } else {
    
    accuracy <- abs(accuracy)
    
  }
  
  boundary_1 <- floor(x / accuracy) * accuracy
  boundary_2 <- ceiling(x / accuracy) * accuracy
  
  if (boundary_1 + accuracy <= x | boundary_2 - accuracy >= x | boundary_1 == boundary_2) {
    
    return(x)
    
  } else {
    
    return(boundary_1)
    
  }
  
}


custom_round_up <- function(x, accuracy) {
  
  if (accuracy == 0) {
    
    return(0)
    
  } else {
    
    accuracy <- abs(accuracy)
    
  }
  
  boundary_1 <- floor(x / accuracy) * accuracy
  boundary_2 <- ceiling(x / accuracy) * accuracy
  
  if (boundary_1 + accuracy <= x | boundary_2 - accuracy >= x | boundary_1 == boundary_2) {
    
    return(x)
    
  } else {
    
    return(boundary_2)
    
  }
  
}


custom_round <- function(x, accuracy) {
  
  x_round_up <- custom_round_up(x, accuracy)
  x_round_down <- custom_round_down(x, accuracy)
  
  if (abs(x - x_round_up) <= abs(x - x_round_down)) {
    
    return(x_round_up)
    
  } else {
    
    return(x_round_down)
    
  }
  
}



# set paths ----

# directory where data files are stored
Directory_Data <- "data/"


# subdirectory within `Directory_Data` where hospital data is stored
Subdirectory_Data_Hospital <- "01_hospital/"

# subdirectory within `Directory_Data` where wastewater data is stored
Subdirectory_Data_Wastewater <- "02_wastewater/"

# subdirectory within `Directory_Data` where feature sets are stored
Subdirectory_Data_Feature_sets <- "03_feature_sets/"


# paths of processed anonymized hospital data
Path_Table_Days_ICD_U07_1_20200101_20230630_Anonymized <- paste0(Subdirectory_Data_Hospital, "processed/table_days_icd_u07_1_anonymized.csv")
Path_Table_Days_Visits_Emergency_20200101_20230630_Anonymized <- paste0(Subdirectory_Data_Hospital, "processed/table_days_visits_emergency_anonymized.csv")
Path_Table_Days_Fever_20200101_20230630_Anonymized <- paste0(Subdirectory_Data_Hospital, "processed/table_days_fever_anonymized.csv")
Path_Table_Days_Number_Patients_High_CRP_20200101_20230630_Anonymized <- paste0(Subdirectory_Data_Hospital, "processed/table_days_number_patients_high_crp_anonymized.csv")
Path_Table_Days_ICD_Freq_Codes_Cat_Chap_Covid_20200101_20230630_Anonymized <- paste0(Subdirectory_Data_Hospital, "processed/table_days_icd_freq_codes_cat_chap_covid_anonymized.csv")

# path of raw wastewater data
Path_Data_Wastewater_Raw <- paste0(Subdirectory_Data_Wastewater, "raw/processed_normed_data_laupen_v2.csv")

# path of processed wastewater data
Path_Data_Wastewater_Processed <- paste0(Subdirectory_Data_Wastewater, "processed/data_wastewater_processed.csv")



# directory where parameter files are stored
Directory_Parameters <- "parameters/"


# subdirectory within `Directory_Parameters` where files containing information about the models are stored
Subdirectory_Parameters_Models <- "01_models/"

# subdirectory within `Directory_Parameters` where  files containing information about the datasets are stored
Subdirectory_Parameters_Datasets <- "02_datasets/"

# subdirectory within `Directory_Parameters` where files containing information about the forecasting setups (parameters k, N and p and train-test splits) are stored
Subdirectory_Parameters_kNp_Dates_Train_Test <- "03_kNp_dates_train_test/"

# subsubdirectory within `Subdirectory_Parameters_kNp_Dates_Train_Test` in which grids of combinations of parameters k, N and p and train-test splits are stored
Subsubdirectory_Parameters_kNp_Dates_Train_Test_Grids <- "grid_combinations_kNp_dates_train_test/"

# subdirectory within `Directory_Parameters` where files containing information about the hyperparameters of the different models are stored
Subdirectory_Parameters_Hyperparameters <- "04_hyperparameters/"

# subdirectory within `Directory_Parameters` where files containing information about the setup of high performance computing jobs (done on UBELIX) are stored
Subdirectory_Parameters_Job_Setup <- "05_job_setup/"


# path within `Directory_Parameters` where overview of models is stored
Path_Overview_Table_Models <- paste0(Subdirectory_Parameters_Models, "01_overview_table_models.csv")

# # path within `Directory_Parameters` where overview of data sets is stored
Path_Overview_Table_Datasets <- paste0(Subdirectory_Parameters_Datasets, "01_overview_table_datasets.csv")

# path within `Directory_Parameters` where table of parameters (k, N and p) is stored
Path_Table_Parameters_kNp <- paste0(Subdirectory_Parameters_kNp_Dates_Train_Test, "01_table_parameters_kNp.csv")

# path within `Directory_Parameters` where overview table of dates (date_min_train, date_max_train, date_min_test and date_max_test) is stored
Path_Table_Dates_Train_Test_Overview <- paste0(Subdirectory_Parameters_kNp_Dates_Train_Test, "02_table_dates_train_test_overview.csv")

# path within `Directory_Parameters` where table of dates (date_min_train, date_max_train, date_min_test and date_max_test) is stored
Path_Table_Dates_Train_Test_Detailed <- paste0(Subdirectory_Parameters_kNp_Dates_Train_Test, "03_table_dates_train_test_details.csv")

# general path within `Directory_Parameters` where grids of parameter combinations
# (parameters k, N and p and dates date_min_train, date_max_train, date_min_test and date_max_test) are stored
Path_Grid_Combinations_kNp_Dates_Train_Test <- paste0(Subdirectory_Parameters_kNp_Dates_Train_Test, Subsubdirectory_Parameters_kNp_Dates_Train_Test_Grids,
                                                      "grid_combinations_kNp_dates_train_test_")

# general path within `Directory_Parameters` where grids of parameter combinations for model runs on UBELIX are stored
Path_Grid_Combinations_Data_kNp_Dates_Hyp <- paste0(Subdirectory_Parameters_Job_Setup, "grid_combinations_data_kNp_dates_hyp_")


