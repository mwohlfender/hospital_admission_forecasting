

# define which parts of the analysis shall be done from scratch ----
Bool_Define_Parameters_Do_New <- FALSE
Bool_Data_Processing_Do_New <- FALSE
Bool_Apply_Models_Do_New <- FALSE
Bool_Results_Processing_Do_New <- FALSE
Bool_Create_Plots_Do_New <- FALSE



# set paths ----

# local directory (outside folder that is synced with GitHub) where especially model output files are stored
# the volume of model output files is too large to store all of them on GitHub
Directory_Local_Files <- "DIRECTORY/"

# local directory where files related to computing jobs done on UBELIX, the HPC cluster of the University of Bern, are stored 
Directory_Computing_Jobs <- paste0(Directory_Local_Files, "jobs/")

# subdirectory within the directory of a computing job stored in `Directory_Computing_Jobs` where data files are  stored
Subdirectory_Jobs_Data <- "data/"

# subdirectory within the directory of a computing job stored in `Directory_Computing_Jobs` where parameter files are  stored
Subdirectory_Jobs_Parameters <- "parameters/"

# subdirectory within the directory of a computing job stored in `Directory_Computing_Jobs` where results files are  stored
Subdirectory_Jobs_Results <- "results/"



# directory where data files are stored
Directory_Data <- "data/"


# subdirectory within `Directory_Data` where hospital data is stored
Subdirectory_Data_Hospital <- "01_hospital/"

# subdirectory within `Directory_Data` where wastewater data is stored
Subdirectory_Data_Wastewater <- "02_wastewater/"

# subdirectory within `Directory_Data` where feature sets are stored
Subdirectory_Data_Feature_sets <- "03_feature_sets/"


# paths of raw hospital data (NEEDS TO BE ADJUSTED)
Path_Data_Hospital_Main_Raw <- paste0(Directory_Data, Subdirectory_Data_Hospital, "raw/", "FILENAME_main.csv")
Path_Data_Hospital_Dia_Raw <- paste0(Directory_Data, Subdirectory_Data_Hospital, "raw/", "FILENAME_dia.csv")
Path_Data_Hospital_Mov_Raw <- paste0(Directory_Data, Subdirectory_Data_Hospital, "raw/", "FILENAME_mov.csv")
Path_Data_Hospital_VDat_Raw <- paste0(Directory_Data, Subdirectory_Data_Hospital, "raw/", "FILENAME_vdat.csv")
Path_Data_Hospital_Lab_Raw <- paste0(Directory_Data, Subdirectory_Data_Hospital, "raw/", "FILENAME_lab.csv")

# paths of processed hospital data
Path_Data_Hospital_Main_Processed <- paste0(Directory_Data, Subdirectory_Data_Hospital, "processed/", "data_hospital_main_processed.csv")
Path_Data_Hospital_Dia_Processed <- paste0(Directory_Data, Subdirectory_Data_Hospital, "processed/", "data_hospital_dia_processed.csv")
Path_Data_Hospital_Mov_Processed <- paste0(Directory_Data, Subdirectory_Data_Hospital, "processed/", "data_hospital_mov_processed.csv")
Path_Data_Hospital_VDat_Body_Temperature_Processed <- paste0(Directory_Data, Subdirectory_Data_Hospital, "processed/", "data_hospital_vdat_body_temperature_processed.csv")
Path_Data_Hospital_Lab_CRP_Processed <- paste0(Directory_Data, Subdirectory_Data_Hospital, "processed/", "data_hospital_lab_crp_processed.csv")

# paths of processed and summarized hospital data used for features
Path_Table_Days_ICD_U07_1_20200101_20230630 <- paste0(Directory_Data, Subdirectory_Data_Hospital, "processed/", "table_days_icd_u07_1.csv")
Path_Table_Days_Visits_Emergency_20200101_20230630 <- paste0(Directory_Data, Subdirectory_Data_Hospital, "processed/", "table_days_visits_emergency.csv")
Path_Table_Days_Fever_20200101_20230630 <- paste0(Directory_Data, Subdirectory_Data_Hospital, "processed/", "table_days_fever.csv")
Path_Table_Days_Number_Patients_High_CRP_20200101_20230630 <- paste0(Directory_Data, Subdirectory_Data_Hospital, "processed/", "table_days_number_patients_high_crp.csv")
Path_Table_Days_ICD_Freq_Codes_Cat_Chap_Covid_20200101_20230630 <- paste0(Directory_Data, Subdirectory_Data_Hospital, "processed/", "table_days_icd_freq_codes_cat_chap_covid.csv")

# paths of processed and summarized hospital data used for features that are anonymized
Path_Table_Days_ICD_U07_1_20200101_20230630_Anonymized <- paste0(Directory_Data, Subdirectory_Data_Hospital, "processed/", "table_days_icd_u07_1_anonymized.csv")
Path_Table_Days_Visits_Emergency_20200101_20230630_Anonymized <- paste0(Directory_Data, Subdirectory_Data_Hospital, "processed/", "table_days_visits_emergency_anonymized.csv")
Path_Table_Days_Fever_20200101_20230630_Anonymized <- paste0(Directory_Data, Subdirectory_Data_Hospital, "processed/", "table_days_fever_anonymized.csv")
Path_Table_Days_Number_Patients_High_CRP_20200101_20230630_Anonymized <- paste0(Directory_Data, Subdirectory_Data_Hospital, "processed/", "table_days_number_patients_high_crp_anonymized.csv")
Path_Table_Days_ICD_Freq_Codes_Cat_Chap_Covid_20200101_20230630_Anonymized <- paste0(Directory_Data, Subdirectory_Data_Hospital, "processed/", "table_days_icd_freq_codes_cat_chap_covid_anonymized.csv")

# path of raw wastewater data
Path_Data_Wastewater_Raw <- paste0(Directory_Data, Subdirectory_Data_Wastewater, "raw/", "processed_normed_data_laupen_v2.csv")

# path of processed wastewater data
Path_Data_Wastewater_Processed <- paste0(Directory_Data, Subdirectory_Data_Wastewater, "processed/", "data_wastewater_processed.csv")



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

# subdirectory within `Directory_Parameters` where files containing information about the processing of the results of the high performance computing jobs (done on UBELIX) are stored
Subdirectory_Parameters_Job_Results_Processing <- "06_job_results_processing/"


# path within `Directory_Parameters` where overview of models is stored
Path_Overview_Table_Models <- paste0(Subdirectory_Parameters_Models, "01_overview_table_models.csv")

# path within `Directory_Parameters` where overview of data sets is stored
Path_Overview_Table_Datasets <- paste0(Subdirectory_Parameters_Datasets, "01_overview_table_datasets.csv")

# path within `Directory_Parameters` where table of parameters (k, N and p) is stored
Path_Table_Parameters_kNp <- paste0(Subdirectory_Parameters_kNp_Dates_Train_Test, "01_table_parameters_kNp.csv")

# path within `Directory_Parameters` where overview table of dates (date_min_train, date_max_train, date_min_test and date_max_test) is stored
Path_Table_Dates_Train_Test_Overview <- paste0(Subdirectory_Parameters_kNp_Dates_Train_Test, "02_table_dates_train_test_overview.csv")

# path within `Directory_Parameters` where table of dates (date_min_train, date_max_train, date_min_test and date_max_test) is stored
Path_Table_Dates_Train_Test_Detailed <- paste0(Subdirectory_Parameters_kNp_Dates_Train_Test, "03_table_dates_train_test_details.csv")

# general path within `Directory_Parameters` where grids of parameter combinations
# (parameters k, N and p and dates date_min_train, date_max_train, date_min_test and date_max_test) are stored
Path_Grid_Combinations_kNp_Dates_Train_Test <- paste0(Subdirectory_Parameters_kNp_Dates_Train_Test,
                                                      Subsubdirectory_Parameters_kNp_Dates_Train_Test_Grids, "grid_combinations_kNp_dates_train_test_")

# general path within `Directory_Parameters` where grids of parameter combinations for model runs on UBELIX are stored
Path_Grid_Combinations_Data_kNp_Dates_Hyp <- paste0(Subdirectory_Parameters_Job_Setup, "grid_combinations_data_kNp_dates_hyp_")

# general path within `Directory_Parameters` where table of computing jobs whose results shall be processed are stored
Path_Table_Jobs_Results_Processing <- paste0(Subdirectory_Parameters_Job_Results_Processing, "table_jobs_process_results.csv")



# directory where results files are stored
Directory_Results <- "results/"



# directory where plots are stored
Directory_Plots <- "plots/"


# subdirectory within `Directory_Plots` where plots of data are stored
Subdirectory_Plots_Data <- "data/"

# subdirectory within `Directory_Plots` where plots of parameters are stored
Subdirectory_Plots_Parameters <- "parameters/"

# subdirectory within `Directory_Plots` where tables are stored
Subdirectory_Plots_Tables <- "tables/"

# subdirectory within `Directory_Plots` where plots for the manuscript are stored
Subdirectory_Plots_Manuscript <- "manuscript/"

# subdirectory within `Directory_Plots` where plots for the supplementary are stored
Subdirectory_Plots_Supplementary <- "supplementary/"


