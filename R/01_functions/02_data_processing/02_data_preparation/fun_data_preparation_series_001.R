


data_preparation_series_001 <- function(data_features,
                                        number_xy = "001",
                                        number_combination_features,
                                        name_data_set,
                                        table_parameters_kNp,
                                        table_dates_train_test,
                                        remove_x = FALSE,
                                        overwrite = FALSE) {
  
  table_dates_train_test_overview <- read_csv(file = paste0(Directory_Parameters, Path_Table_Dates_Train_Test_Overview))
  
  for (ii in 1:nrow(table_parameters_kNp)) {
    
    for (jj in 1:nrow(table_dates_train_test)) {
      
      min_training_date_jj <- table_dates_train_test$date_min_train[jj]
      
      if (min_training_date_jj == ymd("1900-01-01")) {
        
        min_date_jj <- table_dates_train_test_overview %>%
          filter(group == table_dates_train_test$group[jj]) %>%
          pull(date_min)
        
        min_training_date_jj <- min_date_jj + table_parameters_kNp$p[ii]
        
      }
      
      max_training_date_jj <- table_dates_train_test$date_max_train[jj]
      
      if (max_training_date_jj == ymd("1900-01-01")) {
        
        max_training_date_jj <- table_dates_train_test$date_min_test[jj] - (table_parameters_kNp$k[ii] + table_parameters_kNp$N[ii])
        
      }
      
      max_testing_date_jj <- table_dates_train_test$date_max_test[jj]
      
      if (max_testing_date_jj == ymd("1900-01-01")) {
        
        max_date_jj <- table_dates_train_test_overview %>%
          filter(group == table_dates_train_test$group[jj]) %>%
          pull(date_max)
        
        max_testing_date_jj <- max_date_jj - (table_parameters_kNp$k[ii] + table_parameters_kNp$N[ii] - 1)
        
      }
      
      data_preparation_001(data_features = data_features,
                           number_xy = number_xy,
                           number_combination_features = number_combination_features,
                           name_data_set = name_data_set,
                           number_combination_kNp = table_parameters_kNp$number[ii],
                           group_dates_train_test = table_dates_train_test$group[jj],
                           number_dates_train_test = table_dates_train_test$number[jj],
                           min_training_date = min_training_date_jj,
                           max_training_date = max_training_date_jj,
                           min_testing_date = table_dates_train_test$date_min_test[jj],
                           max_testing_date = max_testing_date_jj,
                           k = table_parameters_kNp$k[ii],
                           N = table_parameters_kNp$N[ii],
                           p = table_parameters_kNp$p[ii],
                           remove_x = remove_x,
                           overwrite = overwrite)
      
    }
    
  }
  
}


