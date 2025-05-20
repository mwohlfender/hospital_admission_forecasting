
# function `data_preparation`:

# input:
# `data_features`:
# `min_training_date`: first day (day d ) of training period
# `max_training_date`: last day (day d ) of training period
# `min_testing_date`: first day (day d ) of testing period
# `max_testing_date`: last day (day d ) of testing period
# `k`:
# `N`:
# `p`:
# `remove_x`: 
# `number_combination_features`: primary number of data set, defined by the combination of features
# `name_data_set`:
# `number_combination_kNp`:
# `group_dates_train_test`:
# `number_dates_train_test`:

# output:

data_preparation_001 <- function(data_features,
                                 number_xy = "001",
                                 number_combination_features,
                                 name_data_set,
                                 number_combination_kNp,
                                 group_dates_train_test,
                                 number_dates_train_test,
                                 min_training_date,
                                 max_training_date,
                                 min_testing_date,
                                 max_testing_date,
                                 k = 0,
                                 N = 7,
                                 p = 28,
                                 remove_x = FALSE,
                                 overwrite = FALSE) {
  
  # determine paths of output
  path_data_train <- get_path_data_train(directory_data = Directory_Data_Models,
                                         number_xy = number_xy,
                                         number_combination_features = number_combination_features,
                                         name_data_set = name_data_set,
                                         number_combination_kNp = number_combination_kNp,
                                         group_dates_train_test = group_dates_train_test,
                                         number_dates_train_test = number_dates_train_test)
  
  path_data_test <- get_path_data_test(directory_data = Directory_Data_Models,
                                       number_xy = number_xy,
                                       number_combination_features = number_combination_features,
                                       name_data_set = name_data_set,
                                       number_combination_kNp = number_combination_kNp,
                                       group_dates_train_test = group_dates_train_test,
                                       number_dates_train_test = number_dates_train_test)
  
  path_target_train <- get_path_target_train(directory_data = Directory_Data_Models,
                                             number_xy = number_xy,
                                             number_combination_features = number_combination_features,
                                             name_data_set = name_data_set,
                                             number_combination_kNp = number_combination_kNp,
                                             group_dates_train_test = group_dates_train_test,
                                             number_dates_train_test = number_dates_train_test)
  
  path_target_test <- get_path_target_test(directory_data = Directory_Data_Models,
                                           number_xy = number_xy,
                                           number_combination_features = number_combination_features,
                                           name_data_set = name_data_set,
                                           number_combination_kNp = number_combination_kNp,
                                           group_dates_train_test = group_dates_train_test,
                                           number_dates_train_test = number_dates_train_test)
  
  if (!(file.exists(path_data_train)) | !(file.exists(path_data_test)) | !(file.exists(path_target_train)) | !(file.exists(path_target_test)) | overwrite) {
    
    # `data_features`: add target variable y (= y_{k,N})
    # y_{k,N}(t) = x(t+k) + x(t+k+1) + ... + x(t+k+N-1)
    data_features_target <- data_features %>%
      dplyr::mutate(y=x) %>%
      timetk::tk_augment_leads(.value=y,
                               .lags=-seq(from = k, to = k+N-1, by = 1)) %>%
      dplyr::rename_with(.cols=any_of("y_lag0"),
                         .fn=function(x) {if ("y_lag0" %in% names(.)) {return("y_lead0")}
                           else {return(character())}}) %>%
      dplyr::mutate(y = rowSums(dplyr::select(., starts_with("y_lead")))) %>%
      dplyr::select(-starts_with("y_lead"))
    
    # depending on the value of `remove_x`, remove the column `x`
    if (remove_x) {
      
      data_features_target <- data_features_target %>%
        dplyr::select(-c("x"))
      
    }
    
    # determine names of features
    names_features <- names(data_features_target %>% dplyr::select(-c("date_day", "y")))
    
    # add lags of features
    data_features_target <- data_features_target %>%
      timetk::tk_augment_lags(.value=-c("date_day","y"),
                              .lags=1:p) %>%
      dplyr::select(-all_of(names_features))
    
    n_zeros <- ceiling(log(x = p, base = 10))
    
    if (n_zeros > 1) {
      
      for (ii in 1:10^(n_zeros-1)) {
        
        data_features_target <- data_features_target %>%
          dplyr::rename_with(.cols=ends_with(paste0("_lag", ii)),
                             .fn=function(x) {return(gsub(paste0("_lag", ii), paste0("_lag", str_pad(ii, n_zeros, pad = "0")), x))})
        
      }
      
    }
    
    # add column `date_day_shift_k`
    data_features_target <- data_features_target %>%
      mutate(date_day_shift_k = date_day + k)
    
    # filter to dates between `min_training_date` and `max_testing_date`
    data_features_target <- data_features_target %>%
      dplyr::filter(date_day >= min_training_date) %>%
      dplyr::filter(date_day <= max_testing_date)
    
    # store results
    if (!(file.exists(path_data_train)) | overwrite) {
      
      data_train <- data_features_target %>%
        dplyr::filter(date_day <= max_training_date) %>%
        dplyr::select(-c("date_day","date_day_shift_k","y"))
      
      write_csv(x = data_train,
                file = path_data_train)
      
    }
    
    if (!(file.exists(path_target_train)) | overwrite) {
      
      target_train <- data_features_target %>%
        dplyr::filter(date_day <= max_training_date) %>%
        dplyr::select(c("date_day","date_day_shift_k","y"))
      
      write_csv(x = target_train,
                file = path_target_train)
      
    }
    
    if (!(file.exists(path_data_test)) | overwrite) {
      
      data_test <- data_features_target %>%
        dplyr::filter(date_day >= min_testing_date) %>%
        dplyr::select(-c("date_day","date_day_shift_k","y"))
      
      write_csv(x = data_test,
                file = path_data_test)
      
    }
    
    if (!(file.exists(path_target_test)) | overwrite) {
      
      target_test <- data_features_target %>%
        dplyr::filter(date_day >= min_testing_date) %>%
        dplyr::select(c("date_day","date_day_shift_k","y"))
      
      write_csv(x = target_test,
                file = path_target_test)
      
    }
    
  }

}


