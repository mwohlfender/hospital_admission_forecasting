
# create grid of values of hyperparameters for XGBoost model

# `number_hyp_par_grid`: Number of output grid
# `n_iterations`: Number of times each model run shall be repeated
# `size_limit_subgrids`: Size limit of subgrids of output grid
# `nrounds_range`: List of values for nrounds parameter of XGBoost model
# `eta_range`: List of values for eta parameter of XGBoost model
# `gamma_range`: List of values for gamma parameter of XGBoost model
# `max_depth_range`: List of values for max_depth parameter of XGBoost model
# `min_child_weight_range`: List of values for min_child_weight parameter of XGBoost model
# `max_delta_step_range`: List of values for max_delta_step parameter of XGBoost model
# `subsample_range`: List of values for subsample parameter of XGBoost model
# `colsample_bytree_range`: List of values for colsample_bytree parameter of XGBoost model
# `colsample_bylevel_range`: List of values for colsample_bylevel parameter of XGBoost model
# `colsample_bynode_range`: List of values for colsample_bynode parameter of XGBoost model
# `lambda_range`: List of values for lambda parameter of XGBoost model
# `alpha_range`: List of values for alpha parameter of XGBoost model
# `do_new`: Indicator whether existing output files shall be overwritten

# Output: Grid and subgrids of at most `size_limit_subgrids` elements of all combinations of elements of range 1 to `n_iterations`,
# `nrounds_range`, `eta_range`, `gamma_range`, `max_depth_range`, `min_child_weight_range`, `max_delta_step_range`,
# `subsample_range`, `colsample_bytree_range`, `colsample_bylevel_range`, `colsample_bynode_range`, `lambda_range` and `alpha_range`
# is stored in `Directory_Parameters`/`Subdirectory_Parameters_Hyperparameters`/xgb/hyp_par_xgb_`number_hyp_par_grid`_param_grid.csv.
# The grid has thirteen columns: iteration, nrounds, eta, gamma, max_depth, min_child_weight, max_delta_step,
# subsample, colsample_bytree, colsample_bylevel, colsample_bynode, lambda and alpha
fun_create_grid_hyp_params_xgb <- function(number_hyp_par_grid,
                                           n_iterations,
                                           size_limit_subgrids,
                                           nrounds_range,
                                           eta_range,
                                           gamma_range,
                                           max_depth_range,
                                           min_child_weight_range,
                                           max_delta_step_range,
                                           subsample_range,
                                           colsample_bytree_range,
                                           colsample_bylevel_range,
                                           colsample_bynode_range,
                                           lambda_range,
                                           alpha_range,
                                           do_new = Bool_Define_Parameters_Do_New) {
  
  # determine path of output
  path_output_parameter_grid <- paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "xgb/", str_pad(number_hyp_par_grid, 3, pad = "0"),
                                       "/hyp_par_xgb_", str_pad(number_hyp_par_grid, 3, pad = "0"), "_param_grid.csv")
  
  if (!(file.exists(path_output_parameter_grid)) | do_new) {
    
    # create parameter grid
    parameters_grid_xgb <- expand.grid(iteration = 1:n_iterations,
                                       nrounds = nrounds_range,
                                       eta = eta_range,
                                       gamma = gamma_range,
                                       max_depth = max_depth_range,
                                       min_child_weight = min_child_weight_range,
                                       max_delta_step = max_delta_step_range,
                                       subsample = subsample_range,
                                       colsample_bytree = colsample_bytree_range,
                                       colsample_bylevel = colsample_bylevel_range,
                                       colsample_bynode = colsample_bynode_range,
                                       lambda = lambda_range,
                                       alpha = alpha_range)
    
    # create directory to store output (if it does not already exist)
    if (!(file.exists(paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "xgb/", number_hyp_par_grid)))) {
      
      dir.create(file.path(paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "xgb/", number_hyp_par_grid)))
      
    }
    
    # remove files inside the directory where output will be stored
    if (length(list.files(path = paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "xgb/", number_hyp_par_grid))) > 0) {
      
      file.remove(paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "xgb/", number_hyp_par_grid, "/",
                         list.files(path = paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "xgb/", number_hyp_par_grid))))
      
    }
    
    # write `parameters_grid_xgb` to a csv file
    write_csv(x = parameters_grid_xgb,
              file = path_output_parameter_grid)
    
    index <- 1
    
    # split `parameters_grid_xgb` to smaller grids if `parameters_grid_xgb` has more than `size_limit_subgrids` lines
    
    if (nrow(parameters_grid_xgb) > size_limit_subgrids) { 
      
      for (nrounds_0 in nrounds_range) {
        
        parameters_grid_xgb_split <- parameters_grid_xgb %>%
          filter(nrounds == nrounds_0)
        
        if (nrow(parameters_grid_xgb_split) <= size_limit_subgrids) {
          
          write_csv(x = parameters_grid_xgb_split,
                    file = paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "xgb/", str_pad(number_hyp_par_grid, 3, pad = "0"),
                                  "/hyp_par_xgb_", str_pad(number_hyp_par_grid, 3, pad = "0"), "_param_grid_", index, ".csv"))
          
          index <- index + 1
          
        } else {
          
          for (eta_0 in eta_range) {
            
            parameters_grid_xgb_split <- parameters_grid_xgb %>%
              filter(nrounds == nrounds_0,
                     eta == eta_0)
            
            if (nrow(parameters_grid_xgb_split) <= size_limit_subgrids) {
              
              write_csv(x = parameters_grid_xgb_split,
                        file = paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "xgb/", str_pad(number_hyp_par_grid, 3, pad = "0"),
                                      "/hyp_par_xgb_", str_pad(number_hyp_par_grid, 3, pad = "0"), "_param_grid_", index, ".csv"))
              
              index <- index + 1
              
            } else {
              
              for (gamma_0 in gamma_range) {
                
                parameters_grid_xgb_split <- parameters_grid_xgb %>%
                  filter(nrounds == nrounds_0,
                         eta == eta_0,
                         gamma == gamma_0)
                
                if (nrow(parameters_grid_xgb_split) <= size_limit_subgrids) {
                  
                  write_csv(x = parameters_grid_xgb_split,
                            file = paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "xgb/", str_pad(number_hyp_par_grid, 3, pad = "0"),
                                          "/hyp_par_xgb_", str_pad(number_hyp_par_grid, 3, pad = "0"), "_param_grid_", index, ".csv"))
                  
                  index <- index + 1
                  
                } else {
                  
                  for (max_depth_0 in max_depth_range) {
                    
                    parameters_grid_xgb_split <- parameters_grid_xgb %>%
                      filter(nrounds == nrounds_0,
                             eta == eta_0,
                             gamma == gamma_0,
                             max_depth == max_depth_0)
                    
                    if (nrow(parameters_grid_xgb_split) <= size_limit_subgrids) {
                      
                      write_csv(x = parameters_grid_xgb_split,
                                file = paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "xgb/", str_pad(number_hyp_par_grid, 3, pad = "0"),
                                              "/hyp_par_xgb_", str_pad(number_hyp_par_grid, 3, pad = "0"), "_param_grid_", index, ".csv"))
                      
                      index <- index + 1
                      
                    } else {
                      
                      for (min_child_weight_0 in min_child_weight_range) {
                        
                        parameters_grid_xgb_split <- parameters_grid_xgb %>%
                          filter(nrounds == nrounds_0,
                                 eta == eta_0,
                                 gamma == gamma_0,
                                 max_depth == max_depth_0,
                                 min_child_weight == min_child_weight_0)
                        
                        if (nrow(parameters_grid_xgb_split) <= size_limit_subgrids) {
                          
                          write_csv(x = parameters_grid_xgb_split,
                                    file = paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "xgb/", str_pad(number_hyp_par_grid, 3, pad = "0"),
                                                  "/hyp_par_xgb_", str_pad(number_hyp_par_grid, 3, pad = "0"), "_param_grid_", index, ".csv"))
                          
                          index <- index + 1
                          
                        } else {
                          
                          for (max_delta_step_0 in max_delta_step_range) {
                            
                            parameters_grid_xgb_split <- parameters_grid_xgb %>%
                              filter(nrounds == nrounds_0,
                                     eta == eta_0,
                                     gamma == gamma_0,
                                     max_depth == max_depth_0,
                                     min_child_weight == min_child_weight_0,
                                     max_delta_step == max_delta_step_0)
                            
                            if (nrow(parameters_grid_xgb_split) <= size_limit_subgrids) {
                              
                              write_csv(x = parameters_grid_xgb_split,
                                        file = paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "xgb/", str_pad(number_hyp_par_grid, 3, pad = "0"),
                                                      "/hyp_par_xgb_", str_pad(number_hyp_par_grid, 3, pad = "0"), "_param_grid_", index, ".csv"))
                              
                              index <- index + 1
                              
                            } else {
                              
                              for (lambda_0 in lambda_range) {
                                
                                parameters_grid_xgb_split <- parameters_grid_xgb %>%
                                  filter(nrounds == nrounds_0,
                                         eta == eta_0,
                                         gamma == gamma_0,
                                         max_depth == max_depth_0,
                                         min_child_weight == min_child_weight_0,
                                         max_delta_step == max_delta_step_0,
                                         lambda == lambda_0)
                                
                                if (nrow(parameters_grid_xgb_split) <= size_limit_subgrids) {
                                  
                                  write_csv(x = parameters_grid_xgb_split,
                                            file = paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "xgb/", str_pad(number_hyp_par_grid, 3, pad = "0"),
                                                          "/hyp_par_xgb_", str_pad(number_hyp_par_grid, 3, pad = "0"), "_param_grid_", index, ".csv"))
                                  
                                  index <- index + 1
                                  
                                } else {
                                  
                                  for (alpha_0 in alpha_range) {
                                    
                                    parameters_grid_xgb_split <- parameters_grid_xgb %>%
                                      filter(nrounds == nrounds_0,
                                             eta == eta_0,
                                             gamma == gamma_0,
                                             max_depth == max_depth_0,
                                             min_child_weight == min_child_weight_0,
                                             max_delta_step == max_delta_step_0,
                                             lambda == lambda_0,
                                             alpha == alpha_0)
                                    
                                    write_csv(x = parameters_grid_xgb_split,
                                              file = paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "xgb/", str_pad(number_hyp_par_grid, 3, pad = "0"),
                                                            "/hyp_par_xgb_", str_pad(number_hyp_par_grid, 3, pad = "0"), "_param_grid_", index, ".csv"))
                                    
                                    index <- index + 1
                                    
                                  }
                                  
                                }
                                
                              }
                              
                            }
                            
                          }
                          
                        }
                        
                      }
                      
                    }
                    
                  }
                  
                }
                
              }
              
            }
            
          }
          
        }
        
      }
      
    }
    
  }
  
}


