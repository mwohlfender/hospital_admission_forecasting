
# create grid of values of hyperparameters for LSTM model

# `number_hyp_par_grid`: Number of output grid
# `n_iterations`: Number of times each model run shall be repeated
# `size_limit_subgrids`: Size limit of subgrids of output grid
# `nneurons_one_range`: List of values for number of neurons in first hidden layer
# `nneurons_two_range`: List of values for number of neurons in second hidden layer
# `act_function_one_range`: List of activation functions for neurons in first hidden layer
# `act_function_two_range`: List of activation functions for neurons in second hidden layer
# `optimizer_range`: List of optimizers
# `learning_rate_range`: List of values for learning rate
# `loss_function_range`: List of loss functions
# `nepochs_range`: List of values for number of epochs
# `do_new`: Indicator whether existing output files shall be overwritten

# Output: Grid and subgrids of at most `size_limit_subgrids` elements of all combinations of elements of range 1 to `n_iterations`,
# `nneurons_one_range`, `nneurons_two_range`, `act_function_one_range`, `act_function_two_range`,
# `optimizer_range`, `learning_rate_range`, `loss_function_range` and `nepochs_range`
# is stored in `Directory_Parameters`/`Subdirectory_Parameters_Hyperparameters`/lstm/hyp_par_lstm_`number_hyp_par_grid`_param_grid.csv.
# The grid has ten columns: iteration, nlayers, nneurons_one, nneurons_two, act_function_one, act_function_two,
# optimizer, learning_rate, loss_function and nepochs
fun_create_grid_hyp_params_lstm <- function(number_hyp_par_grid,
                                            n_iterations,
                                            size_limit_subgrids,
                                            nneurons_one_range,
                                            nneurons_two_range,
                                            act_function_one_range,
                                            act_function_two_range,
                                            optimizer_range,
                                            learning_rate_range,
                                            loss_function_range,
                                            nepochs_range,
                                            do_new = Bool_Define_Parameters_Do_New) {
  
  # determine path of output
  path_output_parameter_grid <- paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "lstm/", str_pad(number_hyp_par_grid, 3, pad = "0"),
                                       "/hyp_par_lstm_", str_pad(number_hyp_par_grid, 3, pad = "0"), "_param_grid.csv")
  
  if (!(file.exists(path_output_parameter_grid)) | do_new) {
    
    # create parameter grid
    parameters_grid_lstm_1 <- expand.grid(iteration = 1:n_iterations,
                                          nlayers = 1,
                                          nneurons_one = nneurons_one_range,
                                          nneurons_two = 0,
                                          act_function_one = act_function_one_range,
                                          act_function_two = "none",
                                          optimizer = optimizer_range,
                                          learning_rate = learning_rate_range,
                                          loss_function = loss_function_range,
                                          nepochs = nepochs_range)
    
    parameters_grid_lstm_2 <- expand.grid(iteration = 1:n_iterations,
                                          nlayers = 2,
                                          nneurons_one = nneurons_one_range,
                                          nneurons_two = nneurons_two_range,
                                          act_function_one = act_function_one_range,
                                          act_function_two = act_function_two_range,
                                          optimizer = optimizer_range,
                                          learning_rate = learning_rate_range,
                                          loss_function = loss_function_range,
                                          nepochs = nepochs_range)
    
    parameters_grid_lstm <- rbind(parameters_grid_lstm_1, parameters_grid_lstm_2)
    
    # create directory to store output (if it does not already exist)
    if (!(file.exists(paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "lstm/", number_hyp_par_grid)))) {
      
      dir.create(file.path(paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "lstm/", number_hyp_par_grid)))
      
    }
    
    # remove files inside the directory where output will be stored
    if (length(list.files(path = paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "lstm/", number_hyp_par_grid))) > 0) {
      
      file.remove(paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "lstm/", number_hyp_par_grid, "/",
                         list.files(path = paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "lstm/", number_hyp_par_grid))))
      
    }
    
    # write `parameters_grid_lstm` to a csv file
    write_csv(x = parameters_grid_lstm,
              file = path_output_parameter_grid)
    
    index <- 1
    
    # split `parameters_grid_lstm` to smaller grids if `parameters_grid_lstm` has more than `size_limit_subgrids` lines
    if (nrow(parameters_grid_lstm) > size_limit_subgrids) {
      
      for (iteration_0 in 1:n_iterations) {
        
        parameters_grid_lstm_split <- parameters_grid_lstm %>%
          filter(iteration == iteration_0)
        
        if (nrow(parameters_grid_lstm_split) <= size_limit_subgrids) {
          
          write_csv(x = parameters_grid_lstm_split,
                    file = paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "lstm/", str_pad(number_hyp_par_grid, 3, pad = "0"),
                                  "/hyp_par_lstm_", str_pad(number_hyp_par_grid, 3, pad = "0"), "_param_grid_", index, ".csv"))
          
          index <- index + 1
          
        } else {
          
          for (nneurons_one_0 in nneurons_one_range) {
            
            parameters_grid_lstm_split <- parameters_grid_lstm %>%
              filter(iteration == iteration_0,
                     nneurons_one == nneurons_one_0)
            
            if (nrow(parameters_grid_lstm_split) <= size_limit_subgrids) {
              
              write_csv(x = parameters_grid_lstm_split,
                        file = paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "lstm/", str_pad(number_hyp_par_grid, 3, pad = "0"),
                                      "/hyp_par_lstm_", str_pad(number_hyp_par_grid, 3, pad = "0"), "_param_grid_", index, ".csv"))
              
              index <- index + 1
              
            } else {
              
              for (act_function_one_0 in act_function_one_range) {
                
                parameters_grid_lstm_split <- parameters_grid_lstm %>%
                  filter(iteration == iteration_0,
                         nneurons_one == nneurons_one_0,
                         act_function_one == act_function_one_0)
                
                if (nrow(parameters_grid_lstm_split) <= size_limit_subgrids) {
                  
                  write_csv(x = parameters_grid_lstm_split,
                            file = paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "lstm/", str_pad(number_hyp_par_grid, 3, pad = "0"),
                                          "/hyp_par_lstm_", str_pad(number_hyp_par_grid, 3, pad = "0"), "_param_grid_", index, ".csv"))
                  
                  index <- index + 1
                  
                } else {
                  
                  for (optimizer_0 in optimizer_range) {
                    
                    parameters_grid_lstm_split <- parameters_grid_lstm %>%
                      filter(iteration == iteration_0,
                             nneurons_one == nneurons_one_0,
                             act_function_one == act_function_one_0,
                             optimizer == optimizer_0)
                    
                    if (nrow(parameters_grid_lstm_split) <= size_limit_subgrids) {
                      
                      write_csv(x = parameters_grid_lstm_split,
                                file = paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "lstm/", str_pad(number_hyp_par_grid, 3, pad = "0"),
                                              "/hyp_par_lstm_", str_pad(number_hyp_par_grid, 3, pad = "0"), "_param_grid_", index, ".csv"))
                      
                      index <- index + 1
                      
                    } else {
                      
                      for (learning_rate_0 in learning_rate_range) {
                        
                        parameters_grid_lstm_split <- parameters_grid_lstm %>%
                          filter(iteration == iteration_0,
                                 nneurons_one == nneurons_one_0,
                                 act_function_one == act_function_one_0,
                                 optimizer == optimizer_0,
                                 learning_rate == learning_rate_0)
                        
                        if (nrow(parameters_grid_lstm_split) <= size_limit_subgrids) {
                          
                          write_csv(x = parameters_grid_lstm_split,
                                    file = paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "lstm/", str_pad(number_hyp_par_grid, 3, pad = "0"),
                                                  "/hyp_par_lstm_", str_pad(number_hyp_par_grid, 3, pad = "0"), "_param_grid_", index, ".csv"))
                          
                          index <- index + 1
                          
                        } else {
                          
                          for (loss_function_0 in loss_function_range) {
                            
                            parameters_grid_lstm_split <- parameters_grid_lstm %>%
                              filter(iteration == iteration_0,
                                     nneurons_one == nneurons_one_0,
                                     act_function_one == act_function_one_0,
                                     optimizer == optimizer_0,
                                     learning_rate == learning_rate_0,
                                     loss_function == loss_function_0)
                            
                            if (nrow(parameters_grid_lstm_split) <= size_limit_subgrids) {
                              
                              write_csv(x = parameters_grid_lstm_split,
                                        file = paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "lstm/", str_pad(number_hyp_par_grid, 3, pad = "0"),
                                                      "/hyp_par_lstm_", str_pad(number_hyp_par_grid, 3, pad = "0"), "_param_grid_", index, ".csv"))
                              
                              index <- index + 1
                              
                            } else {
                              
                              for (nepochs_0 in nepochs_range) {
                                
                                parameters_grid_lstm_split <- parameters_grid_lstm %>%
                                  filter(iteration == iteration_0,
                                         nneurons_one == nneurons_one_0,
                                         act_function_one == act_function_one_0,
                                         optimizer == optimizer_0,
                                         learning_rate == learning_rate_0,
                                         loss_function == loss_function_0,
                                         nepochs == nepochs_0)
                                
                                write_csv(x = parameters_grid_lstm_split,
                                          file = paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "lstm/", str_pad(number_hyp_par_grid, 3, pad = "0"),
                                                        "/hyp_par_lstm_", str_pad(number_hyp_par_grid, 3, pad = "0"), "_param_grid_", index, ".csv"))
                                
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


