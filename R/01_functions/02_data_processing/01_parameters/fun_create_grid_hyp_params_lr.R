
# create grid of values of hyperparameters for LR model

# `number_hyp_par_grid`: Number of output grid
# `n_iterations`: Number of times each model run shall be repeated
# `size_limit_subgrids`: Size limit of subgrids of output grid
# `do_new`: Indicator whether existing output files shall be overwritten

# Output: Grid and subgrids of at most `size_limit_subgrids` elements of all combinations of elements of range 1 to `n_iterations`
# is stored in `Directory_Parameters`/`Subdirectory_Parameters_Hyperparameters`/lr/hyp_par_lr_`number_hyp_par_grid`_param_grid.csv.
# The grid has one column: iteration
fun_create_grid_hyp_params_lr <- function(number_hyp_par_grid,
                                          n_iterations,
                                          do_new = Bool_Define_Parameters_Do_New) {
  
  # determine directory of output
  directory_output_parameter_grid <- paste0(Directory_Parameters, Subdirectory_Parameters_Hyperparameters, "lr/", str_pad(number_hyp_par_grid, 3, pad = "0"), "/")
  
  # determine path of output
  path_output_parameter_grid <- paste0(directory_output_parameter_grid, "hyp_par_lr_", str_pad(number_hyp_par_grid, 3, pad = "0"), "_param_grid.csv")
  
  if (!(file.exists(path_output_parameter_grid)) | do_new) {
    
    # create parameter grid
    parameters_grid_lr <- expand.grid(iteration = 1:n_iterations)
    
    # create directory to store output (if it does not already exist)
    if (!(dir.exists(directory_output_parameter_grid))) {
      
      dir.create(file.path(directory_output_parameter_grid), recursive = TRUE)
      
    }
    
    # remove files inside the directory where output will be stored
    if (length(list.files(path = directory_output_parameter_grid)) > 0) {
      
      file.remove(paste0(directory_output_parameter_grid,
                         list.files(path = directory_output_parameter_grid)))
      
    }
    
    # write `parameters_grid_lr` to a csv file
    write_csv(x = parameters_grid_lr,
              file = path_output_parameter_grid)
    
  }
  
}


