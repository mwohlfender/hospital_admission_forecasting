
# create a grid of combinations of parameter values (k, N and p and train-test splits)

# `number_grid`: Number of output grid
# `list_numbers_combinations_kNp`: List of numbers of combinations of values of parameters k, N and p
# `list_groups_dates_train_test`: List of groups of train-test splits
# `do_new`: Indicator whether existing output file shall be overwritten

# Output: Grid of all combinations of elements of `list_numbers_combinations_kNp` and train-test splits contained in elements of `list_groups_dates_train_test`
# is stored in `Directory_Parameters`/`Path_Grid_Combinations_kNp_Dates_Train_Test``number_grid`.csv.
# The grid has three columns: number_combination_kNp, group_dates_train_test and number_dates_train_test
create_grid_combinations_kNp_dates_train_test <- function(number_grid,
                                                          list_numbers_combinations_kNp,
                                                          list_groups_dates_train_test,
                                                          do_new = Bool_Define_Parameters_Do_New) {
  
  # determine path of output
  path_output <- paste0(Directory_Parameters, Path_Grid_Combinations_kNp_Dates_Train_Test, number_grid, ".csv")
  
  if (!(file.exists(path_output)) | do_new) {
    
    # read parameter grid: train-test splits
    table_dates_train_test_detailed <- read_csv(paste0(Directory_Parameters, Path_Table_Dates_Train_Test_Detailed))
    
    # filter `table_dates_train_test_detailed` to groups of train-test splits contained in `list_groups_dates_train_test`
    table_dates_train_test_detailed_filtered <- table_dates_train_test_detailed %>%
      filter(group %in% list_groups_dates_train_test) %>%
      dplyr::select(c("group", "number")) %>%
      dplyr::rename(c("group_dates_train_test" = "group", "number_dates_train_test" = "number"))
    
    # `table_dates_train_test_detailed_filtered`:
    # (a) repeat each row as many times as there are elements in `list_numbers_combinations_kNp`
    # (b) add column `list_numbers_combinations_kNp`
    grid_combinations_kNp_dates_train_test <- table_dates_train_test_detailed_filtered %>%
      dplyr::slice(rep(1:n(), each = length(list_numbers_combinations_kNp))) %>%
      mutate(number_combination_kNp = rep(x = list_numbers_combinations_kNp, times = nrow(table_dates_train_test_detailed_filtered))) %>%
      dplyr::select(c("number_combination_kNp", "group_dates_train_test", "number_dates_train_test")) %>%
      dplyr::arrange(number_combination_kNp, group_dates_train_test, number_dates_train_test)
    
    # write `grid_combinations_kNp_dates_train_test` to a csv file
    write_csv(x = grid_combinations_kNp_dates_train_test,
              file = path_output)
    
  }
  
}


