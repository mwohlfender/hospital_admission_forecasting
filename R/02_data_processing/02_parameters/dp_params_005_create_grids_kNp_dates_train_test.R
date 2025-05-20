

# create grids of combinations of parameters k, N and p and train-test splits

# 001 ----
list_numbers_combinations_kNp_001 <- unlist(lapply(X = 1:25,
                                              FUN = function(x) str_pad(string = as.character(x), width = 3, pad = "0")))

create_grid_combinations_kNp_dates_train_test(number_grid = "001",
                                              list_numbers_combinations_kNp = list_numbers_combinations_kNp_001,
                                              list_groups_dates_train_test = c("001", "002", "003"))

# 002 ----
list_numbers_combinations_kNp_002 <- unlist(lapply(X = 1:25,
                                              FUN = function(x) str_pad(string = as.character(x), width = 3, pad = "0")))

create_grid_combinations_kNp_dates_train_test(number_grid = "002",
                                              list_numbers_combinations_kNp = list_numbers_combinations_kNp_002,
                                              list_groups_dates_train_test = c("002"))

# 003 ----
list_numbers_combinations_kNp_003 <- unlist(lapply(X = 1:25,
                                              FUN = function(x) str_pad(string = as.character(x), width = 3, pad = "0")))

create_grid_combinations_kNp_dates_train_test(number_grid = "003",
                                              list_numbers_combinations_kNp = list_numbers_combinations_kNp_003,
                                              list_groups_dates_train_test = c("006"))


