
# define grids of combinations of values for hyperparameters

# long short-term memory
fun_create_grid_hyp_params_lstm(number_hyp_par_grid = "001",
                                n_iterations = 50,
                                size_limit_subgrids = 144,
                                nneurons_one_range = c(16, 32, 64),
                                nneurons_two_range = c(8, 16, 32),
                                act_function_one_range = c("relu"),
                                act_function_two_range = c("relu"),
                                optimizer_range = c("adam"),
                                learning_rate_range = c(0.0005, 0.001, 0.002, 0.005),
                                loss_function_range = c("mse"),
                                nepochs_range = c(10, 50, 100))


# recurrent neural network
fun_create_grid_hyp_params_nn(number_hyp_par_grid = "003",
                              n_iterations = 50,
                              size_limit_subgrids = 144,
                              nneurons_one_range = c(16, 32, 64),
                              nneurons_two_range = c(8, 16, 32),
                              act_function_one_range = c("relu"),
                              act_function_two_range = c("relu"),
                              optimizer_range = c("adam"),
                              learning_rate_range = c(0.0005, 0.001, 0.002, 0.005),
                              loss_function_range = c("mse"),
                              nepochs_range = c(10, 25, 50))


# xgboost
fun_create_grid_hyp_params_xgb(number_hyp_par_grid = "006",
                               n_iterations = 1,
                               size_limit_subgrids = 36,
                               nrounds_range = c(10, 25, 50, 100, 150, 200),
                               eta_range = c(0.1, 0.3, 0.5, 0.7),
                               gamma_range = c(0),
                               max_depth_range = c(3, 4, 5, 6, 7, 8),
                               min_child_weight_range = c(1),
                               max_delta_step_range = c(0, 5, 10, 15, 20, 25),
                               subsample_range = c(1),
                               colsample_bytree_range = c(1),
                               colsample_bylevel_range = c(1),
                               colsample_bynode_range = c(1),
                               lambda_range = c(1),
                               alpha_range = c(0))


