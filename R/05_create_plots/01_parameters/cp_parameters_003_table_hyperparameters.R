
# create table of values of hyperparameters for RNN, LSTM and XGBoost models with which the models were run


data_table_hyperparameters <- tribble(~model, ~n_parameters_total,                                                   ~parameter,                       ~values, ~dummy,
                                       "RNN",               "144",                                    "number of hidden layers",                        "1, 2",    "A",
                                       "RNN",               "144",                              "neurons in first hidden layer",                  "16, 32, 64",    "A",
                                       "RNN",               "144",                             "neurons in second hidden layer",                   "8, 16, 32",    "A",
                                       "RNN",               "144",                                        "activation function",                        "relu",    "A",
                                       "RNN",               "144",                                                  "optimizer",                        "adam",    "A",
                                       "RNN",               "144",                                              "learning rate", "0.0005, 0.001, 0.002, 0.005",    "A",
                                       "RNN",               "144",                                           "number of epochs",                  "10, 25, 50",    "A",
                                      "LSTM",               "144",                                    "number of hidden layers",                        "1, 2",    "B",
                                      "LSTM",               "144",                              "neurons in first hidden layer",                  "16, 32, 64",    "B",
                                      "LSTM",               "144",                             "neurons in second hidden layer",                   "8, 16, 32",    "B",
                                      "LSTM",               "144",                                        "activation function",                        "relu",    "B",
                                      "LSTM",               "144",                                                  "optimizer",                        "adam",    "B",
                                      "LSTM",               "144",                                              "learning rate", "0.0005, 0.001, 0.002, 0.005",    "B",
                                      "LSTM",               "144",                                           "number of epochs",                 "10, 50, 100",    "B",
                                   "XGBoost",               "864",                      "maximum number of boosting iterations",   "10, 25, 50, 100, 150, 200",    "C",
                                   "XGBoost",               "864",                                        "learning rate (eta)",          "0.1, 0.3, 0.5, 0.7",    "C",
                                   "XGBoost",               "864",                                    "maximum depth of a tree",            "3, 4, 5, 6, 7, 8",    "C",
                                   "XGBoost",               "864", "maximum change allowed between iterations (max delta step)",        "0, 5, 10, 15, 20, 25",    "C")



# create flextable: table of hyperparameters
table_hyperparameters <- flextable(data_table_hyperparameters, col_keys = c("model", "n_parameters_total", "parameter", "values"))
table_hyperparameters <- set_header_labels(table_hyperparameters,
                                                 values = list(model = "Model",
                                                               n_parameters_total = "Combinations",
                                                               parameter = "Parameter",
                                                               values = "Values"))

table_hyperparameters <- theme_vanilla(table_hyperparameters)

table_hyperparameters <- merge_v(table_hyperparameters, j = c("dummy"), target = c("model", "n_parameters_total"))

table_hyperparameters <- width(table_hyperparameters,
                               j = 1:(ncol(data_table_hyperparameters)-1),
                               width = c(1, 1, 3.1, 2.1),
                               unit = "in")

flextable::save_as_image(x = table_hyperparameters,
                         path = paste0(Directory_Plots, Subdirectory_Plots_Tables, "table_hyperparameters.png"))

flextable::save_as_image(x = table_hyperparameters,
                         path = paste0(Directory_Plots, Subdirectory_Plots_Supplementary, "supplementary_table_S1.png"))


