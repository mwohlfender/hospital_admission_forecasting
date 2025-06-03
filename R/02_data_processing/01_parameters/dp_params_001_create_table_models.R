

# create overview table of machine learning models
# We assign the following information to each model we apply in our analysis:
# `type_model`: abbreviation that appears for example in results files
# `label_type_model`: label that appears on figures and tables
# Furthermore, we assign a color to each model and determine
# whether text appearing on top of this color shall be in black or in white.

# define overview table of models
table_models <- tribble(~type_model, ~label_type_model,
                             "locf",            "LOCF",
                               "lr",              "LR",
                              "rnn",             "RNN",
                             "lstm",            "LSTM",
                              "xgb",         "XGBoost")


# assign colors
table_models <- table_models %>%
  mutate(color_model = scales::viridis_pal(option = "G", direction = -1)(nrow(table_models)),
         color_model_font = c(rep(x = "#000000", times = custom_round_down(nrow(table_models)/2, 1)),
                              rep(x = "#FFFFFF", times = custom_round_up(nrow(table_models)/2, 1))))


# save `table_models` as csv file
write_csv(x = table_models,
          file = paste0(Directory_Parameters, Path_Overview_Table_Models))


