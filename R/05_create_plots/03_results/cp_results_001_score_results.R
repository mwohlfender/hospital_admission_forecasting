

score_results(list_models = c("lr", "rnn", "lstm", "xgb"),
              filter_datasets = c(),
              number_xy = "001",
              number_grid_combinations_kNp_dates_train_test = "002",
              filter_k = c(),
              filter_p = c(),
              group_dates_train_test = "002",
              list_hyp_par_grids = c("001", "003", "001", "006"),
              directory_results = Directory_Results,
              include_locf = TRUE,
              scale_limits = c(0.5, 5),
              scale_breaks = c(0.5, 1, 2.5, 5),
              plot_manuscript = "Table_2",
              plot_supplementary = c("Supplementary_Figure_S2", "Supplementary_Figure_S3"))


score_results(list_models = c("lr", "rnn", "lstm", "xgb"),
              filter_datasets = c(),
              number_xy = "001",
              number_grid_combinations_kNp_dates_train_test = "003",
              filter_k = c(),
              filter_p = c(),
              group_dates_train_test = "006",
              list_hyp_par_grids = c("001", "003", "001", "006"),
              directory_results = Directory_Results,
              include_locf = TRUE,
              scale_limits = c(0.3, 30),
              scale_breaks = c(0.5, 1, 5, 25),
              plot_manuscript = "Table_3",
              plot_supplementary = c("Supplementary_Figure_S4", "Supplementary_Figure_S5"))


score_results_multiple_models(list_models = c("lr", "rnn", "lstm", "xgb"),
                              filter_datasets = c(),
                              number_xy = "001",
                              list_numbers_grid_combinations_kNp_dates_train_test = c("002", "003"),
                              filter_k = c(),
                              filter_p = c(),
                              list_groups_dates_train_test = c("002", "006"),
                              list_hyp_par_grids = c("001", "003", "001", "006"),
                              directory_results = Directory_Results,
                              labels_study_period = c("002" = "Full study period", "006" = "Partial study period"),
                              plot_manuscript = "Figure_4",
                              plot_supplementary = c("Supplementary_Figure_S6"))


