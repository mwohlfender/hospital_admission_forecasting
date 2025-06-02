
table_datasets <- read_csv(file = paste0(Directory_Parameters, Path_Overview_Table_Datasets))

score_results(directory_results = paste0(Directory_Ubelix, "000_A_process_results/results/"),
              number_xy = "001",
              number_grid_combinations_kNp_dates_train_test = "002",
              filter_k = c(),
              filter_p = c(),
              group_dates_train_test = "002",
              list_models = c("lr", "nn", "lstm", "xgb"),
              list_hyp_par_grids = c("NONE", "003", "001", "006"),
              filter_datasets = c(),
              scale_limits = c(0.5, 5),
              scale_breaks = c(0.5, 1, 2.5, 5),
              include_locf = TRUE,
              plot_manuscript = "Table_2")

score_results(directory_results = paste0(Directory_Ubelix, "000_A_process_results/results/"),
              number_xy = "001",
              number_grid_combinations_kNp_dates_train_test = "003",
              filter_k = c(),
              filter_p = c(),
              group_dates_train_test = "006",
              list_models = c("lr", "nn", "lstm", "xgb"),
              list_hyp_par_grids = c("NONE", "003", "001", "006"),
              filter_datasets = c(table_datasets %>% filter(substr(label_dataset, start = 1, stop = 1) %in% LETTERS[1:12]) %>% pull(label_dataset)),
              scale_limits = c(0.3, 30),
              scale_breaks = c(0.5, 1, 5, 25),
              name_ending = "",
              include_locf = TRUE,
              plot_manuscript = "Table_3")

score_results_multiple_models(directory_results = paste0(Directory_Ubelix, "000_A_process_results/results/"),
                              number_xy = "001",
                              list_numbers_grid_combinations_kNp_dates_train_test = c("002", "003"),
                              filter_k = c(),
                              filter_p = c(),
                              list_groups_dates_train_test = c("002", "006"),
                              labels_study_period = c("002" = "Full study period", "006" = "Partial study period"),
                              list_models = c("lr", "nn", "lstm", "xgb"),
                              list_hyp_par_grids = c("NONE", "003", "001", "006"),
                              filter_datasets = c(table_datasets %>% filter(substr(label_dataset, start = 1, stop = 1) %in% LETTERS[1:12]) %>% pull(label_dataset)),
                              plot_manuscript = "Figure_4",
                              plot_supplementary = NULL,
                              plot_presentation = NULL)



# score_results(directory_results = "C:/Users/mw22f082/Documents_MW/projects/defeat_covid/ubelix/indicators/000_A_process_results/results/",
#               number_xy = "001",
#               number_grid_combinations_kNp_dates_train_test = "003",
#               filter_k = c(0, 7, 14, 21),
#               filter_p = c(),
#               group_dates_train_test = "006",
#               list_models = c("lr", "nn", "lstm", "xgb"),
#               list_hyp_par_grids = c("NONE", "003", "001", "006"),
#               name_ending = "_filter_k_range_0_21",
#               include_locf = TRUE)

# score_results(directory_results = "C:/Users/mw22f082/Documents_MW/projects/defeat_covid/ubelix/indicators/000_A_process_results/results/",
#               number_xy = "001",
#               number_grid_combinations_kNp_dates_train_test = "003",
#               filter_k = c(0, 7, 14),
#               filter_p = c(),
#               group_dates_train_test = "006",
#               list_models = c("lr", "nn", "lstm", "xgb"),
#               list_hyp_par_grids = c("NONE", "003", "001", "006"),
#               name_ending = "_filter_k_range_0_14",
#               include_locf = TRUE)

# score_results(directory_results = "C:/Users/mw22f082/Documents_MW/projects/defeat_covid/ubelix/indicators/000_A_process_results/results/",
#               number_xy = "001",
#               number_grid_combinations_kNp_dates_train_test = "003",
#               filter_k = c(0, 7),
#               filter_p = c(),
#               group_dates_train_test = "006",
#               list_models = c("lr", "nn", "lstm", "xgb"),
#               list_hyp_par_grids = c("NONE", "003", "001", "006"),
#               name_ending = "_filter_k_range_0_7",
#               include_locf = TRUE)

# score_results(directory_results = "C:/Users/mw22f082/Documents_MW/projects/defeat_covid/ubelix/indicators/000_A_process_results/results/",
#               number_xy = "001",
#               number_grid_combinations_kNp_dates_train_test = "003",
#               filter_k = c(0),
#               filter_p = c(7),
#               group_dates_train_test = "006",
#               list_models = c("lr", "nn", "lstm", "xgb"),
#               list_hyp_par_grids = c("NONE", "003", "001", "006"),
#               name_ending = "_filter_k_0_p_7",
#               include_locf = TRUE)


