

# directory_results <- paste0(Directory_Ubelix, "000_A_process_results/results/")
# number_xy <- "001"
# number_grid_combinations_kNp_dates_train_test <- "003"
# filter_k <- c()
# filter_p <- c()
# group_dates_train_test <- "006"
# list_models <- c("lr", "nn", "lstm", "xgb")
# list_hyp_par_grids <- c("NONE", "003", "001", "006")
# filter_datasets <- c(table_datasets %>% filter(substr(label_dataset, start = 1, stop = 1) %in% LETTERS[1:12]) %>% pull(label_dataset))
# scale_limits = c(0.3, 30)
# scale_breaks = c(0.5, 1, 5, 25)
# name_ending <- ""
# include_locf <- TRUE
# plot_manuscript <- "Table_3"


score_results <- function(directory_results = Directory_Results,
                          number_xy = "001",
                          number_grid_combinations_kNp_dates_train_test,
                          filter_k = c(),
                          filter_p = c(),
                          group_dates_train_test,
                          list_models = c("lr", "nn", "lstm", "xgb"),
                          list_hyp_par_grids = c("NONE", "003", "001", "006"),
                          filter_datasets = c(),
                          scale_limits = c(0.3, 30),
                          scale_breaks = c(0.5, 1, 5, 25),
                          name_ending = "",
                          include_locf = TRUE,
                          plot_manuscript = NULL) {
  
  # define color transform 
  fun_transform_color <- function(x) {
    return(log(x, base = 10))
  }
  
  fun_inv_transform_color <- function(x) {
    return(10^x)
  }
  
  # determine parameters
  table_models <- read_csv(file = paste0(Directory_Parameters, Path_Overview_Table_Models))
  
  table_datasets <- read_csv(file = paste0(Directory_Parameters, Path_Overview_Table_Datasets))
  
  group_dates_train_test_0 <- group_dates_train_test
  
  # read results of locf model
  results_locf <- read_csv(file = get_path_results_processed_summary(directory_results = directory_results,
                                                                     type_model = "locf",
                                                                     number_xy = number_xy,
                                                                     number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test))
  
  results_locf <- results_locf %>%
    filter(group_dates_train_test == group_dates_train_test_0) %>%
    mutate(type_model = "locf") %>%
    dplyr::select(c("type_model", "number_combination_kNp", "k", "N", "p", "group_dates_train_test", "number_dates_train_test", "rmse_locf_train", "rmse_locf_test"))
  
  
  results_models_0 <- NULL
  
  for (ii in 1:length(list_models)) {
    
    if (list_models[ii] == "lr") {
      
      pattern_model_ii <- paste0("results_summary_", list_models[ii], "_XY_", number_xy, "_*", "_PD_", number_grid_combinations_kNp_dates_train_test, ".csv")
      
    } else {
      
      if (list_hyp_par_grids[ii] == "NONE") {
        
        pattern_model_ii <- paste0("results_detailed_", list_models[ii], "_XY_", number_xy, "_*", "_PD_", number_grid_combinations_kNp_dates_train_test, ".csv")
        
      } else {
        
        pattern_model_ii <- paste0("results_detailed_", list_models[ii], "_XY_", number_xy, "_*", "_PD_", number_grid_combinations_kNp_dates_train_test,
                                   "_H_", list_hyp_par_grids[ii], ".csv")
        
      }
      
    }
    
    general_path_results_model_ii <- paste0(directory_results, list_models[ii], "/", number_xy, "/")
    
    list_files_results_model_ii <- list.files(path = general_path_results_model_ii,
                                              pattern = glob2rx(pattern_model_ii), recursive = TRUE)
    
    results_model_ii <- NULL
    
    for (jj in 1:length(list_files_results_model_ii)) {
      
      results_model_ii_jj_0 <- read_csv(paste0(general_path_results_model_ii, list_files_results_model_ii[jj]))
      
      results_model_ii_jj <- results_model_ii_jj_0 %>%
        group_by(number_combination_kNp, group_dates_train_test, number_dates_train_test) %>%
        summarize(number_xy = unique(number_xy),
                  number_combination_features = unique(number_combination_features),
                  name_data_set = unique(name_data_set),
                  k = unique(k),
                  N = unique(N),
                  p = unique(p),
                  rmse_test = min(rmse_test),
                  rmse_train = min(rmse_train[rmse_test == min(rmse_test)])) %>%
        ungroup()
      
      results_model_ii <- bind_rows(results_model_ii, results_model_ii_jj)
      
    }
    
    results_model_ii <- results_model_ii %>%
      filter(group_dates_train_test == group_dates_train_test_0) %>%
      mutate(type_model = list_models[ii]) %>%
      dplyr::select(c("type_model", "number_xy", "number_combination_features", "name_data_set",
                      "number_combination_kNp", "k", "N", "p", "group_dates_train_test", "number_dates_train_test",
                      "rmse_train", "rmse_test"))
    
    results_models_0 <- bind_rows(results_models_0, results_model_ii)
    
  }
  
  
  results_models <- results_models_0 %>%
    left_join(results_locf %>% dplyr::select(number_combination_kNp,
                                             group_dates_train_test,
                                             number_dates_train_test,
                                             rmse_locf_train,
                                             rmse_locf_test),
              by = c("number_combination_kNp" = "number_combination_kNp",
                     "group_dates_train_test" = "group_dates_train_test",
                     "number_dates_train_test" = "number_dates_train_test"))
  
  results_models <- results_models %>%
    mutate(frac_rmse_model_locf_train = rmse_train / rmse_locf_train,
           frac_rmse_model_locf_test = rmse_test / rmse_locf_test) %>%
    left_join(table_models, by = "type_model") %>%
    left_join(table_datasets, by = c("number_xy" = "number_xy",
                                     "number_combination_features" = "number_combination_features",
                                     "name_data_set" = "name_data_set"))
  
  if (length(filter_k) > 0) {
    
    results_models <- results_models %>%
      filter(k %in% filter_k)
    
  }
  
  if (length(filter_p) > 0) {
    
    results_models <- results_models %>%
      filter(p %in% filter_p)
    
  }
  
  if (length(filter_datasets > 0)) {
    
    results_models <- results_models %>%
      filter(label_dataset %in% filter_datasets)
    
  }
  
  
  # (A) Overview table: Average score per model and dataset ----
  
  # `results_models`:
  # group by model and dataset to determine average score
  results_models_summarized_model_dataset <- results_models %>%
    group_by(label_type_model, label_dataset) %>%
    summarize(score_train = prod(frac_rmse_model_locf_train)^(1/n()),
              score_test = prod(frac_rmse_model_locf_test)^(1/n()),
              perc_frac_rmse_model_locf_train_smaller_one = sum(frac_rmse_model_locf_train < 1) / n(),
              perc_frac_rmse_model_locf_test_smaller_one = sum(frac_rmse_model_locf_test < 1) / n()) %>%
    arrange(label_dataset)
  
  # create overview of average score per model and dataset for training period
  data_table_results_models_summarized_model_dataset_rmse_score_train <- results_models_summarized_model_dataset %>%
    mutate(score = paste0(format(round(score_train, 2), nsmall = 2), " (", format(round(100 * perc_frac_rmse_model_locf_train_smaller_one, 0), nsmall = 0), " %)")) %>%
    dplyr::select(c(label_type_model, label_dataset, score)) %>%
    pivot_wider(names_from = label_type_model, values_from = score) %>%
    replace(is.na(.), "-1.00 (0 %)") %>%
    dplyr::select(all_of(c("label_dataset", table_models$label_type_model[table_models$type_model != "locf" & table_models$type_model %in% list_models]))) %>%
    dplyr::rename(c("Feature set" = "label_dataset"))
  
  # create overview of average score per model and dataset for testing period
  data_table_results_models_summarized_model_dataset_rmse_score_test <- results_models_summarized_model_dataset %>%
    mutate(score = paste0(format(round(score_test, 2), nsmall = 2), " (", format(round(100 * perc_frac_rmse_model_locf_test_smaller_one, 0), nsmall = 0), " %)")) %>%
    dplyr::select(c(label_type_model, label_dataset, score)) %>%
    pivot_wider(names_from = label_type_model, values_from = score) %>%
    replace(is.na(.), "-1.00 (0 %)") %>%
    dplyr::select(all_of(c("label_dataset", table_models$label_type_model[table_models$type_model != "locf" & table_models$type_model %in% list_models]))) %>%
    dplyr::rename(c("Feature set" = "label_dataset"))
  
  if (include_locf) {
    
    data_table_results_models_summarized_model_dataset_rmse_score_train <- data_table_results_models_summarized_model_dataset_rmse_score_train %>%
      mutate(LOCF = "1") %>%
      dplyr::select(all_of(c("Feature set", "LOCF", table_models$label_type_model[table_models$type_model != "locf" & table_models$type_model %in% list_models])))
    
    data_table_results_models_summarized_model_dataset_rmse_score_test <- data_table_results_models_summarized_model_dataset_rmse_score_test %>%
      mutate(LOCF = "1") %>%
      dplyr::select(all_of(c("Feature set", "LOCF", table_models$label_type_model[table_models$type_model != "locf" & table_models$type_model %in% list_models])))
    
  }
  
  # create flextable: overview of average score per model and dataset for training period
  table_results_models_summarized_model_dataset_rmse_score_train <- flextable(data_table_results_models_summarized_model_dataset_rmse_score_train)
  table_results_models_summarized_model_dataset_rmse_score_train <- theme_vanilla(table_results_models_summarized_model_dataset_rmse_score_train)
  table_results_models_summarized_model_dataset_rmse_score_train <- width(table_results_models_summarized_model_dataset_rmse_score_train,
                                                                          j = 1:ncol(data_table_results_models_summarized_model_dataset_rmse_score_train),
                                                                          width = c(1.85, 1.09, 1.09, 1.09, 1.09, 1.09),
                                                                          unit = "in")
  
  flextable::save_as_image(x = table_results_models_summarized_model_dataset_rmse_score_train,
                           path = paste0(Directory_Plots, Subdirectory_Plots_Tables, "table_results_rmse_score_models_XY_", number_xy, "_PD_",
                                         number_grid_combinations_kNp_dates_train_test, "_D_", group_dates_train_test, "_train", name_ending, ".png"))
  
  # create flextable: overview of average score per model and dataset for testing period
  table_results_models_summarized_model_dataset_rmse_score_test <- flextable(data_table_results_models_summarized_model_dataset_rmse_score_test)
  table_results_models_summarized_model_dataset_rmse_score_test <- theme_vanilla(table_results_models_summarized_model_dataset_rmse_score_test)
  table_results_models_summarized_model_dataset_rmse_score_test <- width(table_results_models_summarized_model_dataset_rmse_score_test,
                                                                         j = 1:ncol(data_table_results_models_summarized_model_dataset_rmse_score_test),
                                                                         width = c(1.85, 1.09, 1.09, 1.09, 1.09, 1.09),
                                                                         unit = "in")
  
  flextable::save_as_image(x = table_results_models_summarized_model_dataset_rmse_score_test,
                           path = paste0(Directory_Plots, Subdirectory_Plots_Tables, "table_results_rmse_score_models_XY_", number_xy, "_PD_",
                                         number_grid_combinations_kNp_dates_train_test, "_D_", group_dates_train_test, "_test", name_ending, ".png"))
  
  if (!(is.null(plot_manuscript))) {
    
    flextable::save_as_image(x = table_results_models_summarized_model_dataset_rmse_score_test,
                             path = paste0(Directory_Plots, Subdirectory_Plots_Manuscript, plot_manuscript, ".png"))
    
  }
  
  # table_results_models_summarized_model_dataset_rmse_score_test <- color(table_results_models_summarized_model_dataset_rmse_score_test,
  #                                                                        i = 1, j = 6, color = unibeRed(), part = "body")
  # 
  # table_results_models_summarized_model_dataset_rmse_score_test <- color(table_results_models_summarized_model_dataset_rmse_score_test,
  #                                                                        i = 3, j = 4, color = unibeRed(), part = "body")
  # 
  # table_results_models_summarized_model_dataset_rmse_score_test <- color(table_results_models_summarized_model_dataset_rmse_score_test,
  #                                                                        i = 4, j = 6, color = unibeRed(), part = "body")
  # 
  # table_results_models_summarized_model_dataset_rmse_score_test <- bold(table_results_models_summarized_model_dataset_rmse_score_test,
  #                                                                        i = 1, j = 6, part = "body")
  # 
  # table_results_models_summarized_model_dataset_rmse_score_test <- bold(table_results_models_summarized_model_dataset_rmse_score_test,
  #                                                                        i = 3, j = 4, part = "body")
  # 
  # table_results_models_summarized_model_dataset_rmse_score_test <- bold(table_results_models_summarized_model_dataset_rmse_score_test,
  #                                                                        i = 4, j = 6, part = "body")
  # 
  # table_results_models_summarized_model_dataset_rmse_score_test <- color(table_results_models_summarized_model_dataset_rmse_score_test,
  #                                                                        i = 1, j = 6, color = unibeRed(), part = "body")
  # 
  # table_results_models_summarized_model_dataset_rmse_score_test <- color(table_results_models_summarized_model_dataset_rmse_score_test,
  #                                                                        i = 3, j = 4, color = unibeRed(), part = "body")
  # 
  # table_results_models_summarized_model_dataset_rmse_score_test <- color(table_results_models_summarized_model_dataset_rmse_score_test,
  #                                                                        i = 11, j = 6, color = unibeRed(), part = "body")
  # 
  # table_results_models_summarized_model_dataset_rmse_score_test <- bold(table_results_models_summarized_model_dataset_rmse_score_test,
  #                                                                        i = 1, j = 6, part = "body")
  # 
  # table_results_models_summarized_model_dataset_rmse_score_test <- bold(table_results_models_summarized_model_dataset_rmse_score_test,
  #                                                                        i = 3, j = 4, part = "body")
  # 
  # table_results_models_summarized_model_dataset_rmse_score_test <- bold(table_results_models_summarized_model_dataset_rmse_score_test,
  #                                                                        i = 11, j = 6, part = "body")
  # 
  # flextable::save_as_image(x = table_results_models_summarized_model_dataset_rmse_score_test,
  #                          path = paste0(Directory_Plots, Subdirectory_Plots_Tables, "table_results_rmse_score_models_XY_", number_xy, "_PD_",
  #                                        number_grid_combinations_kNp_dates_train_test, "_D_", group_dates_train_test, "_test", "_presentation", ".png"))
  
  
  # (B) Plot of average score per model, dataset and combination of k and p ----
  
  # `results_models`:
  # group by model, dataset and combination of k and p
  # determine average score per model, dataset and combination of k and p
  results_models_summarized_model_dataset_kNp <- results_models %>%
    group_by(label_type_model, label_dataset, number_combination_kNp) %>%
    summarize(label_dataset_linebreak = unique(label_dataset_linebreak),
              k = unique(k),
              N = unique(N),
              p = unique(p),
              score_train = prod(frac_rmse_model_locf_train)^(1/n()),
              score_test = prod(frac_rmse_model_locf_test)^(1/n()),
              perc_frac_rmse_model_locf_train_smaller_one = sum(frac_rmse_model_locf_train < 1) / n(),
              perc_frac_rmse_model_locf_test_smaller_one = sum(frac_rmse_model_locf_test < 1) / n())
  
  
  plot_results_models_overview_score_train <- ggplot(data = results_models_summarized_model_dataset_kNp) +
    geom_tile(aes(x = factor(p), y = factor(k), fill = score_train)) +
    geom_text(aes(x = factor(p), y = factor(k), label = format(round(score_train, 2), nsmall = 2)),
              color = "white", size = 3, fontface = "bold", angle = 0) +
    coord_equal() +
    facet_grid(rows = vars(label_dataset_linebreak), cols = vars(label_type_model),
               labeller = labeller(type = label_parsed)) +
    xlab("p") +
    ylab("k") +
    scale_fill_viridis_c(name = "Score",
                         breaks = c(0.25, 0.75, 1.25),
                         limits = c(0, 1.5),
                         direction = -1) +
    theme_bw() +
    theme(legend.position = "bottom",
          strip.text.x = element_text(size = 6),
          strip.text.y = element_text(size = 6))
  
  ggsave(plot = plot_results_models_overview_score_train,
         filename = paste0(Directory_Plots, "results_models/summary/plot_results_models_overview_XY_", number_xy, "_PD_",
                           number_grid_combinations_kNp_dates_train_test, "_D_", group_dates_train_test, "_score_train", name_ending, ".pdf"),
         width = 7.3,
         height = length(unique(results_models_summarized_model_dataset_kNp$label_dataset)) * 7.3 / length(unique(results_models_summarized_model_dataset_kNp$label_type_model)),
         units = c("in"))
  
  plot_results_models_overview_score_test <- ggplot(data = results_models_summarized_model_dataset_kNp) +
    geom_tile(aes(x = factor(p), y = factor(k), fill = score_test)) +
    geom_text(aes(x = factor(p), y = factor(k), label = format(round(score_test, 2), nsmall = 2)),
              color = "white", size = 3, fontface = "bold", angle = 0) +
    coord_equal() +
    facet_grid(rows = vars(label_dataset_linebreak), cols = vars(label_type_model),
               labeller = labeller(type = label_parsed)) +
    xlab("p") +
    ylab("k") +
    scale_fill_viridis_c(name = "Score",
                         breaks = scale_breaks,
                         limits = scale_limits,
                         transform = scales::trans_new("transform_color", fun_transform_color, fun_inv_transform_color),
                         direction = -1) +
    theme_bw() +
    theme(legend.position = "bottom",
          strip.text.x = element_text(size = 6),
          strip.text.y = element_text(size = 6))
  
  ggsave(plot = plot_results_models_overview_score_test,
         filename = paste0(Directory_Plots, "results_models/summary/plot_results_models_overview_XY_", number_xy, "_PD_",
                           number_grid_combinations_kNp_dates_train_test, "_D_", group_dates_train_test, "_score_test", name_ending, ".pdf"),
         width = 7.3,
         height = length(unique(results_models_summarized_model_dataset_kNp$label_dataset)) * 7.3 / length(unique(results_models_summarized_model_dataset_kNp$label_type_model)),
         units = c("in"))
  
  
  if (length(unique(results_models_summarized_model_dataset_kNp$label_dataset)) > 6) {
    
    list_label_dataset <- unique(results_models_summarized_model_dataset_kNp$label_dataset)
    
    for (ii in 1:ceiling(length(list_label_dataset)/6)) {
      
      plot_results_models_overview_score_test <- ggplot(data = results_models_summarized_model_dataset_kNp %>%
                                                          filter(label_dataset %in% list_label_dataset[((ii-1)*6+1):(min(ii*6, length(list_label_dataset)))])) +
        geom_tile(aes(x = factor(p), y = factor(k), fill = score_test)) +
        geom_text(aes(x = factor(p), y = factor(k), label = format(round(score_test, 2), nsmall = 2)),
                  color = "white", size = 2.5, fontface = "bold", angle = 0) +
        coord_equal() +
        facet_grid(rows = vars(label_dataset_linebreak), cols = vars(label_type_model),
                   labeller = labeller(type = label_parsed)) +
        xlab("p") +
        ylab("k") +
        scale_fill_viridis_c(name = "Score",
                             breaks = scale_breaks,
                             limits = scale_limits,
                             transform = scales::trans_new("transform_color", fun_transform_color, fun_inv_transform_color),
                             direction = -1) +
        theme_bw() +
        theme(legend.position = "bottom",
              strip.text.x = element_text(size = 6),
              strip.text.y = element_text(size = 6))
      
      ggsave(plot = plot_results_models_overview_score_test,
             filename = paste0(Directory_Plots, "results_models/summary/plot_results_models_overview_XY_", number_xy, "_PD_",
                               number_grid_combinations_kNp_dates_train_test, "_D_", group_dates_train_test, "_score_test", name_ending, "_", as.character(ii), ".pdf"),
             width = 7.3, height = ((min(ii*6, length(list_label_dataset))) - ((ii-1)*6+1) + 1)/6 * 9 + 1.6, units = c("in"))
      
    }
    
  }
  
  # (C) Plot of average performance of models per combination of k and p ----
  
  # `results_models`:
  # group by model and combination of k and p
  # determine average score per model and combination of k and p
  results_models_summarized_model_kNp <- results_models %>%
    group_by(label_type_model, number_combination_kNp) %>%
    summarize(k = unique(k),
              N = unique(N),
              p = unique(p),
              score_train = prod(frac_rmse_model_locf_train)^(1/n()),
              score_test = prod(frac_rmse_model_locf_test)^(1/n()),
              perc_frac_rmse_model_locf_train_smaller_one = sum(frac_rmse_model_locf_train < 1) / n(),
              perc_frac_rmse_model_locf_test_smaller_one = sum(frac_rmse_model_locf_test < 1) / n())
  
  plot_models_average_datasets_score_test <- ggplot(data = results_models_summarized_model_kNp) +
    geom_tile(aes(x = factor(p), y = factor(k), fill = score_test)) +
    geom_text(aes(x = factor(p), y = factor(k), label = format(round(score_test, 2), nsmall = 2)),
              color = "white", size = 3, fontface = "bold", angle = 0) +
    coord_equal() +
    facet_wrap(facets = vars(label_type_model), ncol = 4, labeller = labeller(type = label_parsed)) +
    xlab("p") +
    ylab("k") +
    scale_fill_viridis_c(name = "Score",
                         breaks = c(1, 2, 3),
                         limits = c(0.5, 3.5),
                         transform = scales::trans_new("transform_color", fun_transform_color, fun_inv_transform_color),
                         direction = -1) +
    theme_bw() +
    theme(legend.position = "bottom",
          strip.text.x = element_text(size = 6))
  
  # ggsave(plot = plot_models_average_datasets_score_test,
  #        filename = paste0(Directory_Plots, "results_models/summary/plot_results_models_average_XY_", number_xy, "_PD_",
  #                          number_grid_combinations_kNp_dates_train_test, "_D_", group_dates_train_test, "_score_test", name_ending, ".pdf"),
  #        width = 7.3, units = c("in"))
  
  
  # (D) Plot of average performance of datasets per combination of k and p ----
  
  # `results_models`:
  # group by dataset and combination of k and p
  # determine average score per dataset and combination of k and p
  results_models_summarized_dataset_kNp <- results_models %>%
    group_by(label_dataset, number_combination_kNp) %>%
    summarize(label_dataset_linebreak = unique(label_dataset_linebreak),
              k = unique(k),
              N = unique(N),
              p = unique(p),
              score_train = prod(frac_rmse_model_locf_train)^(1/n()),
              score_test = prod(frac_rmse_model_locf_test)^(1/n()),
              perc_frac_rmse_model_locf_train_smaller_one = sum(frac_rmse_model_locf_train < 1) / n(),
              perc_frac_rmse_model_locf_test_smaller_one = sum(frac_rmse_model_locf_test < 1) / n())
  
  plot_datasets_average_models_score_test <- ggplot(data = results_models_summarized_dataset_kNp) +
    geom_tile(aes(x = factor(p), y = factor(k), fill = score_test)) +
    geom_text(aes(x = factor(p), y = factor(k), label = format(round(score_test, 2), nsmall = 2)),
              color = "white", size = 3, fontface = "bold", angle = 0) +
    coord_equal() +
    facet_wrap(facets = vars(label_dataset_linebreak), ncol = 4, labeller = labeller(type = label_parsed)) +
    xlab("p") +
    ylab("k") +
    scale_fill_viridis_c(name = "Score",
                         breaks = c(1, 2, 3),
                         limits = c(0.5, 3.5),
                         transform = scales::trans_new("transform_color", fun_transform_color, fun_inv_transform_color),
                         direction = -1) +
    theme_bw() +
    theme(legend.position = "bottom",
          strip.text.x = element_text(size = 6))
  
  # ggsave(plot = plot_datasets_average_models_score_test,
  #        filename = paste0(Directory_Plots, "results_models/summary/plot_results_datasets_average_XY_", number_xy, "_PD_",
  #                          number_grid_combinations_kNp_dates_train_test, "_D_", group_dates_train_test, "_score_test", name_ending, ".pdf"),
  #        width = 7.3, units = c("in"))
  
  
  # (E) Plot grid of average performance of models and datasets per combination of k and p ----
  n_models <- results_models_summarized_model_kNp %>% pull(label_type_model) %>% unique() %>% length()
  n_datasets <- results_models_summarized_dataset_kNp %>% pull(label_dataset) %>% unique() %>% length()
  
  legend_models_datasets_average <- get_legend(plot_models_average_datasets_score_test)
  
  plot_grid_models_datasets_average <- plot_grid(plot_models_average_datasets_score_test +
                                                   guides(fill = "none") +
                                                   theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "in")),
                                                 plot_datasets_average_models_score_test +
                                                   guides(fill = "none") +
                                                   theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "in")),
                                                 as_ggplot(legend_models_datasets_average),
                                                 labels = c("A", "B", ""),
                                                 rel_heights = c(ceiling(n_models/4)+ 0.3, ceiling(n_datasets/4) + 0.3, 0.2),
                                                 ncol = 1)
  
  ggsave(plot = plot_grid_models_datasets_average,
         filename = paste0(Directory_Plots, "results_models/summary/plot_results_model_dataset_average_XY_", number_xy, "_PD_",
                           number_grid_combinations_kNp_dates_train_test, "_D_", group_dates_train_test, "_score_test", name_ending, ".pdf"),
         width = 7.3, height = 9.7, units = c("in"))

}


