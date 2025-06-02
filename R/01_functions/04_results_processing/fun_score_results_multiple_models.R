

# directory_results <- "C:/Users/mw22f082/Documents_MW/projects/defeat_covid/ubelix/indicators/000_A_process_results/results/"
# directory_results <- paste0(Directory_Ubelix, "000_A_process_results/results/")
# number_xy <- "001"
# list_numbers_grid_combinations_kNp_dates_train_test <- c("002", "003")
# filter_k <- NULL
# filter_p <- NULL
# list_groups_dates_train_test <- c("002", "006")
# labels_study_period <- c("002" = "Full study period", "006" = "Partial study period")
# list_models <- c("lr", "nn", "lstm", "xgb")
# list_hyp_par_grids <- c("NONE", "003", "001", "006")
# filter_models <- NULL
# filter_datasets <- NULL
# name_ending <- ""


score_results_multiple_models <- function(directory_results = Directory_Results,
                                          number_xy = "001",
                                          list_numbers_grid_combinations_kNp_dates_train_test,
                                          filter_k = NULL,
                                          filter_p = NULL,
                                          list_groups_dates_train_test,
                                          labels_study_period,
                                          list_models = c("lr", "nn", "lstm", "xgb"),
                                          list_hyp_par_grids = c("NONE", "003", "001", "006"),
                                          filter_models = NULL,
                                          filter_datasets = NULL,
                                          name_ending = "",
                                          plot_manuscript = NULL,
                                          plot_supplementary = NULL,
                                          plot_presentation = NULL) {
  
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
  
  results_models <- NULL
  
  for (aa in 1:length(list_numbers_grid_combinations_kNp_dates_train_test)) {
    
    group_dates_train_test_0 <- list_groups_dates_train_test[aa]
    
    # read results of locf model
    results_locf <- read_csv(file = get_path_results_processed_summary(directory_results = directory_results,
                                                                       type_model = "locf",
                                                                       number_xy = number_xy,
                                                                       number_grid_combinations_kNp_dates_train_test = list_numbers_grid_combinations_kNp_dates_train_test[aa]))
    
    results_locf <- results_locf %>%
      filter(group_dates_train_test == group_dates_train_test_0) %>%
      mutate(type_model = "locf") %>%
      dplyr::select(c("type_model", "number_combination_kNp", "k", "N", "p", "group_dates_train_test", "number_dates_train_test", "rmse_locf_train", "rmse_locf_test"))
    
    
    results_models_0 <- NULL
    
    for (ii in 1:length(list_models)) {
      
      if (list_models[ii] == "lr") {
        
        pattern_model_ii <- paste0("results_summary_", list_models[ii], "_XY_", number_xy, "_*", "_PD_", list_numbers_grid_combinations_kNp_dates_train_test[aa], ".csv")
        
      } else {
        
        if (list_hyp_par_grids[ii] == "NONE") {
          
          pattern_model_ii <- paste0("results_detailed_", list_models[ii], "_XY_", number_xy, "_*", "_PD_", list_numbers_grid_combinations_kNp_dates_train_test[aa], ".csv")
          
        } else {
          
          pattern_model_ii <- paste0("results_detailed_", list_models[ii], "_XY_", number_xy, "_*", "_PD_", list_numbers_grid_combinations_kNp_dates_train_test[aa],
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
    
    results_models <- results_models %>%
      bind_rows(results_models_0 %>%
                  left_join(results_locf %>% dplyr::select(number_combination_kNp,
                                                           group_dates_train_test,
                                                           number_dates_train_test,
                                                           rmse_locf_train,
                                                           rmse_locf_test),
                            by = c("number_combination_kNp" = "number_combination_kNp",
                                   "group_dates_train_test" = "group_dates_train_test",
                                   "number_dates_train_test" = "number_dates_train_test")))
    
  }
  
  results_models <- results_models %>%
    mutate(frac_rmse_model_locf_train = rmse_train / rmse_locf_train,
           frac_rmse_model_locf_test = rmse_test / rmse_locf_test) %>%
    left_join(table_models, by = "type_model") %>%
    left_join(table_datasets, by = c("number_xy" = "number_xy",
                                     "number_combination_features" = "number_combination_features",
                                     "name_data_set" = "name_data_set"))
  
  if (!(is.null(filter_k))) {
    
    results_models <- results_models %>%
      filter(k %in% filter_k)
    
  }
  
  if (!(is.null(filter_p))) {
    
    results_models <- results_models %>%
      filter(p %in% filter_p)
    
  }
  
  if (!(is.null(filter_models))) {
    
    results_models <- results_models %>%
      filter(label_type_model %in% filter_models)
    
  }
  
  if (!(is.null(filter_datasets))) {
    
    results_models <- results_models %>%
      filter(label_dataset %in% filter_datasets)
    
  }
  
  # `results_models`:
  # group by model, dataset and combination of k and p
  # determine average score per model, dataset and combination of k and p
  results_models_summarized_model_dataset_kNp <- results_models %>%
    group_by(label_type_model, label_dataset, number_combination_kNp, group_dates_train_test) %>%
    summarize(label_dataset_linebreak = unique(label_dataset_linebreak),
              k = unique(k),
              N = unique(N),
              p = unique(p),
              score_train = prod(frac_rmse_model_locf_train)^(1/n()),
              score_test = prod(frac_rmse_model_locf_test)^(1/n()),
              perc_frac_rmse_model_locf_train_smaller_one = sum(frac_rmse_model_locf_train < 1) / n(),
              perc_frac_rmse_model_locf_test_smaller_one = sum(frac_rmse_model_locf_test < 1) / n())
  
  
  # (A) Plot of best model per combination of k and p (minimum across datasets) ----
  
  # `results_models_summarized_model_dataset_kNp`:
  # group by combination of k and p
  # determine best model per combination of k and p
  results_models_summarized_best_model_minimum <- results_models_summarized_model_dataset_kNp %>%
    group_by(number_combination_kNp, group_dates_train_test) %>%
    summarize(k = unique(k),
              N = unique(N),
              p = unique(p),
              min_score_train = min(score_train),
              min_score_test = min(score_test),
              max_perc_frac_rmse_model_locf_train_smaller_one = max(perc_frac_rmse_model_locf_train_smaller_one),
              max_perc_frac_rmse_model_locf_test_smaller_one = max(perc_frac_rmse_model_locf_test_smaller_one),
              best_model_score_train = if (min(score_train) <= 1) {label_type_model[score_train == min(score_train)][1]} else {"LOCF"},
              best_model_score_test = if (min(score_test) <= 1) {label_type_model[score_test == min(score_test)][1]} else {"LOCF"},
              best_model_perc_frac_rmse_model_locf_train_smaller_one = label_type_model[perc_frac_rmse_model_locf_train_smaller_one == max(perc_frac_rmse_model_locf_train_smaller_one)][1],
              best_model_perc_frac_rmse_model_locf_test_smaller_one = label_type_model[perc_frac_rmse_model_locf_test_smaller_one == max(perc_frac_rmse_model_locf_test_smaller_one)][1])
  
  
  plot_best_model_minimum_datasets_score_test <- ggplot(data = results_models_summarized_best_model_minimum) +
    geom_raster(aes(x = factor(p), y = factor(k), fill = best_model_score_test)) +
    geom_text(aes(x = factor(p), y = factor(k), label = format(round(min_score_test, 2), nsmall = 2), color = best_model_score_test),
              size = 5, fontface = "bold", angle = 0) +
    facet_wrap(facets = vars(group_dates_train_test), nrow = 1,
               labeller = labeller(group_dates_train_test = labels_study_period)) +
    xlab("p") +
    ylab("k") +
    scale_color_manual(name = NULL,
                       breaks = table_models %>% pull(label_type_model),
                       values = table_models %>% pull(color_model_font)) +
    scale_fill_manual(name = "Model",
                      breaks = table_models %>% pull(label_type_model),
                      values = table_models %>% pull(color_model)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal") +
    guides(color = "none")
  
  ggsave(plot = plot_best_model_minimum_datasets_score_test,
         filename = paste0(Directory_Plots, "results_models/summary/plot_results_best_model_minimum_XY_", number_xy, "_PD_",
                           paste(list_numbers_grid_combinations_kNp_dates_train_test, collapse = '_'), "_D_", paste(list_groups_dates_train_test, collapse = '_'), "_score_test", name_ending, ".pdf"),
         width = 11.7, height = 5, units = c("in"))
  
  
  # (B) Plot of best dataset per combination of k and p (minimum across models) ----
  
  # `results_models_summarized_model_dataset_kNp`:
  # group by combination of k and p
  # determine best model per combination of k and p
  results_models_summarized_best_dataset_minimum <- results_models_summarized_model_dataset_kNp %>%
    group_by(number_combination_kNp, group_dates_train_test) %>%
    summarize(k = unique(k),
              N = unique(N),
              p = unique(p),
              min_score_train = min(score_train),
              min_score_test = min(score_test),
              max_perc_frac_rmse_model_locf_train_smaller_one = max(perc_frac_rmse_model_locf_train_smaller_one),
              max_perc_frac_rmse_model_locf_test_smaller_one = max(perc_frac_rmse_model_locf_test_smaller_one),
              best_dataset_score_train = sort(label_dataset_linebreak[score_train == min(score_train)])[1],
              best_dataset_score_test = sort(label_dataset_linebreak[score_test == min(score_test)])[1],
              best_dataset_perc_frac_rmse_model_locf_train_smaller_one = sort(label_dataset_linebreak[perc_frac_rmse_model_locf_train_smaller_one == max(perc_frac_rmse_model_locf_train_smaller_one)])[1],
              best_dataset_perc_frac_rmse_model_locf_test_smaller_one = sort(label_dataset_linebreak[perc_frac_rmse_model_locf_test_smaller_one == max(perc_frac_rmse_model_locf_test_smaller_one)])[1])
  
  
  plot_best_dataset_minimum_models_score_test <- ggplot(data = results_models_summarized_best_dataset_minimum) +
    geom_raster(aes(x = factor(p), y = factor(k), fill = factor(best_dataset_score_test))) +
    geom_text(aes(x = factor(p), y = factor(k), label = format(round(min_score_test, 2), nsmall = 2), color = best_dataset_score_test),
              size = 5, fontface = "bold", angle = 0) +
    facet_wrap(facets = vars(group_dates_train_test), nrow = 1,
               labeller = labeller(group_dates_train_test = labels_study_period)) +
    xlab("p") +
    ylab("k") +
    scale_color_manual(name = NULL,
                       breaks = table_datasets %>% pull(label_dataset_linebreak),
                       values = table_datasets %>% pull(color_dataset_font)) +
    scale_fill_manual(name = "Feature set",
                      breaks = table_datasets %>% pull(label_dataset_linebreak),
                      values = table_datasets %>% pull(color_dataset)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal") +
    guides(color = "none",
           fill = guide_legend(ncol = 2, byrow = FALSE))
  
  ggsave(plot = plot_best_dataset_minimum_models_score_test,
         filename = paste0(Directory_Plots, "results_models/summary/plot_results_best_dataset_minimum_XY_", number_xy, "_PD_",
                           paste(list_numbers_grid_combinations_kNp_dates_train_test, collapse = '_'), "_D_", paste(list_groups_dates_train_test, collapse = '_'), "_score_test", name_ending, ".pdf"),
         width = 11.7, height = 5, units = c("in"))
  
  
  # (C) plot grid of `plot_best_model_minimum_datasets_score_test`, `plot_best_dataset_minimum_models_score_test`
  legend_best_models_minimum <- get_legend(plot_best_model_minimum_datasets_score_test)
  legend_best_datasets_minimum <- get_legend(plot_best_dataset_minimum_models_score_test)
  
  plot_grid_best_model_best_dataset_minimum <- plot_grid(plot_best_model_minimum_datasets_score_test +
                                                           guides(fill = "none") +
                                                           theme(plot.margin = unit(c(0.1,0.1,0,0.1), "in")),
                                                         as_ggplot(legend_best_models_minimum),
                                                         plot_best_dataset_minimum_models_score_test +
                                                           guides(fill = "none") +
                                                           theme(plot.margin = unit(c(0.1,0.1,0,0.1), "in")),
                                                         as_ggplot(legend_best_datasets_minimum),
                                                         labels = c("A", "", "B", ""),
                                                         rel_heights = c(1, 0.2, 1, max(0.2, length(unique(results_models_summarized_best_dataset_minimum$best_dataset_score_test))/2*0.15)),
                                                         ncol = 1)
  
  ggsave(plot = plot_grid_best_model_best_dataset_minimum,
         filename = paste0(Directory_Plots, "results_models/summary/plot_results_best_model_dataset_minimum_XY_", number_xy, "_PD_",
                           paste(list_numbers_grid_combinations_kNp_dates_train_test, collapse = '_'), "_D_", paste(list_groups_dates_train_test, collapse = '_'), "_score_test", name_ending, ".pdf"),
         width = 7.3, height = 7.3, units = c("in"))
  
  if (!(is.null(plot_manuscript))) {
    
    ggsave(plot = plot_grid_best_model_best_dataset_minimum,
           filename = paste0(Directory_Plots, Subdirectory_Plots_Manuscript, plot_manuscript, ".pdf"),
           width = 7.3, height = 7.3, units = c("in"))
    
  }
  
  if (!(is.null(plot_supplementary))) {
    
    ggsave(plot = plot_grid_best_model_best_dataset_minimum,
           filename = paste0(Directory_Plots, Subdirectory_Plots_Supplementary, plot_supplementary, ".pdf"),
           width = 7.3, height = 7.3, units = c("in"))
    
  }
  
  if (!(is.null(plot_presentation))) {
    
    ggsave(plot = plot_grid_best_model_best_dataset_minimum,
           filename = paste0(Directory_Plots, "results_models/summary/plot_results_best_model_dataset_minimum_XY_", number_xy, "_PD_",
                             paste(list_numbers_grid_combinations_kNp_dates_train_test, collapse = '_'), "_D_", paste(list_groups_dates_train_test, collapse = '_'), "_score_test", name_ending, ".pdf"),
           width = 7.3, height = 7.3, units = c("in"))
    
  }
  
  
  # (D) Plot of best model per combination of k and p (average across datasets) ----
  
  # `results_models`:
  # group by model and combination of k and p
  # determine average score per model and combination of k and p
  results_models_summarized_model_kNp <- results_models %>%
    group_by(label_type_model, number_combination_kNp, group_dates_train_test) %>%
    summarize(k = unique(k),
              N = unique(N),
              p = unique(p),
              score_train = prod(frac_rmse_model_locf_train)^(1/n()),
              score_test = prod(frac_rmse_model_locf_test)^(1/n()),
              perc_frac_rmse_model_locf_train_smaller_one = sum(frac_rmse_model_locf_train < 1) / n(),
              perc_frac_rmse_model_locf_test_smaller_one = sum(frac_rmse_model_locf_test < 1) / n())
  
  
  # `results_models_summarized_model_kNp`:
  # group by combination of k and p
  # determine best model per combination of k and p
  results_models_summarized_best_model_average <- results_models_summarized_model_kNp %>%
    group_by(number_combination_kNp, group_dates_train_test) %>%
    summarize(k = unique(k),
              N = unique(N),
              p = unique(p),
              min_score_train = min(score_train),
              min_score_test = min(score_test),
              max_perc_frac_rmse_model_locf_train_smaller_one = max(perc_frac_rmse_model_locf_train_smaller_one),
              max_perc_frac_rmse_model_locf_test_smaller_one = max(perc_frac_rmse_model_locf_test_smaller_one),
              best_model_score_train = if (min(score_train) <= 1) {label_type_model[score_train == min(score_train)][1]} else {"LOCF"},
              best_model_score_test = if (min(score_test) <= 1) {label_type_model[score_test == min(score_test)][1]} else {"LOCF"},
              best_model_perc_frac_rmse_model_locf_train_smaller_one = label_type_model[perc_frac_rmse_model_locf_train_smaller_one == max(perc_frac_rmse_model_locf_train_smaller_one)][1],
              best_model_perc_frac_rmse_model_locf_test_smaller_one = label_type_model[perc_frac_rmse_model_locf_test_smaller_one == max(perc_frac_rmse_model_locf_test_smaller_one)][1])
  
  
  plot_best_model_average_datasets_score_test <- ggplot(data = results_models_summarized_best_model_average) +
    geom_raster(aes(x = factor(p), y = factor(k), fill = best_model_score_test)) +
    geom_text(aes(x = factor(p), y = factor(k), label = format(round(min_score_test, 2), nsmall = 2), color = best_model_score_test),
              size = 5, fontface = "bold", angle = 0) +
    facet_wrap(facets = vars(group_dates_train_test), nrow = 1,
               labeller = labeller(group_dates_train_test = labels_study_period)) +
    xlab("p") +
    ylab("k") +
    scale_color_manual(name = NULL,
                       breaks = table_models %>% pull(label_type_model),
                       values = table_models %>% pull(color_model_font)) +
    scale_fill_manual(name = "Model",
                      breaks = table_models %>% pull(label_type_model),
                      values = table_models %>% pull(color_model)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal") +
    guides(color = "none")
  
  ggsave(plot = plot_best_model_average_datasets_score_test,
         filename = paste0(Directory_Plots, "results_models/summary/plot_results_best_model_average_XY_", number_xy, "_PD_",
                           paste(list_numbers_grid_combinations_kNp_dates_train_test, collapse = '_'), "_D_", paste(list_groups_dates_train_test, collapse = '_'), "_score_test", name_ending, ".pdf"),
         width = 11.7, height = 5, units = c("in"))
  
  
  # (E) Plot of best dataset per combination of k and p (average across models) ----
  
  # `results_models`:
  # group by dataset and combination of k and p
  # determine average score per dataset and combination of k and p
  results_models_summarized_dataset_kNp <- results_models %>%
    group_by(label_dataset, number_combination_kNp, group_dates_train_test) %>%
    summarize(label_dataset_linebreak = unique(label_dataset_linebreak),
              k = unique(k),
              N = unique(N),
              p = unique(p),
              score_train = prod(frac_rmse_model_locf_train)^(1/n()),
              score_test = prod(frac_rmse_model_locf_test)^(1/n()),
              perc_frac_rmse_model_locf_train_smaller_one = sum(frac_rmse_model_locf_train < 1) / n(),
              perc_frac_rmse_model_locf_test_smaller_one = sum(frac_rmse_model_locf_test < 1) / n())
  
  
  # `results_models_summarized_dataset_kNp`:
  # group by combination of k and p
  # determine best model per combination of k and p
  results_models_summarized_best_dataset_average <- results_models_summarized_dataset_kNp %>%
    group_by(number_combination_kNp, group_dates_train_test) %>%
    summarize(k = unique(k),
              N = unique(N),
              p = unique(p),
              min_score_train = min(score_train),
              min_score_test = min(score_test),
              max_perc_frac_rmse_model_locf_train_smaller_one = max(perc_frac_rmse_model_locf_train_smaller_one),
              max_perc_frac_rmse_model_locf_test_smaller_one = max(perc_frac_rmse_model_locf_test_smaller_one),
              best_dataset_score_train = sort(label_dataset_linebreak[score_train == min(score_train)])[1],
              best_dataset_score_test = sort(label_dataset_linebreak[score_test == min(score_test)])[1],
              best_dataset_perc_frac_rmse_model_locf_train_smaller_one = sort(label_dataset_linebreak[perc_frac_rmse_model_locf_train_smaller_one == max(perc_frac_rmse_model_locf_train_smaller_one)])[1],
              best_dataset_perc_frac_rmse_model_locf_test_smaller_one = sort(label_dataset_linebreak[perc_frac_rmse_model_locf_test_smaller_one == max(perc_frac_rmse_model_locf_test_smaller_one)])[1])
  
  
  plot_best_dataset_average_models_score_test <- ggplot(data = results_models_summarized_best_dataset_average) +
    geom_raster(aes(x = factor(p), y = factor(k), fill = factor(best_dataset_score_test))) +
    geom_text(aes(x = factor(p), y = factor(k), label = format(round(min_score_test, 2), nsmall = 2), color = best_dataset_score_test),
              size = 5, fontface = "bold", angle = 0) +
    facet_wrap(facets = vars(group_dates_train_test), nrow = 1,
               labeller = labeller(group_dates_train_test = labels_study_period)) +
    xlab("p") +
    ylab("k") +
    scale_color_manual(name = NULL,
                       breaks = table_datasets %>% pull(label_dataset_linebreak),
                       values = table_datasets %>% pull(color_dataset_font)) +
    scale_fill_manual(name = "Feature set",
                      breaks = table_datasets %>% pull(label_dataset_linebreak),
                      values = table_datasets %>% pull(color_dataset)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal") +
    guides(color = "none",
           fill = guide_legend(ncol = 2, byrow = FALSE))
  
  ggsave(plot = plot_best_dataset_average_models_score_test,
         filename = paste0(Directory_Plots, "results_models/summary/plot_results_best_dataset_average_XY_", number_xy, "_PD_",
                           paste(list_numbers_grid_combinations_kNp_dates_train_test, collapse = '_'), "_D_", paste(list_groups_dates_train_test, collapse = '_'), "_score_test", name_ending, ".pdf"),
         width = 11.7, height = 5, units = c("in"))
  
  
  # (F) plot grid of `plot_best_model_average_datasets_score_test`, `plot_best_dataset_average_models_score_test`
  legend_best_models_average <- get_legend(plot_best_model_average_datasets_score_test)
  legend_best_datasets_average <- get_legend(plot_best_dataset_average_models_score_test)
  
  plot_grid_best_model_best_dataset_average <- plot_grid(plot_best_model_average_datasets_score_test +
                                                           guides(fill = "none") +
                                                           theme(plot.margin = unit(c(0.1,0.1,0,0.1), "in")),
                                                         as_ggplot(legend_best_models_average),
                                                         plot_best_dataset_average_models_score_test +
                                                           guides(fill = "none") +
                                                           theme(plot.margin = unit(c(0.1,0.1,0,0.1), "in")),
                                                         as_ggplot(legend_best_datasets_average),
                                                         labels = c("A", "", "B", ""),
                                                         rel_heights = c(1, 0.2, 1, max(0.2, length(unique(results_models_summarized_best_dataset_average$best_dataset_score_test))/2*0.15)),
                                                         ncol = 1)
  
  ggsave(plot = plot_grid_best_model_best_dataset_average,
         filename = paste0(Directory_Plots, "results_models/summary/plot_results_best_model_dataset_average_XY_", number_xy, "_PD_",
                           paste(list_numbers_grid_combinations_kNp_dates_train_test, collapse = '_'), "_D_", paste(list_groups_dates_train_test, collapse = '_'), "_score_test", name_ending, ".pdf"),
         width = 7.3, height = 7.3, units = c("in"))
  
}


