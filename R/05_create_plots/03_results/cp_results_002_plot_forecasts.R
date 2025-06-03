

# create Figure 3 of manuscript, examples of forecasts of COVID-19-related hospital admissions

type_model <- "xgb"
number_xy <- "001"
number_combination_features <- "000"
name_data_set <- "base"
number_grid_combinations_kNp_dates_train_test <- "002"
filter_k <- c(0, 7, 14)
N0 <- 7
filter_p <- c(28)
number_hyp_par_grid <- "006"


date_zero <- lubridate::ymd("2022-09-09")
date_diff <- 28
list_dates = c(date_zero,
                        date_zero + date_diff,
                        date_zero + 2*date_diff,
                        date_zero + 3*date_diff,
                        date_zero + 4*date_diff,
                        date_zero + 5*date_diff)

date_plot_min <- date_zero - 21
date_plot_max <- date_zero + 5*date_diff + 21


path_data_base <- get_path_features_target(number_xy = "001",
                                           number_combination_features = "000",
                                           name_data_set = "base",
                                           option_output = "features_target",
                                           directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                           option_path = "long_directory_filename")

name_column_y_old <- paste0("y_", str_pad(N0, 2, pad = "0"))
name_column_y_new <- "y"

data_base <- read_csv(file = path_data_base) %>%
  na.omit() %>%
  filter(date_day >= date_plot_min, date_day <= date_plot_max) %>%
  dplyr::rename_with(~c(name_column_y_new), tidyselect::all_of(name_column_y_old))




forecasts <- read_csv(file = get_path_results_forecasts(type_model = type_model,
                                                        number_xy = number_xy,
                                                        number_combination_features = number_combination_features,
                                                        name_data_set = name_data_set,
                                                        number_grid_combinations_kNp_dates_train_test = number_grid_combinations_kNp_dates_train_test,
                                                        number_hyp_par_grid = number_hyp_par_grid,
                                                        type_period = "test",
                                                        directory_results = Directory_Results,
                                                        option = "directory_filename"))

if (!(is.null(filter_k))) {
  
  forecasts <- forecasts %>% filter(k %in% filter_k)
  
}

if (!(is.null(filter_p))) {
  
  forecasts <- forecasts %>% filter(p %in% filter_p)
  
}

forecasts_list_dates <- forecasts %>%
  filter(date_day %in% list_dates, N == N0)

min_scale_y <- min(data_base %>% pull(y), forecasts_list_dates %>% pull(y), 0)
max_scale_y <- max(data_base %>% pull(y), forecasts_list_dates %>% pull(y)) + 1




data_prior_dates_forecast <- tibble(date_day = list_dates-7) %>%
  left_join(data_base %>% dplyr::select(c("date_day", "y")), by = "date_day") %>%
  mutate(group = 1:length(list_dates),
         category = "data") %>%
  bind_rows(forecasts_list_dates %>%
              filter(k == 0) %>%
              dplyr::select(c("date_day", "y")) %>%
              mutate(group = 1:length(list_dates),
                     category = "forecast"))


plot_forecasts <- ggplot() +
  geom_vline(xintercept = list_dates, color = "steelblue", linetype = "dashed") +
  geom_line(data = data_base, aes(x = date_day, y = y, color = factor("color_last_observation")), linewidth = 0.2) +
  geom_line(data = data_base, aes(x = date_day, y = y, color = factor("color_observations")), linewidth = 0.2) +
  geom_point(data = data_base, aes(x = date_day, y = y, color = factor("color_observations")), size = 0.7, shape = 21, stroke = 0.3, fill = "white") +
  geom_line(data = data_prior_dates_forecast, aes(x = date_day, y = y, color = factor("color_forecast"), group = group), linetype = "dashed") +
  geom_line(data = forecasts_list_dates, aes(x = date_day_shift_k, y = y, color = factor("color_forecast"), group = interaction(date_day, p))) +
  geom_point(data = forecasts_list_dates, aes(x = date_day_shift_k, y = y, color = factor("color_forecast"), group = interaction(date_day, p))) +
  geom_point(data = data_prior_dates_forecast %>% filter(category == "data"), aes(x = date_day, y = y, color = factor("color_last_observation")), shape = 19, stroke = 0.3) +
  scale_y_continuous(name = NULL,
                     limits = c(custom_round_down(x = min_scale_y, accuracy = 5),
                                custom_round_up(x = max_scale_y, accuracy = 25))) +
  scale_color_manual(name = NULL,
                     breaks = c(factor("color_observations"), factor("color_last_observation"), factor("color_forecast")),
                     labels = c("Observation", "Last value observed", "Forecast"),
                     values = c("black", "black", "firebrick3")) +
  scale_x_date(name = NULL,
               date_labels = "%d/%m/%Y",
               breaks = c(date_plot_min, list_dates, date_plot_max)) +
  theme_bw() +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")


ggsave(plot = plot_forecasts,
       filename = paste0(Directory_Plots, Subdirectory_Plots_Manuscript, "Figure_3.pdf"),
       width = 7.3, height = 7.3/ 1.618, units = c("in"))


