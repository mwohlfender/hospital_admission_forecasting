
# create plot illustrating train-test splits
# more precise: grid of plots with plots of cut dates between training and testing periods for full and partial study period (top row)
# and plots of training and testing periods for full and partial study period (bottom row)


# read data defining train-test splits
table_dates_train_test_overview <- read_csv(file = Path_Table_Dates_Train_Test_Overview)
table_dates_train_test_detailed <- read_csv(file = Path_Table_Dates_Train_Test_Detailed)

# read time series of COVID-19-related hospital admissions
data_features_001_000_base <- read_csv(file = get_path_data_features(directory_data = paste0(Directory_Data, Subdirectory_Data_Feature_sets),
                                                                     number_xy = "001",
                                                                     number_combination_features = "000",
                                                                     name_data_set = "base",
                                                                     option = "NONE"))


# `data_features_001_000_base`: add x-coordinate used for creating plots
data_features_001_000_base <- data_features_001_000_base %>%
  mutate(cord_x = as.numeric(date_day - lubridate::ymd("2020-01-01")) + 1)


# start date of full study period
min_date_full <- table_dates_train_test_overview %>% filter(group == "002") %>% pull(date_min)

# end date of full study period
max_date_full <- table_dates_train_test_overview %>% filter(group == "002") %>% pull(date_max)

# split dates of full study period
dates_splits_full <- table_dates_train_test_detailed %>% filter(group == "002") %>% pull(date_min_test)

# start date, end date and cut dates of full study period and their x-coordinates
dates_start_splits_end_full <- c(min_date_full, dates_splits_full, max_date_full)
dates_start_splits_end_full_cord_x <- data_features_001_000_base %>% filter(date_day %in% dates_start_splits_end_full) %>% pull(cord_x)

# time series of COVID-19-related hospital admissions filtered to full study period
data_train_test_splits_full <- data_features_001_000_base %>%
  filter(!(is.na(y_07))) %>%
  filter(date_day >= min_date_full, min_date_full <= max_date_full)


# create plot: time series of COVID-19-related hospital admissions and cut dates of full study period
plot_split_dates_full <- ggplot(data = data_train_test_splits_full) +
  geom_line(mapping = aes(x = date_day, y = y_07)) +
  scale_x_date(date_labels = "%Y-%m-%d",
               breaks = c(ymd("2020-01-01"), ymd("2020-07-01"), ymd("2021-01-01"), ymd("2021-07-01"),
                          ymd("2022-01-01"), ymd("2022-07-01"), ymd("2023-01-01"), ymd("2023-07-01"))) +
  xlab(label = NULL) +
  ylab(label = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


for (ii in 1:length(dates_splits_full)) {
  
  plot_split_dates_full <- plot_split_dates_full +
    geom_vline(xintercept = dates_splits_full[ii], color = "firebrick3", linewidth = 0.5)
  
}


# create plot: training and testing periods of full study period
plot_train_test_periods_full <- ggplot() +
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[1]-0.5, xmax = dates_start_splits_end_full_cord_x[2]-1-0.5, ymin = 11.45, ymax = 12.05, fill = "A")) +
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[2]-0.5, xmax = dates_start_splits_end_full_cord_x[3]-1-0.5, ymin = 11.45, ymax = 12.05, fill = "B")) +
  
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[1]-0.5, xmax = dates_start_splits_end_full_cord_x[3]-1-0.5, ymin = 10.45, ymax = 11.05, fill = "A")) +
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[3]-0.5, xmax = dates_start_splits_end_full_cord_x[4]-1-0.5, ymin = 10.45, ymax = 11.05, fill = "B")) +
  
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[1]-0.5, xmax = dates_start_splits_end_full_cord_x[4]-1-0.5, ymin = 9.45, ymax = 10.05, fill = "A")) +
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[4]-0.5, xmax = dates_start_splits_end_full_cord_x[5]-1-0.5, ymin = 9.45, ymax = 10.05, fill = "B")) +
  
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[1]-0.5, xmax = dates_start_splits_end_full_cord_x[5]-1-0.5, ymin = 8.45, ymax = 9.05, fill = "A")) +
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[5]-0.5, xmax = dates_start_splits_end_full_cord_x[6]-1-0.5, ymin = 8.45, ymax = 9.05, fill = "B")) +
  
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[1]-0.5, xmax = dates_start_splits_end_full_cord_x[6]-1-0.5, ymin = 7.45, ymax = 8.05, fill = "A")) +
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[6]-0.5, xmax = dates_start_splits_end_full_cord_x[7]-1-0.5, ymin = 7.45, ymax = 8.05, fill = "B")) +
  
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[1]-0.5, xmax = dates_start_splits_end_full_cord_x[7]-1-0.5, ymin = 6.45, ymax = 7.05, fill = "A")) +
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[7]-0.5, xmax = dates_start_splits_end_full_cord_x[8]-1-0.5, ymin = 6.45, ymax = 7.05, fill = "B")) +
  
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[1]-0.5, xmax = dates_start_splits_end_full_cord_x[8]-1-0.5, ymin = 5.45, ymax = 6.05, fill = "A")) +
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[8]-0.5, xmax = dates_start_splits_end_full_cord_x[9]-1-0.5, ymin = 5.45, ymax = 6.05, fill = "B")) +
  
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[1]-0.5, xmax = dates_start_splits_end_full_cord_x[9]-1-0.5, ymin = 4.45, ymax = 5.05, fill = "A")) +
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[9]-0.5, xmax = dates_start_splits_end_full_cord_x[10]-1-0.5, ymin = 4.45, ymax = 5.05, fill = "B")) +
  
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[1]-0.5, xmax = dates_start_splits_end_full_cord_x[10]-1-0.5, ymin = 3.45, ymax = 4.05, fill = "A")) +
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[10]-0.5, xmax = dates_start_splits_end_full_cord_x[11]-1-0.5, ymin = 3.45, ymax = 4.05, fill = "B")) +
  
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[1]-0.5, xmax = dates_start_splits_end_full_cord_x[11]-1-0.5, ymin = 2.45, ymax = 3.05, fill = "A")) +
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[11]-0.5, xmax = dates_start_splits_end_full_cord_x[12]-1-0.5, ymin = 2.45, ymax = 3.05, fill = "B")) +
  
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[1]-0.5, xmax = dates_start_splits_end_full_cord_x[12]-1-0.5, ymin = 1.45, ymax = 2.05, fill = "A")) +
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[12]-0.5, xmax = dates_start_splits_end_full_cord_x[13]-1-0.5, ymin = 1.45, ymax = 2.05, fill = "B")) +
  
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[1]-0.5, xmax = dates_start_splits_end_full_cord_x[13]-1-0.5, ymin = 0.45, ymax = 1.05, fill = "A")) +
  geom_rect(aes(xmin = dates_start_splits_end_full_cord_x[13]-0.5, xmax = dates_start_splits_end_full_cord_x[14]-1-0.5, ymin = 0.45, ymax = 1.05, fill = "B")) +
  
  scale_x_continuous(breaks = c(dates_start_splits_end_full_cord_x),
                     labels = c(dates_start_splits_end_full),
                     limits = c(1, 1280)) +
  scale_y_continuous(breaks = c(),
                     labels = c()) +
  scale_fill_manual(name = NULL,
                    values = c(A = "forestgreen", B = "firebrick3"),
                    labels = c("training", "testing")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# start date of partial study period
min_date_partial <- table_dates_train_test_overview %>% filter(group == "006") %>% pull(date_min)

# end date of partial study period
max_date_partial <- table_dates_train_test_overview %>% filter(group == "006") %>% pull(date_max)

# split dates of partial study period
dates_splits_partial <- table_dates_train_test_detailed %>% filter(group == "006") %>% pull(date_min_test)

# start date, end date and cut dates of full study period and their x-coordinates
dates_start_splits_end_partial <- c(min_date_partial, dates_splits_partial, max_date_partial)
dates_start_splits_end_partial_cord_x <- data_features_001_000_base %>% filter(date_day %in% dates_start_splits_end_partial) %>% pull(cord_x)

# time series of COVID-19-related hospital admissions filtered to partial study period
data_train_test_splits_partial <- data_features_001_000_base %>%
  filter(!(is.na(y_07))) %>%
  filter(date_day >= min_date_partial, min_date_partial <= max_date_partial)


# create plot: time series of COVID-19-related hospital admissions and cut dates of partial study period
plot_split_dates_partial <- ggplot(data = data_train_test_splits_partial) +
  geom_line(mapping = aes(x = date_day, y = y_07)) +
  scale_x_date(date_labels = "%Y-%m-%d",
               breaks = c(ymd("2020-01-01"), ymd("2020-07-01"), ymd("2021-01-01"), ymd("2021-07-01"),
                          ymd("2022-01-01"), ymd("2022-07-01"), ymd("2023-01-01"), ymd("2023-07-01"))) +
  xlab(label = NULL) +
  ylab(label = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


for (ii in 1:length(dates_splits_partial)) {
  
  plot_split_dates_partial <- plot_split_dates_partial +
    geom_vline(xintercept = dates_splits_partial[ii], color = "firebrick3", linewidth = 0.5)
  
}


# create plot: training and testing periods of partial study period
plot_train_test_periods_partial <- ggplot() +
  geom_rect(aes(xmin = dates_start_splits_end_partial_cord_x[1]-0.5, xmax = dates_start_splits_end_partial_cord_x[2]-1-0.5, ymin = 3.45, ymax = 4.05, fill = "A")) +
  geom_rect(aes(xmin = dates_start_splits_end_partial_cord_x[2]-0.5, xmax = dates_start_splits_end_partial_cord_x[3]-1-0.5, ymin = 3.45, ymax = 4.05, fill = "B")) +
  
  geom_rect(aes(xmin = dates_start_splits_end_partial_cord_x[1]-0.5, xmax = dates_start_splits_end_partial_cord_x[3]-1-0.5, ymin = 2.45, ymax = 3.05, fill = "A")) +
  geom_rect(aes(xmin = dates_start_splits_end_partial_cord_x[3]-0.5, xmax = dates_start_splits_end_partial_cord_x[4]-1-0.5, ymin = 2.45, ymax = 3.05, fill = "B")) +
  
  geom_rect(aes(xmin = dates_start_splits_end_partial_cord_x[1]-0.5, xmax = dates_start_splits_end_partial_cord_x[4]-1-0.5, ymin = 1.45, ymax = 2.05, fill = "A")) +
  geom_rect(aes(xmin = dates_start_splits_end_partial_cord_x[4]-0.5, xmax = dates_start_splits_end_partial_cord_x[5]-1-0.5, ymin = 1.45, ymax = 2.05, fill = "B")) +
  
  geom_rect(aes(xmin = dates_start_splits_end_partial_cord_x[1]-0.5, xmax = dates_start_splits_end_partial_cord_x[5]-1-0.5, ymin = 0.45, ymax = 1.05, fill = "A")) +
  geom_rect(aes(xmin = dates_start_splits_end_partial_cord_x[5]-0.5, xmax = dates_start_splits_end_partial_cord_x[6]-1-0.5, ymin = 0.45, ymax = 1.05, fill = "B")) +
  
  scale_x_continuous(breaks = c(dates_start_splits_end_partial_cord_x),
                     labels = c(dates_start_splits_end_partial),
                     limits = c(min(dates_start_splits_end_partial_cord_x)-30, max(dates_start_splits_end_partial_cord_x))) +
  scale_y_continuous(breaks = c(),
                     labels = c()) +
  scale_fill_manual(name = NULL,
                    values = c(A = "forestgreen", B = "firebrick3"),
                    labels = c("training", "testing")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# extract legend of plot of training and testing periods of full study period
legend_train_test <- ggpubr::get_legend(plot_train_test_periods_full)

# create grid of plots
# top row: time series of COVID-19-related hospital admissions and cut dates of full and partial study period
# bottom row: training and testing periods of full and partial study period
plot_grid_train_test <- cowplot::plot_grid(plot_split_dates_full +
                                             theme(plot.margin = unit(c(0.1,0.1,0.1,0.5), "in")),
                                           plot_split_dates_partial +
                                             theme(plot.margin = unit(c(0.1,0.1,0.1,0.5), "in")),
                                           plot_train_test_periods_full +
                                             theme(plot.margin = unit(c(0.1,0.1,0.1,0.5), "in")) +
                                             guides(fill = "none"),
                                           plot_train_test_periods_partial +
                                             theme(plot.margin = unit(c(0.1,0.1,0.1,0.5), "in")) +
                                             guides(fill = "none"),
                                           nrow = 2,
                                           labels = c("A", "B", "C", "D"),
                                           rel_widths = c(4, 3),
                                           rel_heights = 1)

plot_grid_train_test <- cowplot::plot_grid(plot_grid_train_test,
                                           ggpubr::as_ggplot(legend_train_test),
                                           rel_heights = c(1, 0.075),
                                           nrow = 2)


ggsave(plot = plot_grid_train_test,
       filename = paste0(Directory_Plots, Subdirectory_Plots_Parameters, "plot_grid_train_test.pdf"),
       width = 7.3, height = 8, units = c("in"))

ggsave(plot = plot_grid_train_test,
       filename = paste0(Directory_Plots, Subdirectory_Plots_Supplementary, "supplementary_figure_S1.pdf"),
       width = 7.3, height = 8, units = c("in"))


