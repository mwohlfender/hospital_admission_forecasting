
# create plot illustrating the parameters k, N and p
# more precise: grid of plots for multiple values of k (vertically), one value of N and multiple values of p (horizontally)


# range values for parameter k
k_range <- c(0, 7, 14, 21, 28)

# value for parameter N
N <- 7

# range of values for parameter p
p_range <- c(7, 14)

# space around polygon (between 0 and 1)
d <- 0.1

# additional vertical difference between two data samples
d_y <- 0.4

# number of data samples that shall be displayed
n_samples <- 5



data <- NULL

for (p in p_range) {
  
  for (k in k_range) {
    
    for (ii in 1:n_samples) {
      
      # determine x and y coordinates of corners of rectangles representing the period for which a forecast is made
      # dimensions of rectangle: N x 1
      # top left corner of rectangle in top row has coordinates (k_range[1]+d, -d)
      # the corners of the rectangle are defined in the following order: top left, bottom left, bottom right, top right
      data_temp <- tribble(~x, ~y, ~sample, ~period,
                           k+d+ii-1, -(ii-1)-d-(ii-1)*d_y, as.character(ii), "test",
                           k+d+ii-1, -ii+d-(ii-1)*d_y, as.character(ii), "test",
                           k+N-d+ii-1, -ii+d-(ii-1)*d_y, as.character(ii), "test",
                           k+N-d+ii-1, -(ii-1)-d-(ii-1)*d_y, as.character(ii), "test")
      
      data_temp <- data_temp %>%
        mutate(group = paste0("p_", p, "_k_", k, "_sample_", sample, "_", period),
               p = p,
               k = k)
      
      data <- data %>% bind_rows(data_temp)
      
      for (jj in 1:p) {
        
        # determine x and y coordinates of corners of squares representing observed data from past days
        # side length of square is 1
        # the center of each square is located at x and y coordinate of the form integer + 0.5
        # top left corner of rightmost square in top row has coordinates (-1+d, -d)
        # order of which squares for a combination parameters p and k are defined:
        # start with top right square, move to squares to the left, then down up to next row of squares
        # the corners of the rectangle are defined in the following order: top left, bottom left, bottom right, top right
        data_temp <- tribble(~x, ~y, ~sample, ~period,
                             -jj+d+(ii-1), -(ii-1)-d-(ii-1)*d_y, as.character(ii), "train",
                             -jj+d+(ii-1), -ii+d-(ii-1)*d_y, as.character(ii), "train",
                             -jj+1-d+(ii-1), -ii+d-(ii-1)*d_y, as.character(ii), "train",
                             -jj+1-d+(ii-1), -(ii-1)-d-(ii-1)*d_y, as.character(ii), "train")
        
        data_temp <- data_temp %>%
          mutate(group = paste0("p_", p, "_k_", k, "_sample_", sample,
                                "_day_", str_pad(as.character(jj), ceiling(log(x = p+1, base = 10)), pad = "0"),
                                "_", period),
                 p = p,
                 k = k)
        
        data <- data %>% bind_rows(data_temp)
        
      }
      
    }
    
  }
  
}

data <- data %>% arrange(p, k, group)



# define breaks and labels for x axis of plot
breaks_x <- c(-0.5-p_range+1, 0.5+k_range, 0.5+max(k_range)+N)
labels_x <- c(unlist(lapply(X = p_range,
                            FUN = function(x) paste0("t-", x))),
              unlist(lapply(X = k_range,
                            FUN = function(x) if (x == 0) {"t"} else {paste0("t+", x)})),
              paste0("t+", max(k_range+N)))


# create plot
plot_pNk <- ggplot() +
  geom_polygon(data = data %>% filter(period == "test"),
               mapping = aes(x = x, y = y, group = group, fill = period),
               color = "black", linewidth = 0.3) +
  geom_polygon(data = data %>% filter(period == "train"),
               mapping = aes(x = x, y = y, group = group, fill = period),
               color = "white", linewidth = 0.2) +
  scale_x_continuous(name = NULL,
                     breaks = breaks_x,
                     labels = labels_x) +
  scale_y_continuous(name = NULL,
                     limits = c(min(data$y) - d - 0.5, 0.5)) +
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") +
  scale_fill_manual(name = NULL,
                    breaks = c("train", "test"),
                    values = c("black", "white"),
                    labels = c("input", "forecast")) +
  facet_grid(rows = vars(k), cols = vars(p), labeller = label_both) +
  guides(fill = guide_legend(override.aes = list(color = "black", linewidth = 0.3))) +
  theme_bw() +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())


ggsave(plot = plot_pNk,
       filename = paste0(Directory_Plots, Subdirectory_Plots_Parameters, "plot_parameters_kNp.pdf"),
       width = 7.3, height = 4, units = c("in"))

ggsave(plot = plot_pNk,
       filename = paste0(Directory_Plots, Subdirectory_Plots_Manuscript, "Figure_1.pdf"),
       width = 7.3, height = 4, units = c("in"))


