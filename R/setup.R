# 
# 
# # load R packages ----
# 
# # tidyverse
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
# library(tibble)
# library(tidyselect)
# 
# # colors
# library(unibeCols) # yes
# 
# # data processing
library(data.table)
# library(reshape)
# 
# # dates
library(lubridate)
# 
# # map data
# library(sf)
# 
# # machine learning
library(stats)
library(Metrics)
# # library(keras)
# # library(tensorflow)
# library(xgboost)
# 
# # python
library(reticulate)

# # rounding
# library(scrutiny)
# 
# # time series
library(timetk)
library(zoo)

# # visualization
library(cowplot)
# library(DT)
# library(ggforce)
library(ggpubr)
library(ggridges)
# library(shiny)
library(flextable)
# library(officer)
library(scales)

# # varia
# library(NCmisc)





# set up python environment (NEEDS TO BE ADAPTED) ----
reticulate::use_python("PATH", required = TRUE)
reticulate::use_condaenv(condaenv = "NAME")



# define default values for flextables
set_flextable_defaults(background.color = "white")


# define global variables
source("R/setup_global_variables.R")


# source R functions ----
path_script_R <- "R/01_functions/"
files_R <- grep(dir(path = path_script_R, recursive = TRUE), pattern = "_old", invert = TRUE, value = TRUE)
lapply(X = files_R, FUN = function(x) {source(paste0(path_script_R, x), echo = FALSE)})


# define additional R functions ----

# rounding functions
custom_round_down <- function(x, accuracy) {
  
  if (accuracy == 0) {
    
    return(0)
    
  } else {
    
    accuracy <- abs(accuracy)
    
  }
  
  boundary_1 <- floor(x / accuracy) * accuracy
  boundary_2 <- ceiling(x / accuracy) * accuracy
  
  if (boundary_1 + accuracy <= x | boundary_2 - accuracy >= x | boundary_1 == boundary_2) {
    
    return(x)
    
  } else {
    
    return(boundary_1)
    
  }
  
}


custom_round_up <- function(x, accuracy) {
  
  if (accuracy == 0) {
    
    return(0)
    
  } else {
    
    accuracy <- abs(accuracy)
    
  }
  
  boundary_1 <- floor(x / accuracy) * accuracy
  boundary_2 <- ceiling(x / accuracy) * accuracy
  
  if (boundary_1 + accuracy <= x | boundary_2 - accuracy >= x | boundary_1 == boundary_2) {
    
    return(x)
    
  } else {
    
    return(boundary_2)
    
  }
  
}


custom_round <- function(x, accuracy) {
  
  x_round_up <- custom_round_up(x, accuracy)
  x_round_down <- custom_round_down(x, accuracy)
  
  if (abs(x - x_round_up) <= abs(x - x_round_down)) {
    
    return(x_round_up)
    
  } else {
    
    return(x_round_down)
    
  }
  
}


