# 
# 
# # load R packages ----
# 
# # tidyverse
# library(ggplot2) # yes
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
# library(data.table)
# library(reshape)
# 
# # dates
library(lubridate)
# 
# # map data
# library(sf)
# 
# # machine learning
# library(stats)
# library(Metrics)
# # library(keras)
# # library(tensorflow)
# library(xgboost)
# 
# # python
# library(reticulate)
# 
# # rounding
# library(scrutiny)
# 
# # time series
# library(timetk)
library(zoo)

# # visualization
# library(cowplot)
# library(DT)
# library(ggforce)
# library(ggpubr)
# library(ggridges)
# library(shiny)
# library(flextable)
# library(officer)
library(scales)

# # varia
# library(NCmisc)





# # set up python environment ----
# use_python("C:/Users/mw22f082/AppData/Local/anaconda3", required = TRUE)
# use_condaenv(condaenv = "testpython310")
# source_python("python/setup.py")


# define default values for flextables
# set_flextable_defaults(background.color = "white")


# define global variables
source("R/setup_global_variables.R")


# source R functions ----
path_script_R <- "R/01_functions/"
files_R <- grep(dir(path = path_script_R, recursive = TRUE), pattern = "_old", invert = TRUE, value = TRUE)
lapply(X = files_R, FUN = function(x) {source(paste0(path_script_R, x), echo = FALSE)})


# # source python functions ----
# path_script_python <- "python/functions/"
# files_python <- dir(path = path_script_python)
# lapply(X = files_python, FUN = function(x) {source_python(paste0(path_script_python, x))})


