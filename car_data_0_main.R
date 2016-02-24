## These 4 lines let you install all packages that are necessary and then load them
list.of.packages <- c("plyr","dplyr","ggplot2","ggExtra","foreach","parallel",
                      "doParallel","randomForest","caret","reshape2","party","e1071")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

  ## To make sure that we are in the right working directory (allows for relative paths)
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

  ## Make sure folders for input and output exist
  ## showWarnings is of, because those folders will exist after the first run
dir.create("car_data", showWarnings = FALSE)
dir.create("car_output", showWarnings = FALSE)

### THE CODE
### DOWNLOAD THE DATA
# source("car_data/car_data_1_download.R")

## MERGE ALL TEXT FILES TO carData DATAFRAME. MUNG (CLEAN) THE DATA
source("car_data/car_data_2_merge_n_mung.R")

## DATA EXPLORATION USING GGPLOT2
source("car_data/car_data_3_exploratory_viz.R")

## GLM MODELLING 
source("car_data/car_data_4_glm.R")

## RANDOM FOREST
source("car_data/car_data_5_random_forest.R")

