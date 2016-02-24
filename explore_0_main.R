## These 4 lines let you install all packages that are necessary and then load them
list.of.packages <- c("ggplot2","gdata","grDevices","tm","RWeka","RJDBC")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

## To make sure that we are in the right working directory (allows for relative paths)
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("summaryTables.r")
## that explore_1_load_DB_table.R code is only usefull if you directly acces a data base. 
# source("exploratory/explore_1_load_DB_table.R")

## But we get our code from the car data
DF <- read.table("car_data/data/car_data_exploratory.txt",header=T,stringsAsFactors = FALSE)
tableName<-"CAR_INFO"
dimDF <- dim(DF)
colIsEmpty <- sapply(DF, function(x) sum(is.na(x))+sum(is.null(x))==dimDF[1])
colIsLogical <- sapply(DF, function(x) is.logical(x)) & !colIsEmpty
## Note: this might not be necessary, but I am not 100% sure if logicals are handled correctly
DF[,colIsLogical]<-DF[,colIsLogical]*1

## clean table mostly assures what type of data we have
source("exploratory/explore_2_clean_table.R")

source("exploratory/explore_3_viz.R")