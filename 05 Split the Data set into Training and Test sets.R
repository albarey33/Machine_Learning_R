#########################################################.
# SPLITTING THE DATA INTO TRAINING SET AND TEST SET
#########################################################.

rm(list=ls()) # Delete all objects
ls() # List variables in memory

options(header=T, sep=",", stringsAsFactors = FALSE, 
        str = strOptions(strict.width = "cut"), 
        readr.show_progress=FALSE)

##################################################################.
# 0 Load-packages -------------
#library(GGally) ## Adds pair-wise scatter plots to ggplot2
required_packages <- c("rlang", "ggplot2", "dplyr", "repr", "caret",
                       "hexbin", "GGally")
lapply(required_packages, library, character.only = TRUE)

##################################################################.
# 1 LOAD DATA THAT CONTAINS MISSING VALUES WITH "?" -----

auto_prices <- read.csv("Data_Files//auto_prices_wo_missing.csv", stringsAsFactors = FALSE, header = TRUE)

head(auto_prices, 12)
dim(auto_prices)


##################################################################.
# 2 SPLIT THE DATA SET -----
# Create randomly training and test sets 

set.seed(1955)
partition <- caret::createDataPartition(auto_prices[,"fuel.type"], times = 1, p = 0.75, list = FALSE)
training <- auto_prices[partition,]  # Create the training sample
test <- auto_prices[-partition,]  # Create the test sample
dim(training)
dim(test)




auto_prices <- read.csv("Data_Files//auto_prices_wo_missing.csv", stringsAsFactors = FALSE, header = TRUE)

head(auto_prices, 12)
dim(auto_prices)







