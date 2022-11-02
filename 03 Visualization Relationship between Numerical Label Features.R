#########################################################.
# DATA EXPLORATORY
# VISUALIZATION RELATIONSHIP BETWEEN NUMERICAL LABEL FEATURES
# TWO DIMENSIONAL PLOTS
# INTRODUCTION TO MACHINE LEARNING - 2017
# data: Automobile price 
#########################################################.

rm(list=ls()) # Delete all objects
ls() # List variables in memory

options(header=T, sep=",", stringsAsFactors = FALSE, 
        str = strOptions(strict.width = "cut"), 
        readr.show_progress=FALSE)

##################################################################.
# 0 Load-packages -------------
#library(GGally) ## Adds pair-wise scatter plots to ggplot2
required_packages <- c("rlang", "ggplot2", "dplyr", "repr", 
                       "hexbin", "GGally")
lapply(required_packages, library, character.only = TRUE)

##################################################################.
# 1 LOAD DATA cARs -----

read.csv("Data_Files//Automobile price data.csv")

# Set the initial plot area dimensions
options(repr.plot.width=4, repr.plot.height=4) 

###########################################################.
# 2 LOAD DATA #### DEFINE FUNCTION FX_READ.AUTO--------
# FUNCTION INCLUDES SOME DATA PRE-PROCESSING TASKS
### start of function =======,
fx_read.auto <- function(file = "Data_Files//Automobile price data.csv"){
  ## LOAD DATA # auto.price only works inside the function
    auto.price <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
    numcols <- c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm')
  for(col in numcols){
    temp <- auto.price[,col]
    # replace question mark for NAs
    auto.price[,col] = ifelse(temp == '?', NA, auto.price[,col])
                        }
  ## Coerce some character columns to numeric
  auto.price[, numcols] <- lapply(auto.price[, numcols], as.numeric)
  
  ## Remove cases or rows with missing values. In this case we keep the 
  ## rows which do not have nas. 
  auto.price = auto.price[complete.cases(auto.price[, numcols]), ]
  
  ## Drop some unneeded columns
  auto.price[,'symboling'] = NULL
  auto.price[,'normalized.losses'] = NULL
  
  return(auto.price)

    }
### end of function ========,

###########################################################.
# 3 APPLY THE FUNCTION --------
auto_prices <- fx_read.auto()  # Convert the function to a data table (Berly's question)
class(auto_prices)

###########################################################.
# 4 EXPLORING THE DATA --------
head(auto_prices)
str(auto_prices)
summary(auto_prices)
colnames(auto_prices)

# * 4.1 Standard Deviation of Numerical columns ----
# Use cat {base} -> Concatenate and Print
for(col in colnames(auto_prices)){
  if(is.numeric(auto_prices[,col])){
    cat(paste(col, as.character(round(sd(auto_prices[,col]), 2)), '\n'))
  }
}

# * 4.2 Distribution of Categorical Variables - Frequency Tables ----
for(col in colnames(auto_prices)){
  if(is.character(auto_prices[, col])){
    cat('\n')
    cat(paste('Frequency table for', col))
    print(table(auto_prices[, col]))
  }    
}

###########################################################.
# 5 VISUALIZATION  ------

# * 5.5 TWO DIMENSIONAL PLOTS --------
# Create scatter plots Feature vs Label (Price)
plot_scatter <- function(df, cols, col_y = 'price'){
  options(repr.plot.width=4, repr.plot.height=3.5) # Set the initial plot area dimensions
  for(col in cols){
    print(paste0("Numerical col: ",col))
    p = ggplot(df, aes_string(col, col_y)) + 
      geom_point() +
      ggtitle(paste('Scatter plot of', col_y, 'vs.', col))
    print(p)
  }
}

numcols = c('curb.weight', 'engine.size', 'horsepower', 'city.mpg')
plot_scatter(auto_prices, numcols)

# It seems likely that horsepower and engine size are collinear.
# To test this hypothesis execute the code
# in the cell below and examine the result.
plot_scatter(auto_prices, c('horsepower'), 'engine.size') 
# Indeed these features do appear linearly dependent.
# Therefore, you will NOT want to use them 
# in the same machine learning model. 

# * 5.6 Using ALPHA for Transparency -----------
# Dealing with Overplotting
plot_scatter_t <- function(df, cols, col_y = 'price', alpha = 1.0){
  options(repr.plot.width=4, repr.plot.height=3.5) # Set the initial plot area dimensions
  for(col in cols){
    print(paste0("Numerical col: ",col))
    p = ggplot(df, aes_string(col, col_y)) + 
      geom_point(alpha = alpha) +
      ggtitle(paste('Scatter plot of', col_y, 'vs.', col))
    print(p)
  }
}

plot_scatter_t(auto_prices, numcols, alpha = 0.2)

# * 5.7 CONTOUR 2-D DENSITY PLOT ======
plot_2density <- function(df, cols, col_y = 'price', alpha = 1.0){
  options(repr.plot.width=4, repr.plot.height=3.5) # Set the initial plot area dimensions
  for(col in cols){
    print(paste0("Numerical col: ",col))
    p = ggplot(df, aes_string(col, col_y)) + 
      geom_density_2d() +
      geom_point(alpha = alpha) +
      ggtitle(paste('2-D density plot of', col_y, 'vs.', col))
    print(p)
  }
}

plot_2density(auto_prices, numcols, alpha = 0.2)

# * 5.8 HEXBIN PLOT ==========
plot_hex = function(df, cols, col_y = 'price', bins = 30){
  options(repr.plot.width=4, repr.plot.height=3.5) # Set the initial plot area dimensions
  for(col in cols){
    print(paste0("Numerical col: ",col))
    p = ggplot(df, aes_string(col, col_y)) + 
      geom_hex(show.legend = TRUE, bins = bins) +
      ggtitle(paste('2-D hexbin plot of', col_y, 'vs.', col))
    print(p)
  }
}

plot_hex(auto_prices, numcols, bins = 10)

