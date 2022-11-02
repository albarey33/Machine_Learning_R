#########################################################.
# DATA EXPLORATORY
# VISUALIZATION 
# AESTHETICS FOR HIGHER DIMENSIONALITY: MARKER SHAPE, SIZE, COLOR
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
# 6 AESTHETICS ======================================
# Use aesthetics to add project additional dimensions

# * 6.1 Marker Shape =============
# Works well for categorical variables
plot_scatter_sp <- function(df, cols, col_y = 'price', alpha = 1.0){
  options(repr.plot.width=5, repr.plot.height=3.5) 
  for(col in cols){
    print(paste0("Categorical variable: ",col))
    p = ggplot(df, aes_string(col, col_y)) + 
      geom_point(aes(shape = factor(fuel.type)), alpha = alpha) +
      ggtitle(paste('Scatter plot of', col_y, 'vs.', col, '\n with shape by fuel type'))
    print(p)
  }
}

numcols = c('curb.weight', 'engine.size', 'horsepower', 'city.mpg')#, 'price')
plot_scatter_sp(auto_prices, numcols, alpha = 0.2)

# * 6.2 Marker Size ================
# Works with numerical or categorical ordinal variables
plot_scatter_sp_sz = function(df, cols, col_y = 'price', alpha = 1.0){
  options(repr.plot.width=5, repr.plot.height=3.5)
  df$curb.weight.2 = df$curb.weight**2
  for(col in cols){
    print(paste0("Categorical variable: ",col))
    p = ggplot(df, aes_string(col, col_y)) + 
      geom_point(aes(shape = factor(fuel.type), size = curb.weight.2), alpha = alpha) +
      ggtitle(paste('Scatter plot of', col_y, 'vs.', col, '\n with shape by fuel type'))
    print(p)
  }
}

plot_scatter_sp_sz(auto_prices, numcols, alpha = 0.1)
  
# * 6.3 Color in Scatter Plots ================
# Works with categorical variables
plot_scatter_sp_sz_cl = function(df, cols, col_y = 'price', alpha = 1.0){
  options(repr.plot.width=5, repr.plot.height=3.5) # Set the initial plot area dimensions
  df$curb.weight.2 = df$curb.weight**2
  for(col in cols){
    print(paste0("Categorical variable: ",col))
    p = ggplot(df, aes_string(col, col_y)) + 
      geom_point(aes(shape = factor(fuel.type), size = curb.weight.2, color = aspiration), 
                 alpha = alpha) +
      ggtitle(paste('Scatter plot of', col_y, 'vs.', col, 
                    '\n with shape by fuel type',
                    '\n and color by aspiration'))
    print(p)
  }
}

plot_scatter_sp_sz_cl(auto_prices, numcols, alpha = 0.3)

# * 6.4 Color in Violin Plots ================
# violin plots with color set by aspiration type. 

cat_cols = c('fuel.type', 'aspiration', 'num.of.doors', 'body.style', 
             'drive.wheels', 'engine.location', 'engine.type', 'num.of.cylinders')

plot_violin <- function(df, cols, col_y = 'price', bins = 30){
  options(repr.plot.width=5, repr.plot.height=3.5) # Set the initial plot area dimensions
  for(col in cols){
      print(paste0("Categorical variable: ",col))
      p = ggplot(df, aes_string(col, col_y)) + 
      geom_violin(aes(fill = factor(aspiration))) +
      ggtitle(paste('Violin plot of', col, 'vs.', col_y, 
                    '\n with fill by aspiration'))
    print(p)
  }
}

plot_violin(auto_prices, cat_cols) 
 
