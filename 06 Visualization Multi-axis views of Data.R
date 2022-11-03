#########################################################.
# DATA EXPLORATORY - VISUALIZATION 
# MULTIPLE AXIS - PLOT OF SUBSETS OF DATA THAT WE CAN COMPARE
# FACETING - METHOD OF SMALL MULTIPLES - CONDITIONING
# PRINCIPLES OF MACHINE LEARNING - 2017
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

#####################################################.
# 7 MULTI-AXIS VIEWS OF DATA  -------------

# * 7.1 Pair-wise scatter plots or scatter plot matrices ===========
numcols = c('curb.weight', 'engine.size', 'horsepower', 'city.mpg', 'price')
options(repr.plot.width=6, repr.plot.height=6) # Set the initial plot area dimensions
ggpairs(auto_prices,
        columns = numcols,
        aes(color = fuel.type, alpha = 0.1),   # High transparency
        lower = list(continuous = 'points'),    # Lower part: Scatter plot
        upper = list(continuous = ggally_density))  # Higher part: Contour plot


# * 7.2 Conditioned Plots (faceted) =============
plot_hist_grid = function(df, numcols, bins = 10){
  options(repr.plot.width=6, repr.plot.height=3) # Set the initial plot area dimensions
  for(col in numcols){
    if(is.numeric(df[,col])){
      bw = (max(df[,col]) - min(df[,col]))/(bins + 1)
      print(paste0("Numerical col: ",col))
      p = ggplot(df, aes_string(col)) + 
        geom_histogram(binwidth = bw, aes(y=..density..), alpha = 0.5) +
        geom_density(aes(y=..density..), color = 'blue') + 
        geom_rug() +
        facet_grid(. ~ drive.wheels)
      print(p)
    }
  }
}

plot_hist_grid(auto_prices, numcols)  

# * 7.3 conditioned scatter plots =============
plot_scatter_grid = function(df, cols, col_y = 'price', alpha = 1.0){
  options(repr.plot.width=7, repr.plot.height=5) # Set the initial plot area dimensions
  for(col in cols){
    print(paste0("Numerical col: ",col))
    p = ggplot(df, aes_string(col, col_y)) + 
      geom_point(aes(color = fuel.type), alpha = alpha) +
      ggtitle(paste('Scatter plot of', col_y, 'vs.', col, 
                    '\n conditioned on drive wheels and body style',
                    '\n with color by fuel type')) +
      facet_grid(drive.wheels ~ body.style)
    print(p)
  }
}

numcols = c('curb.weight', 'engine.size', 'horsepower', 'city.mpg')
plot_scatter_grid(auto_prices, numcols, alpha = 0.2)

# It generates a grid of graphs with the same dimention.

# END
