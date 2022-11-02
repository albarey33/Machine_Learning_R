#########################################################.
# DATA EXPLORATORY - FOR NUMERICAL FEATURES AND LABELS
# Automobile price data - 
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

# * 5.1 BAR PLOTS - FREQUENCY CHARTS ------

# The bar plot is created using the ggpot2 geom_bar plot type.
plot_bars <- function(df){
  # Set the initial plot area dimensions
  options(repr.plot.width=4, repr.plot.height=3.5) 
  for(col in colnames(df)){
      # A filter is applied to find character columns.
      if(is.character(df[,col])){
        print(paste0("Bar Plots: ",col))
      p = ggplot(df, aes_string(col)) + 
        geom_bar(alpha = 0.6) 
        theme(axis.text.x = element_text(angle = 120, hjust = 1))
      print(p)
    }
  }
}
plot_bars(auto_prices)

# * 5.2 HISTOGRAMS -------
plot_hist <- function(df, numcols, bins = 10){
  options(repr.plot.width=4, repr.plot.height=3) # Set the initial plot area dimensions
  for(col in numcols){
    if(is.numeric(df[,col])){
      print(paste0("Histogram: ",col))
      bw = (max(df[,col]) - min(df[,col]))/(bins + 1)
      p = ggplot(df, aes_string(col)) + 
        geom_histogram(alpha = 0.6, binwidth = bw) 
      print(p)
    }
  }
}

numcols = c('curb.weight', 'engine.size', 'horsepower', 'city.mpg', 'price')
plot_hist(auto_prices, numcols)

# * 5.3 KERNEL DENSITY PLOTS -------
# smoothed version of a histogram
plot_dist <- function(df, numcols){
  options(repr.plot.width=4, repr.plot.height=3) # Set the initial plot area dimensions
  for(col in numcols){
    if(is.numeric(df[,col])){
      print(paste0("Kernel Density plot: ",col))
      p = ggplot(df, aes_string(col)) + 
        geom_density(color = 'blue') +
        geom_rug()
      print(p)
    }
  }
}

plot_dist(auto_prices, numcols)

# * 5.4 COMBINE HISTOGRAMS AND KDES -----
plot_hist_dens <- function(df, numcols, bins = 10){
  options(repr.plot.width=4, repr.plot.height=3) # Set the initial plot area dimensions
  for(col in numcols){
    if(is.numeric(df[,col])){
      print(paste0("Hist + KDE:  plot: ",col))
      bw = (max(df[,col]) - min(df[,col]))/(bins + 1)
      p = ggplot(df, aes_string(col)) + 
        geom_histogram(binwidth = bw, aes(y=..density..), alpha = 0.5) +
        geom_density(aes(y=..density..), color = 'blue') + 
        geom_rug()
      print(p)
    }
  }
}

plot_hist_dens(auto_prices, numcols)  

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

# * 5.9 BOX PLOTS ================================
# Relation between Categorical and Numeric variables
plot_box <- function(df, cols, col_y = 'price'){
  options(repr.plot.width=4, repr.plot.height=3.5) # Set the initial plot area dimensions
  for(col in cols){
    print(paste0("Categorical variable: ",col))
    p = ggplot(df, aes_string(col, col_y)) + 
      geom_boxplot() +
      ggtitle(paste('Box plot of', col, 'vs.', col_y))
    print(p)
  }
}

cat_cols = c('fuel.type', 'aspiration', 'num.of.doors', 'body.style', 
     'drive.wheels', 'engine.location', 'engine.type', 'num.of.cylinders')
plot_box(auto_prices, cat_cols)  

# * 5.10 Violin Plots ================
plot_violin = function(df, cols, col_y = 'price', bins = 30){
  options(repr.plot.width=4, repr.plot.height=3.5) # Set the initial plot area dimensions
  for(col in cols){
    print(paste0("Categorical variable: ",col))
    p = ggplot(df, aes_string(col, col_y)) + 
      geom_violin() +
      ggtitle(paste('Violin plot of', col, 'vs.', col_y))
    print(p)
  }
}

plot_violin(auto_prices, cat_cols)  

#####################################################.
# 6 AESTHETICS ======================================
# Use aesthetics to add project additional dimensions

# * 6.1 Marker Shape =============
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

plot_scatter_sp(auto_prices, numcols, alpha = 0.2)

# * 6.2 Marker Size ================
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

#####################################################.
# 7 MULTI-AXIS VIEWS OF DATA  -------------

# * 7.1 Pair-wise scatter plots or scatter plot matrices ===========
numcols = c('curb.weight', 'engine.size', 'horsepower', 'city.mpg', 'price')
options(repr.plot.width=6, repr.plot.height=6) # Set the initial plot area dimensions
ggpairs(auto_prices,
        columns = numcols,
        aes(color = fuel.type, alpha = 0.1),
        lower = list(continuous = 'points'),
        upper = list(continuous = ggally_density))


# * 7.2 Conditioned Plots =============
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

