#########################################################.
# SPLITTING THE DATA INTO TRAINING SET AND TEST SET
# SCALING THE NUMERICAL FEATURES
# APPLY FEATURE ENGINEERING TO CATEGORICAL VARIABLE AND NUMERICAL LABEL
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

auto_prices <- read.csv("Data_Files//auto_prices_wo_missing.csv", 
                        stringsAsFactors = FALSE, header = TRUE)

head(auto_prices, 12)
dim(auto_prices)


##################################################################.
# 2 SPLIT THE DATA SET -----
# Create randomly training and test sets - video 24

set.seed(1955)
partition <- caret::createDataPartition(auto_prices[,"fuel.type"], 
                                        times = 1, p = 0.75, list = FALSE)
training <- auto_prices[partition,]  # Create the training sample
test <- auto_prices[-partition,]  # Create the test sample
dim(training)
dim(test)



##################################################################.
# 3 SCALING NUMERICAL VARIABLES -----
# Important Note: Do scaling only AFTER split the data set

# Applied to just 3 numerical columns (DEMO)
num_cols <- c('curb.weight', 'horsepower', 'city.mpg')

# Before change
summary(training[,num_cols])

# generate scaler - scaling method: "center" and "scale". Mean = 0 and standard deviation = 1
preProcValues <- caret::preProcess(training[,num_cols],method=c("center","scale")) 

training[,num_cols] <- predict(preProcValues, training[,num_cols])
test[,num_cols] <-     predict(preProcValues, test[,num_cols])

# After Change
head(training[,num_cols])
summary(training[,num_cols])

##################################################################.
# 4 FEATURE ENGINEERING IDEAS -------
# GOAL: DEVELOP HIGHLY PREDICTIVE FEATURES
# Raw data are often not the best. Some features are repetitive or unnecessary
# Good features allow simple ML algorithms to work well.
# Poor features result in poor performance from even the best ML algorithms.
# STEPS
# * Exploration to understand data relationships
# * Transform features
# * Generate new features from math operations on other features
# * Use visualization to check results
# * Test with ML model
# * Repeat all steps

# TRANSFORM FEATURES
# * More covariate with label

# COMMON TRANSFORMATIONS
# Log, exponential, square root, variance
# 
# INTERACTIONS
# Many interactions in real world. Think about mean, median, radio of features

# PREDICTIVE FEATURES are key to success in ML. Find them by: 
# * Data exploration
# * Testing multiple, lots of ideas.
# * This is a Iterative process: test, fail fast, keep that works.
# * A lot of creativity and human energy goes into feature engineering.

# DEMOS EXAMPLE OF FEATURE ENGINEERING TECHNIQUES:

# 4.1 DEMO: AGGREGATING - REPLACING REPLACE VALUES ----
# fOR categorical variables
# For cases when too many categories, or categories with very few members

table(auto_prices[,"num.of.cylinders"])
#eight   five   four    six  three twelve 
#  4     10    155     24      1      1 

ggplot(data=auto_prices, aes(num.of.cylinders,price)) +
       geom_boxplot()


# METHOD 1 USING LIST
# List similar to dictionary in Python (keys and values)
cylinder_categories <- c('three' = 'three_four', 'four' = 'three_four',
                         'five' = 'five_six', 'six' = 'five_six', 
                         'eight' = 'eight_twelve', 'twelve' = 'eight_twelve')

out = rep('i', length.out = nrow(auto_prices)) 
i = 1
for(x in auto_prices[,'num.of.cylinders']){
  out[i] = cylinder_categories[[x]]  # cylinder_categories[["four"]] -> "three_four"
  i = i+1
}
auto_prices['num.of.cylinders2'] = out

table(auto_prices[,'num.of.cylinders2'])

# METHOD 2 USING VECTOR
out <- c()   # instead of list(), use empty vector
for(x in auto_prices[,'num.of.cylinders']){
  newvalue <- cylinder_categories[[x]]  # cylinder_categories[["four"]] -> "three_four"
  out <- append(out, newvalue)
}
table(out)

# METHOD 2 DPLYR
table(auto_prices[,"num.of.cylinders"])

auto_prices <- auto_prices %>% 
  mutate(num.of.cylinders3 = case_when(num.of.cylinders == 'three' ~ 'three_four',
                                       num.of.cylinders == 'four' ~ 'three_four',
                                       num.of.cylinders == 'five' ~ 'five_six',
                                       num.of.cylinders == 'six' ~ 'five_six',
                                       num.of.cylinders == 'eight' ~ 'eight_twelve',
                                       num.of.cylinders == 'twelve' ~ 'eight_twelve'))
auto_prices[,'num.of.cylinders3']

ggplot(data=auto_prices, aes(num.of.cylinders3,price)) +
  geom_boxplot()

# 4.2 DEMO: TRANSFORMING - APPLY LOG TO LABEL ----
# fOR numeric variables

# * 4.4 COMBINE HISTOGRAMS AND KDES -----
plot_hist <- function(df, col = 'price', bins = 10){
  options(repr.plot.width=4, repr.plot.height=3) # Set the initial plot area dimensions
  bw = (max(df[,col]) - min(df[,col]))/(bins + 1)
  p = ggplot(df, aes_string(col)) + 
        geom_histogram(binwidth = bw, aes(y=..density..), alpha = 0.5) +
        geom_density(aes(y=..density..), color = 'blue') + 
        geom_rug()
   print(p)
   }
plot_hist(auto_prices)  

# Apply Log to change the distribution to normal
auto_prices[,'log_price'] <- log(auto_prices[,'price'])
plot_hist(auto_prices,col = 'log_price')  

# The result is a whole more symmetric

# END 
