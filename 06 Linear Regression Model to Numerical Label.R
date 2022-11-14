#########################################################.
# LINEAR REGRESSION PROBLEM - CALCULATE CAR PRICE
# SPLITTING THE DATA INTO TRAINING SET AND TEST SET
# SCALING THE NUMERICAL FEATURES
# APPLY FEATURE ENGINEERING TO VARIABLES AND NUMERICAL LABEL
# APPLY DUMMY VARIABLES
#########################################################.

rm(list=ls()) # Delete all objects
ls() # List variables in memory

source("ML_Functions.R")

options(header=T, sep=",", stringsAsFactors = FALSE, 
        str = strOptions(strict.width = "cut"), 
        readr.show_progress=FALSE)

##################################################################.
# 0 Load-packages -------------
#library(GGally) ## Adds pair-wise scatter plots to ggplot2

#library("Rcpp")
required_packages <- c("rlang", "ggplot2", "dplyr", "repr", "caret", "glmnet",
                       "Rcpp", "hexbin", "GGally")
lapply(required_packages, library, character.only = TRUE)


##################################################################.
# 2 LOAD DATA WITHOUT MISSING VALUES WITH "?" -----
# After data pre-processing: eliminate duplicates, unneeded columns, missing values
auto_prices <- read.csv("Data_Files//auto_prices_wo_missing.csv", 
                        stringsAsFactors = TRUE, header = TRUE)

head(auto_prices, 12)
str(auto_prices, 12)
dim(auto_prices)


# Plot_bars - Frequency Claims

for(col in colnames(auto_prices)){
  if(is.factor(auto_prices[,col])){
    geom_bar_fx(auto_prices, col) } }

##################################################################.
# 3 FEATURE ENGINEERING IDEAS / TECHNIQUES -------

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

# * 3.1 AGGREGATING - DEMO: REPLACING REPLACE VALUES ----
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
auto_prices['num.of.cylinders.grouped'] = out #comment out

table(auto_prices[,'num.of.cylinders.grouped'])

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
  mutate(num.of.cylinders.grouped = 
           case_when(num.of.cylinders == 'three' ~ 'three_four',
                     num.of.cylinders == 'four' ~ 'three_four',
                     num.of.cylinders == 'five' ~ 'five_six',
                     num.of.cylinders == 'six' ~ 'five_six',
                     num.of.cylinders == 'eight' ~ 'eight_twelve',
                     num.of.cylinders == 'twelve' ~ 'eight_twelve'))
table(auto_prices[,'num.of.cylinders.grouped'])
ggplot(data=auto_prices, aes(num.of.cylinders.grouped,price)) +
  geom_boxplot()

str(auto_prices)
ggplot(data=auto_prices, aes(engine.type, price)) +
  geom_boxplot()

# ADDED FOR GLMNET
auto_prices[,'num.of.cylinders'] <- NULL   # ALREADY AGGREGATED
#auto_prices[,'num.of.cylinders.grouped'] <- NULL  # By some reason this column is eliminated in edx
#"engine.size", "fuel.system", "bore", "stroke", "compression.ratio",
#"horsepower", "peak.rpm", "city.mpg", "highway.mpg",
#auto_prices[,'fuel.system'] <- NULL
#auto_prices[,'engine.location'] <- NULL
# auto_prices[,'engine.type'] <- NULL
#auto_prices[,'num.of.doors'] <- NULL
#auto_prices[,'make'] <- NULL

str(auto_prices)

# * 3.2 TRANSFORMING - DEMO: APPLY LOG TO LABEL ----
# FOR numeric variables
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

#training[,'log_price'] <- log(training[,'price'])
# The result is a whole more symmetric
# str(training)

str(auto_prices)

##################################################################.
# 3 SPLIT THE DATA SET -----
# Create randomly training and test sets - video 24

set.seed(1955)
partition <- caret::createDataPartition(auto_prices[,"fuel.type"], 
                                        times = 1, p = 0.75, list = FALSE)
training <- auto_prices[partition,]  # Create the training sample
test <- auto_prices[-partition,]  # Create the test sample
dim(training)
dim(test)

##################################################################.
# 4 SCALING NUMERICAL VARIABLES -----
# Important Note: Do scaling only AFTER split the data set

str(auto_prices)

# Applied to just 3 numerical columns (DEMO)
num_cols <- c('wheel.base', 'length', 'width', 'height', 
              'curb.weight', 'engine.size', 'bore', 'stroke', 
              'compression.ratio', 'horsepower', 'peak.rpm', 
              'city.mpg', 'highway.mpg')

# Before change
summary(training[,num_cols])

# generate scaler - scaling method: "center" and "scale". Mean = 0 and standard deviation = 1
preProcValues <- caret::preProcess(training[,num_cols],method=c("center","scale")) 

training[,num_cols] <- predict(preProcValues, training[,num_cols])
    test[,num_cols] <- predict(preProcValues,     test[,num_cols])

# After Change
head(training[,num_cols])
summary(training)
str(training)
# Comparison data three groups
# for(col in colnames(auto_prices)){
#   if(is.factor(auto_prices[,col])){
#     geom_bar_fx(auto_prices, col) } }
# for(col in colnames(training)){
#   if(is.factor(training[,col])){
#     geom_bar_fx(training, col) } }
# for(col in colnames(test)){
#   if(is.factor(test[,col])){
#     geom_bar_fx(test, col) } }

######################################################################.
# 5 LINEAR REGRESSION MODEL AND EVALUATION =========================

# lin_mod <- lm(data = training, log_price ~ curb.weight + horsepower + city.mpg +
#                 fuel.type + aspiration + 
#                 body.style + drive.wheels + num.of.cylinders )

lin_mod = lm(data = training, log_price ~ fuel.type + aspiration + 
               make + 
               num.of.doors + 
               body.style +
               drive.wheels + engine.location + 
               # engine.type +    # Critical problem
               wheel.base + length +
               width + height + curb.weight + 
               num.of.cylinders.grouped +
               engine.size + # fuel.system + 
               bore + stroke + compression.ratio +
               horsepower + peak.rpm + city.mpg + highway.mpg)

#summary(lin_mod) #$adj.r.squared
# Check 

#summary(lin_mod)$adj.r.squared
score = predict(lin_mod, newdata = test) # warning because engine.locationrear = NA

print_metrics_fx(lin_mod, test, score, label = 'log_price')

hist_resids_fx(test, score, label = 'log_price')   
  resids_qq_fx(test, score, label = 'log_price')
 resid_plot_fx(test, score, label = 'log_price')

# The resid_plot is for the log_price. The next is for the real prices.
score_untransform = exp(score)
resid_plot_fx(test, score_untransform, label = 'price')

# Conclusion: The simple linear model is not the ideal for predicting the price
# of low cost automoviles

######################################################################.
# 6 REGULARIZATION - GENERALIZE LINEAR MODEL (GMLNET) ==============
# * 6.1 Create matrix of Dummy variables - Model Matrix  =============
# Create Model Matrix
str(auto_prices)

cols <- c(
          "make",
          "fuel.type","aspiration",
          "num.of.doors",
          "body.style",
          "drive.wheels","wheel.base","length", "engine.location",
          "width", "height", "curb.weight", 
          #"num.of.cylinders",  
          "engine.type", 
          "engine.size", 
          "fuel.system", 
          "bore", "stroke", "compression.ratio",
          "horsepower", "peak.rpm", "city.mpg", "highway.mpg",
          "num.of.cylinders.grouped", 
          "log_price")   # "price"

# Create the matrix "training_dummies"
dummies <- caret::dummyVars(log_price ~ ., data = auto_prices[,cols])

training_dummies = predict(dummies, newdata = training[,cols])
dim(training_dummies)
class(training_dummies)
print(dim(training_dummies))  # 71 columns
head(training_dummies)

test_dummies = predict(dummies, newdata = test[,cols])
dim(test_dummies)
head(test_dummies)

names(data.frame(training_dummies))
names(data.frame(test_dummies))

######################################################################.
# * 6.2 APPLY L2 (SOFT) REGULARIZATION ========
# Use of GLM (Generalize Linear Model)

options(repr.plot.width=6, repr.plot.height=6) # Set the initial plot area dimensions

# input of glmnet training_dummies as matrix
# alpha = 0 means pure L2 Regularization
# This is a regression problem so the residuals should be 'gaussian' or normal
glmnet_mod_l2 = glmnet(x = training_dummies, y = training[,'log_price'], 
                       nlambda = 20, alpha = 0, family = 'gaussian')

plot(glmnet_mod_l2, xlab = 'Inverse of Regulariation')
# Inverse because regularization increases to the left

# * 6.3 Compute Cross Validation of the Model ========
# cross validation of the model with different regularization hyper-parameters
# Look at the standard errors of the fit
# Bias-variance trade-off: 
# * the bias is increasing to the right, 
# * the variance is increasing to the left.
# Cross Validation method: Trains the model multiple times
cv_fit = cv.glmnet(x = training_dummies, y = training[,'log_price'], 
                   nlambda = 20, alpha = 0, family = 'gaussian')
plot(cv_fit)


# REPEAT L2 With 16 value

# Checking the cv_fit plot, taking the 16 value (from right to left)
# Check vertical dotted line
score = predict(glmnet_mod_l2, newx = test_dummies)[,16]

print_metrics_glm_fx(test, score, 'log_price')
hist_resids_fx(test, score, label = 'log_price')   
resids_qq_fx(test, score, label = 'log_price')
resid_plot_fx(test, score, label = 'log_price')

# The error metrics for the regularized model are somewhat better
############################################################

# APPLY L1 REGULARIZATION

options(repr.plot.width=6, repr.plot.height=6) # Set the initial plot area dimensions

glmnet_mod_l1 = glmnet(x = training_dummies, y = training[,'log_price'], 
                       nlambda = 20, alpha = 1, family = 'gaussian')
plot(glmnet_mod_l1, xlab = 'Inverse of regulariation')

cv_fit = cv.glmnet(x = training_dummies, y = training[,'log_price'], 
                   nlambda = 20, alpha = 1, family = 'gaussian')
plot(cv_fit)

# EVALUATE WITH BEST L1

score = predict(glmnet_mod_l1, newx = test_dummies)[,13]
print_metrics_glm(test, score, 'log_price')
hist_resids(test, score, label = 'log_price')   
resids_qq(test, score, label = 'log_price')
resid_plot(test, score, label = 'log_price')

#######################################################################



######### END DELETE DELETE 

lin_mod = lm(data = training, log_price ~ make + fuel.type + aspiration + 
               num.of.doors + body.style +
               drive.wheels + engine.location + wheel.base + length +
               width + height + curb.weight + num.of.cylinders.grouped +
               engine.size + bore + stroke + compression.ratio +
               horsepower + peak.rpm + city.mpg)

#summary(lin_mod) #$adj.r.squared
#summary(lin_mod)$adj.r.squared
score = predict(lin_mod, newdata = test) # warning because engine.locationrear = NA

print_metrics(lin_mod, test, score, label = 'log_price')

hist_resids(test, score, label = 'log_price')   
resids_qq(test, score, label = 'log_price')
resid_plot(test, score, label = 'log_price')

# The resid_plot is for the log_price. The next is for the real prices.
score_untransform = exp(score)
resid_plot(test, score_untransform, label = 'price')

# Conclusion: The simple linear model is not the ideal for predicting the price
# of low cost automoviles



# END 


#summary(lin_mod)$coefficients
#print_metrics(lin_mod, test, score)     
#hist_resids(test, score)
# 
