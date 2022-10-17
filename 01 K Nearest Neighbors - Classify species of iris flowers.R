#######################################################################.
# K NEAREST NEIGHBORS 
# KNN algorithm to classify the species of iris flowers
# The goal is to predict the type or class of the label (classification model)
# Machine Learning in R. Lab 01 
#######################################################################.

# 0 PREPARE INSTALL CALL PACKAGES ----------------------------------

st <- Sys.time()
rm(list=ls()) # Delete all objects
ls() # List variables in memory

options(header=T, sep=",", stringsAsFactors = FALSE, 
        str = strOptions(strict.width = "cut"), 
        readr.show_progress=FALSE)

# Load-multiple-packages-at-once
required_packages <- c("rlang", "ggplot2", "dplyr", "repr", "kknn")
lapply(required_packages, library, character.only = TRUE)

##################################################.
# 1 READ IRIS DATA ---------

data(iris) # Load the iris data set
head(iris) # look at the first few rows of the data frame
str(iris)  # PETALS AND SEPALS - MEASURES IN INCHES

# number of unique categories, and number of cases for each category
table(iris$Species, useNA = "always") # Frequency table of the label # We have balanced cases or balances classes

# for the classification problem

##################################################.
# 2 VISUALIZATION -------
# you will create some plots to see how the classes might, 
# or might not, be well separated by the value of the features
options(repr.plot.width=5, repr.plot.height=4) # Set the initial plot area dimensions

ggplot(iris, aes(Sepal.Width, Sepal.Length)) + 
  geom_point(aes(color = Species))
ggplot(iris, aes(Petal.Width, Sepal.Length)) + 
  geom_point(aes(color = Species))  

##################################################.
# 3 DATA PREPROCESSING - STANDARDIZATION  -----

# These data require only two preparation steps: . 

# * 3.1 Standardization of numerical values of the features. -----
# Before standardization
summary(iris[,c('Sepal.Width', 'Sepal.Length', 'Petal.Width', 'Petal.Length')])

# LAPPLY SCALE 
iris[,c('Sepal.Width', 'Sepal.Length', 
        'Petal.Width', 'Petal.Length')] <-  
  lapply(iris[,c('Sepal.Width', 'Sepal.Length', 
                 'Petal.Width', 'Petal.Length')], scale)
summary(iris)
print(sapply(iris[,c('Sepal.Width', 'Sepal.Length', 
                     'Petal.Width', 'Petal.Length')], mean)) # Means ~ Zero
print(sapply(iris[,c('Sepal.Width', 'Sepal.Length', 
                     'Petal.Width', 'Petal.Length')], sd)) # SD = 1 
# Results: Mean is zero, variance is one

################################################.
# 4 SPLIT TRAIN AND TEST SETS --------
# Split the dataset into randomly sampled training and evaluation data sets.
# Split using Bernoulli sampling

set.seed(2345)
iris$rows <- as.numeric(rownames(iris)) # use as.numeric because rownames() returns character

# Train Set
train.iris <- dplyr::sample_frac(iris, 0.7)     # 70% ~ 105 from 150 rows

# Test Set
# test.iris <- dplyr::setdiff(iris,train.iris) #[-train.iris$rows,]
test.iris <- iris[!iris$rows %in% train.iris$rows,]
train.iris <- select(train.iris,-"rows") # all except col rows
test.iris <- select(test.iris,-"rows")
# test.iris = iris,train.iris) # [-as.numeric(rownames(train.iris)),] 
# test.iris = iris[-as.numeric(rownames(iris)),]
# Resulting frequency tables
table(train.iris$Species)   
table(test.iris$Species)

#####################################################################.
# 5 TRAIN AND EVALUATE THE KNN K=3 MODEL --------

# With some understanding of the relationships between the features and 
# the label and preparation of the data completed you will now train 
# and evaluate a  K=3  model.
  
# * 5.1 Defines the model ---- 
# Tilda ~ means "modeled by"
# Model the label Species by ALL FEATURES ("." (dot))

knn.3 <- kknn::kknn(Species ~ ., 
              train = train.iris, 
              test = test.iris, k=3)
summary(knn.3) 

# 6 PREDICTED RESULTS --------
test.iris$predicted <- predict(knn.3)  # Classifies to the most probable Species
# knn.3$fitted.values same result

# 7 MEASURE ACCURACY ---------- 
# Accuracy = % test cases correctly classified. 
test.iris$correct <- test.iris$Species == test.iris$predicted
round(100 * sum(test.iris$correct) / nrow(test.iris))
# accuracy 91%

# 8 VISUALIZE THE INCORRECT RESULTS ---------
# Correctly classified cases are shown by triangles and 
# incorrectly classified cases are shown by circles.

ggplot(test.iris, aes(Sepal.Width, Sepal.Length)) + 
  geom_point(aes(color = predicted, shape = correct))

ggplot(test.iris, aes(Petal.Width, Sepal.Length)) + 
  geom_point(aes(color = predicted, shape = correct))

# END
