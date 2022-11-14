#########################################################.
# An Introduction to glmnet
# Trevor Hastie Junyang Qian Kenneth Tay
# April 13, 2022
# GLMNET.PDF
#########################################################.

# Installation ================
#install.packages("glmnet", repos = "https://cran.us.r-project.org")

#install.packages('Rcpp')

# Quick Start ==================
library(Rcpp)
library(glmnet)

# Guassian linear model or “least squares” - L2 REGULARIZATION =======

data(QuickStartExample)
summary(QuickStartExample)
x <- QuickStartExample$x
y <- QuickStartExample$y

fit <- glmnet(x, y)

plot(fit)

