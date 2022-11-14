#########################################################.
# COMPILATION OF MACHINE LEARNING FUNCTIONS 
#########################################################.

###########################################################.
# 1 VISUALIZATION - DISTRIBUTION OF VARIABLES  ----
# One variable graphs, charts, distribution and range

# * 1.1 GEOM_BAR - BAR PLOTS FREQUENCY CHARTS for Categorical features ------
# The bar plot is created using the ggpot2 geom_bar plot type.

# The bar plot is created using the ggpot2 geom_bar plot type =====
geom_bar_fx <- function(df, col){
  # Set the initial plot area dimensions
  options(repr.plot.width=4, repr.plot.height=3.5) 
  print(paste0("Bar Plots: ",col))
  print(table(df[,col]))
  p = ggplot(df, aes_string(col)) + 
    geom_bar(alpha = 0.6) 
  theme(axis.text.x = element_text(angle = 120, hjust = 1))
  print(p)
}

# plot_bars_fx <- function(df){
#   # Set the initial plot area dimensions
#   options(repr.plot.width=4, repr.plot.height=3.5)
#   for(col in colnames(df)){
#     # A filter is applied to find character columns.
#     if(is.character(df[,col])){
#       print(paste0("Bar Plots: ",col))
#       p = ggplot(df, aes_string(col)) +
#         geom_bar(alpha = 0.6)
#       theme(axis.text.x = element_text(angle = 120, hjust = 1))
#       print(p)
#     }
#   }
# }
#plot_bars_fx(df)


##############################################################.
# 2 EVALUATION FUNCTIONS ========
# * 2.1 FUNCTION TO EVALUATE REGRESSION MODEL PERFORMANCE ========
# (v.34)
print_metrics_fx <- function(lin_mod, df, score, label){
  resids = df[,label] - score   # Residuals (error)
  resids2 = resids**2           # square residuals
  N = length(score)
  r2 = as.character(round(summary(lin_mod)$r.squared, 4))
  adj_r2 = as.character(round(summary(lin_mod)$adj.r.squared, 4))
  cat(paste('Mean Square Error      = ', as.character(round(sum(resids2)/N, 4)), '\n'))
  cat(paste('Root Mean Square Error = ', as.character(round(sqrt(sum(resids2)/N), 4)), '\n'))
  cat(paste('Mean Absolute Error    = ', as.character(round(sum(abs(resids))/N, 4)), '\n'))
  cat(paste('Median Absolute Error  = ', as.character(round(median(abs(resids)), 4)), '\n'))
  cat(paste('R^2                    = ', r2, '\n'))
  cat(paste('Adjusted R^2           = ', adj_r2, '\n'))
}

# print_metrics_glm_fx without linmod
print_metrics_glm_fx = function(df, score, label){
  resids = df[,label] - score
  resids2 = resids**2
  N = length(score)
  SSR = sum(resids2)
  SST = sum((mean(df[,label]) - df[,label])**2)
  r2 = as.character(round(1 - SSR/SST, 4))
  cat(paste('Mean Square Error      = ', as.character(round(sum(resids2)/N, 4)), '\n'))
  cat(paste('Root Mean Square Error = ', as.character(round(sqrt(sum(resids2)/N), 4)), '\n'))
  cat(paste('Mean Absolute Error    = ', as.character(round(sum(abs(resids))/N, 4)), '\n'))
  cat(paste('Median Absolute Error  = ', as.character(round(median(abs(resids)), 4)), '\n'))
  cat(paste('R^2                    = ', r2, '\n'))
}




#############################################################.
# * 2.2 HISTOGRAM OF THE RESIDUALS -------------
# of the regression model and plot a kernel density plot (v.34)
hist_resids_fx = function(df, score, label, bins = 10){
  options(repr.plot.width=4, repr.plot.height=3) # Set the initial plot area dimensions
  df$resids = df[,label] - score
  bw = (max(df$resids) - min(df$resids))/(bins + 1)
  ggplot(df, aes(resids)) + 
    geom_histogram(binwidth = bw, aes(y=..density..), alpha = 0.5) +
    geom_density(aes(y=..density..), color = 'blue') +
    xlab('Residual value') + ggtitle('Histogram of residuals')
}

#############################################################.
# * 1.3 QUANTILES - QQ PLOT  -------------
# Quantiles of Residuals vs Quantiles of Standard Normal
resids_qq_fx = function(df, score, label){
  options(repr.plot.width=4, repr.plot.height=3.5) # Set the initial plot area dimensions
  df$resids = df[,label] - score
  ggplot() + 
    geom_qq(data = df, aes(sample = resids)) + 
    ylab('Quantiles of residuals') + xlab('Quantiles of standard Normal') +
    ggtitle('QQ plot of residual values')
}

#############################################################.
# * 1.4 RESIDUALS PLOT  -------------
resid_plot_fx = function(df, score, label){
  df$score = score
  df$resids = df[,label] - score
  ggplot(df, aes(score, resids)) + 
    geom_point() + 
    ggtitle('Residuals vs. Predicted Values') +
    xlab('Predicted values') + ylab('Residuals')
}


