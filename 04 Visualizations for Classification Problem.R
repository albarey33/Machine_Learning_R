#########################################################.
# DATA EXPLORATORY
# CLASSIFICATION PROBLEM: CREDIT BAD OR GOOD
# VISUALIZATION RELATIONSHIP BETWEEN VARIABLES AND CATEGORICAL LABEL
# INTRODUCTION TO MACHINE LEARNING - 2017
# data: German Credit - Classification Problem
#########################################################.

rm(list=ls()) # Delete all objects
ls() # List variables in memory

##################################################################.
# 0 Load-packages -------------
#library(GGally) ## Adds pair-wise scatter plots to ggplot2
required_packages <- c("ggplot2", "repr" )     # "rlang", "dplyr", 
                                               #"hexbin", "GGally")
lapply(required_packages, library, character.only = TRUE)

# Set the initial plot area dimensions
#options(repr.plot.width=4, repr.plot.height=4) 
options(header=T, sep=",", stringsAsFactors = FALSE, 
        str = strOptions(strict.width = "cut"), 
        readr.show_progress=FALSE, 
        repr.plot.width=4, repr.plot.height=4) # Set the initial plot area dimensions

#################################################################.
# 1 LOAD DATA GERMAN CREDIT -----

credit_df <- read.csv("Data_Files//German_Credit.csv", header = FALSE)

names(credit_df) <- c('Customer_ID','checking_account_status', 'loan_duration_mo', 'credit_history', 
                  'purpose', 'loan_amount', 'savings_account_balance', 
                  'time_employed_yrs', 'payment_pcnt_income','gender_status', 
                  'other_signators', 'time_in_residence', 'property', 'age_yrs',
                  'other_credit_outstanding', 'home_ownership', 'number_loans', 
                  'job_category', 'dependents', 'telephone', 'foreign_worker', 
                  'bad_credit')

head(credit_df)           # Only codes

# Examine classes and class imbalance
table(credit_df$bad_credit)

###########################################################.
# 2 EXPLORING THE DATA --------
head(credit_df)
str(credit_df)
summary(credit_df)
colnames(credit_df)

##########################################################.
# 3 PROCESS FACTORS LABELS NAMES ----------

########## Checking account status
checking_account_status = c('< 0 DM', '0 - 200 DM', '> 200 DM or salary assignment', 'none')
names(checking_account_status) = c('A11', 'A12', 'A13', 'A14')

########## credit history
credit_history = c('no credit - paid', 'all loans at bank paid', 'current loans paid', 
                   'past payment delays',  'critical account - other non-bank loans')
names(credit_history) = c('A30', 'A31', 'A32', 'A33', 'A34')

########## purpose
purpose = c( 'car (new)', 'car (used)', 'furniture/equipment', 'radio/television', 
             'domestic appliances', 'repairs', 'education', 'vacation', 'retraining',
             'business', 'other')
names(purpose) = c('A40', 'A41', 'A42', 'A43', 'A44', 'A45', 'A46', 'A47', 'A48', 'A49', 'A410')

########## savings account balance
savings_account_balance = c('< 100 DM', '100 - 500 DM', '500 - 1000 DM', '>= 1000 DM', 'unknown/none')
names(savings_account_balance) = c('A61', 'A62', 'A63', 'A64', 'A65')

########## time employed years
time_employed_yrs = c('unemployed', '< 1 year', '1 - 4 years', '4 - 7 years', '>= 7 years')
names(time_employed_yrs) = c('A71', 'A72', 'A73', 'A74', 'A75')

########## gender_status
gender_status = c('male-divorced/separated', 'female-divorced/separated/married',
                  'male-single', 'male-married/widowed', 'female-single')
names(gender_status) = c('A91', 'A92', 'A93', 'A94', 'A95')

########## other signators
other_signators = c('none', 'co-applicant', 'guarantor')
names(other_signators) = c('A101', 'A102', 'A103')

########## property
property =  c('real estate', 'building society savings/life insurance', 'car or other', 'unknown-none')
names(property) = c('A121', 'A122', 'A123', 'A124')

########## credit outstanding
other_credit_outstanding = c('bank', 'stores', 'none')
names(other_credit_outstanding) = c('A141', 'A142', 'A143')

########## home ownership
home_ownership = c('rent', 'own', 'for free')
names(home_ownership) = c('A151', 'A152', 'A153')

########## job category
job_category = c('unemployed-unskilled-non-resident', 'unskilled-resident', 'skilled', 'highly skilled')
names(job_category) =c('A171', 'A172', 'A173', 'A174')

########## telephone
telephone = c('none', 'yes')
names(telephone) = c('A191', 'A192')

########## foreign worker
foreign_worker = c('yes', 'no')
names(foreign_worker) = c('A201', 'A202')

########## bad credit
bad_credit = c(1, 0)
names(bad_credit) = c(2, 1)

####################################################

codes = c('checking_account_status' = checking_account_status,
          'credit_history' = credit_history,
          'purpose' = purpose,
          'savings_account_balance' = savings_account_balance,
          'time_employed_yrs' = time_employed_yrs,
          'gender_status' = gender_status,
          'other_signators' = other_signators,
          'property' = property,
          'other_credit_outstanding' = other_credit_outstanding,
          'home_ownership' = home_ownership,
          'job_category' = job_category,
          'telephone' = telephone,
          'foreign_worker' = foreign_worker,
          'bad_credit' = bad_credit)         

cat_cols = c('checking_account_status', 'credit_history', 'purpose', 'savings_account_balance', 
             'time_employed_yrs','gender_status', 'other_signators', 'property',
             'other_credit_outstanding', 'home_ownership', 'job_category', 'telephone', 
             'foreign_worker','bad_credit')

for(col in cat_cols){
 credit_df[,col] = sapply(credit_df[,col], function(code){codes[[paste(col, '.', code, sep = '')]]})
}

#credit$bad_credit = as.numeric(credit$bad_credit)
head(credit_df)

###########################################################.
# 4 VISUALIZATION  Classification Categorical Label--------
# * 4.1 Numerical variables vs Label - BoxPlots   -----
# Visualize class separation by numeric features  - Video 14

plot_box_fx = function(df, cols, col_x = 'bad_credit'){
  options(repr.plot.width=4, repr.plot.height=3.5) # Set the initial plot area dimensions
  for(col in cols){
    p = ggplot(df, aes_string(col_x, col)) + 
      geom_boxplot() +
      ggtitle(paste('Box plot of', col, '\n vs.', col_x))
    print(p)
  }
}

num_cols = c('loan_duration_mo', 'loan_amount', 'payment_pcnt_income',    # 
             'age_yrs', 'number_loans', 'dependents')
plot_box_fx(credit_df, num_cols)  


#################################################################.
# * 4.2 Numerical variables vs Label - Violin Plots  -----
# Alternative to box plots, violin plots - Video 14 4'
# Visualize class separation by numeric features

plot_violin_fx = function(df, cols, col_x = 'bad_credit'){
  options(repr.plot.width=4, repr.plot.height=3.5) # Set the initial plot area dimensions
  for(col in cols){
    print(col)
    p = ggplot(df, aes_string(col_x, col)) + 
      geom_violin() +
      ggtitle(paste('Box plot of', col, '\n vs.', col_x))
    print(p)
  }
}

plot_violin_fx(credit_df, num_cols)  

############################################################################################
# * 4.3 Visualizing class separation by categorical features -----
# Side by side bar charts  - Video 14 5' 30"

library(gridExtra)
plot_bars_fx = function(df, catcols){
  options(repr.plot.width=6, repr.plot.height=5) # Set the initial plot area dimensions
  temp0 = df[df$bad_credit == 0,]               # temporal table
  temp1 = df[df$bad_credit == 1,]               # temporal table
  for(col in cat_cols){
    print(col)
    p1 = ggplot(temp0, aes_string(col)) + 
      geom_bar() +
      ggtitle(paste('Bar plot of \n', col, '\n for good credit')) +  
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    p2 = ggplot(temp1, aes_string(col)) + 
      geom_bar() +
      ggtitle(paste('Bar plot of \n', col, '\n for bad credit')) +  
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    gridExtra::grid.arrange(p1,p2, nrow = 1)
  }
}

plot_bars_fx(credit_df, cat_cols)     # Check for proportions





############################################################.
# 5 EXAMPLE DUMMY VARIABLES ===============================

# * 5.1 One categorical feature =====

table(credit_df$checking_account_status)
dummies <- dummyVars(bad_credit ~ checking_account_status, data = credit_df)
credit_dummies <- data.frame(predict(dummies, newdata = credit_df))
str(credit_dummies)

# * 5.2 All categorical feature ======

str(credit_df)
dummies <- dummyVars(bad_credit ~ ., data = credit_df)
credit_dummies <- data.frame(predict(dummies, newdata = credit_df))
str(credit_dummies)

# The numeric columns are left alone, so they are not affected

######### END ===========================