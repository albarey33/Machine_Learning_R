#########################################################.
# DATA PRE-PROCESSING
# * Remove of columns with high percentage of missing values
# * Remove of rows of incomplete cases (containing missing values)
# * Remove of Duplicates
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
# 1 LOAD DATA THAT CONTAINS MISSING VALUES WITH "?" -----

auto_prices <- read.csv("Data_Files//Automobile price data.csv", stringsAsFactors = FALSE, header = TRUE)

head(auto_prices, 12)


##################################################################.
# 2 FIRST OVERVIEW - BOOLEAN CHECK BY COLUMNS IF CONTAINS MISSING VALUES ---- 

lapply(auto_prices, function(x){any(x=="?")})

str(auto_prices) # label price as character, because of the missing values

nrow(auto_prices)

##################################################################.
# 3 COUNT OF MISSING VALUES PER COLUMN  ---- 

for(col in names(auto_prices)){
  if(is.character(auto_prices[,col])){
      count <- sum(ifelse(auto_prices[,col]=="?",1,0))
      print(paste(col,count,sep=": "))
      #print(paste(col, "perc ", count/nrow(auto_prices)*100,"%"))
      #cat(paste(col,as.character(count),'\n'))  
  }
}

##################################################################.
# 4 DECISION HOW TO TREAT INDIVIDUALLY THE INFO WITH MISSING VALUES ----- 

# * 4.1 Delete the fields with high percentage of missing values
dim(auto_prices)
auto_prices[,"normalized.losses"] = NULL    #  DELETE COLUMN

head(auto_prices)

# * 4.2 Delete Incomplete cases ----- 
# With fx complete cases delete cases with missing values
dim(auto_prices)
col <- c("price", "bore", "stroke", "horsepower", "peak.rpm")
auto_prices[,col] <- lapply(auto_prices[,col],function(x){ifelse(x=="?",NA,x)})
auto_prices <- auto_prices[complete.cases(auto_prices[,col]),]

head(auto_prices,12)
dim(auto_prices)

fwrite(auto_prices, )
data.table::fwrite(auto_prices, paste0("Data_Files//auto_prices_wo_missing.csv"))

##################################################################.
# 5 REMOVE DUPLICATE RECORDS ----- 

# * 5.1 Read the data German Credit -----
credit_df <- read.csv("Data_Files//German_Credit.csv", stringsAsFactors = FALSE, header = TRUE)

head(credit_df, 12)

dim(credit_df)  # 1011 records

# * 5.2 Appply function Distinct

dim(distinct(credit_df))   # 999 records

# The difference 12 records are duplicated

# * 5.3 Checking the duplicated records ---------
dup_ids <- credit_df %>% group_by(X1122334) %>% tally() %>% filter(n>1) %>% pull(X1122334)
credit_df %>% filter(X1122334 %in% dup_ids) %>% arrange(X1122334)

# ======== END ========