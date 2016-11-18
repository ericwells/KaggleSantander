# explore.R:  Just a place to keep track of my data explorations.

# Libraries **************
#install.packages('rattle')
library(rattle)
#install.packages('rpart.plot')
library("rpart.plot")
#install.packages('RColorBrewer')
library("RColorBrewer")
#install.packages('randomForest')
library("randomForest")
install.packages("Amelia")
library('Amelia') # has mismap which quickly checks for missing values

library("rpart")

# end Libraries **************

# Utilities **************

# my own utility to convert the factor back to its proper numeric value (instead of the label of the level!)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

# create helper function to random sample rows from a dataframe
randomRows = function(df,n){
  return(df[sample(nrow(df),n),])
}

# cool little helper re missing values
install.packages("Amelia")
library('Amelia')

set.seed(111) 

# end Utilities ***************

# Import Data ****************

# Import the training sample
train_samp_location <- "../train_ver2_samp.csv"
train_samp <- read.csv(train_samp_location, header = TRUE)

# end Import Data ****************


# Data Exploration ***************

# Survival rates in absolute numbers
table(train$Survived)

# Survival rates in proportions
prop.table(table(train$Survived))

# Two-way comparison: Sex and Survived
table(train$Sex, train$Survived)

# Two-way comparison: row-wise proportions
prop.table(table(train$Sex,train$Survived), margin = 1)

# Create the column child, and indicate whether child or no child
train$Child <- NA
train$Child[train$Age < 18] <- 1
train$Child[train$Age >= 18] <- 0

# Two-way comparison
prop.table(table(train$Child,train$Survived),1)

missmap(train, main = "Missing values vs observed")

# end Data Exploration ****************



