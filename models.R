# models.R: This contains the code for the modeling activities.  Code for data sampling and data exploration is elsewhere. Models results are documented in models.hist.txt

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

# a function to calculate the performance metric
calc.accuracy <- function(myprediction, the_answers) {
  mistakes <- abs(myprediction-the_answers)
  accuracy <- (length(myprediction)-sum(mistakes))/length(myprediction)
  return(accuracy)
}

# create helper function to random sample rows from a dataframe
randomRows = function(df,n){
  return(df[sample(nrow(df),n),])
}

# need a model evaluation function
model.perf <- function(x){
  return(x)
}

# need a function that creates the Kaggle submission CSV
output_for_kaggle <- function(my_model, mtype, outputname){
  # my_model is the actual model object.  mtype is the char string that needs to go in the predict type param. outputname eg "my_solution.csv".
  my_testprediction <- predict(my_model, test, type = mtype)
  my_solution <- data.frame(PassengerId = test$PassengerId, Survived = as.integer(round(my_testprediction)))
  write.csv(my_solution, file = outputname, row.names = FALSE) 
}

set.seed(111) 

# end Utilities ***************

# Import Data ****************

# Import the training set: train
train_samp_location <- "../train_ver2_samp.csv"
train_samp <- read.csv(train_samp_location, header = TRUE)
# Import the testing set: test
test_location <- "../test_ver2.csv"
test <- read.csv(test_location, header = TRUE)

# Combine into all_data
test$Survived <- NA
all_data <- rbind(train, test)
rm(train_samp,test)

# Confirm by printing a few rows to console
str(all_data)
head(all_data, 10L)

# end Import Data ****************

# Data Cleansing ******************

all_data$Embarked[c(62,830)] = "S"
all_data$Embarked <- factor(all_data$Embarked)

all_data$Fare[1044] <- median(all_data$Fare, na.rm = TRUE)

# Deal with missing Ages
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size, data=all_data[!is.na(all_data$Age),], method="anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])


# end Data Cleansing ******************

# Variable Generation ***************

# Create the column child, and indicate whether child or no child
train$Child <- NA
train$Child[train$Age < 18] <- 1
train$Child[train$Age >= 18] <- 0

# Create Title variable by stripping from Name field
all_data$Name <- as.character(all_data$Name)
all_data$Title <- sapply(all_data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
all_data$Title <- sub(' ', '', all_data$Title)
all_data$Title[all_data$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
all_data$Title[all_data$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
all_data$Title[all_data$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
all_data$Title <- factor(all_data$Title)

# Create Deck variable from Cabin
# Cabin has a lot of missings. Let's set those to S for Steerage.
all_data$Cabin <- as.character(all_data$Cabin)
all_data$Cabin[all_data$Cabin==""] <- "S"
all_data$Cabin[is.na(all_data$Cabin)] <- "S"
# Cabins seem to have a leading value of either "A" "B" "C" "D" "E" "F" "G" "S" (that's the one I added), or "T".  Let's strip that out into a new variable.  I'm not sure if that is really corresponding to the deck of the ship, but I'll call it Deck anyway.
all_data$Deck <- sapply(all_data$Cabin, FUN=function(x) {strsplit(x, split="")[[1]][1]})
all_data$Deck <- factor(all_data$Deck)

# Create Family Size variable by adding siblings and parent/child
all_data$family_size <- all_data$SibSp + all_data$Parch +1

# end Variable Generation ***************

# Model and Model Eval *****************

# cool little helper re missing values
missmap(train, main = "Missing values vs observed")

# Split all_data back into Train and Test and split Train into train_tr and train_ho.
train <- all_data[1:891,]
test <- all_data[892:1309,]
train_ho <- XXX  # probably can do this with rattle or dplyr
train_tr <- XXX

# Build a decision tree
my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class", control = rpart.control(minsplit = 50, cp = 0))
# Visualize the decision tree using plot() and text()
plot(my_tree_two)
text(my_tree_two)
fancyRpartPlot(my_tree_two)

# build a random forest
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size + Title, data=train, importance = TRUE, ntree=1000) 
varImpPlot(my_forest)

# Build a logistic regression
my_logit_one <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size + Deck + Title, data=train, family = "binomial")
summary(my_logit_one)
my_scoredtrain <- data.frame(Survived = train$Survived, Score = my_trainprediction)
my_scoredtrain$Score <- round(my_scoredtrain$Score)

# end Model and Model Eval ***************

