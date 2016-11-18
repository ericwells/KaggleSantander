# datasampling.R: reduce the size of the train data to something manageable for my laptop.  Before final Kaggle submissions, need to rerun models on full train data and score out full test.

# Import Data ****************

# Import the training set: train
train_location <- "../train_ver2.csv"
train <- read.csv(train_location, header = TRUE)

# Confirm by printing a few rows to console
str(train)
head(train, 50L)

# end Import Data ****************

# Sample Data ****************

train_samp <- XXX

# end Sample Data ****************

# Write Data ****************

print(paste("Writing this many rows to train_ver2_samp.csv: ", nrow(my_solution)))
write.csv(train_samp, file = "train_ver2_samp.csv", row.names = FALSE)

# end Write Data ****************