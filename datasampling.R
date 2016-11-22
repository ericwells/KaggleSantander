# datasampling.R: reduce the size of the train data to something manageable for my laptop.  Before final Kaggle submissions, need to rerun models on full train data and score out full test.
# Update on Nov 19: decided to downsample using Perl first.  Therefore this R script is no longer needed.  Perl code is "perl -n -e 'print if (rand()<.01||$.==1)' < train_ver2.csv > train_ver2_tenpct_samp2.csv"

# Import Data ****************

# Import the training set: train
train_location <- "../train_ver2.csv"
train <- read.csv(train_location, header = TRUE)

# Confirm by printing a few rows to console
str(train)
head(train, 50L)

# end Import Data ****************

# Sample Data ****************

randomRows = function(df,n){
  return(df[sample(nrow(df),n),])
}
train_samp <- randomRows(train,1000)

# end Sample Data ****************

# Write Data ****************

print(paste("Writing this many rows to train_ver2_samp.csv: ", nrow(train_samp)))
write.csv(train_samp, file = "../train_ver2_samp.csv", row.names = FALSE)

# end Write Data ****************