# This is a test script for writing a user defined function to sort the recommendation order.

ID <- c(7,10,13,18)
M16P1 <- c(1,0,0,1)
M16P2 <- c(0,1,1,1)
M16P3 <- c(0,1,0,0)
M17Pr1 <- c(.95,.90,.85,.85)
M17Pr2 <- c(.90,.95,.90,.95)
M17Pr3 <- c(.85,.85,.95,.90)

df <- data.frame(ID=ID, M16P1=M16P1, M16P2=M16P2, M16P3=M16P3, M17Pr1=M17Pr1, M17Pr2=M17Pr2, M17Pr3=M17Pr3)
df

# First overwrite Probabilities to zero if the customer already has the product the previous month
df$M17Pr1 <- (1-df$M16P1)*df$M17Pr1
df$M17Pr2 <- (1-df$M16P2)*df$M17Pr2
df$M17Pr3 <- (1-df$M16P3)*df$M17Pr3

df

# Define function to sort recommendations
sort.recommendations <- function(passed_vector) {
  # takes in a row of data for a single customer (of length 3!), creates a temp array, sorts the array, returns the array.
  x <- data.frame(Product = c("P1","P2","P3"), Probs = passed_vector, stringsAsFactors=FALSE)
  y <- x[order(-x$Probs),1]
  w <- paste(y[1],y[2],y[3])
  return(w)
}

# Define function to return ordered recommendations from a scored test file
get.ordered.recommendations <- function(df) {
  output <- vector(mode="character", length = nrow(df))
  for (i in 1:nrow(df)){
    temp <- c(df[i,5], df[i,6], df[i,7])
    output[i] <- sort.recommendations(temp)
  }
  return(output)
}


df$added_products <- get.ordered.recommendations(df)
df
