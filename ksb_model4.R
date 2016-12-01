# ksb_model4.R

setwd("/Users/ericwells/Documents/KaggleSantander/myCodeForKaggleSantander")

# This script starts from the Month18and17SortedNoHeader.csv that command_line_tools.txt created.

# Read in Month18and17SortedNoHeader.csv
col_names = c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31","C32","C33","C34","C35","C36","C37","C38","C39","C40","C41","C42","C43","C44","C45","C46","C47","C48","C49","C50","C51","C52","C53","C54","C55","C56","C57","C58","C59","C60","C61","C62","C63","C64","C65","C66","C67","C68","C69","C70","C71")
# test version
#location <- "../Joins/M18and17SortedNoHeader_head200.csv"
# full version
location <- "../Joins/M18and17SortedNoHeader.csv"
input <- read.csv(location, stringsAsFactors=FALSE, header = FALSE, col.names = col_names)

# Convert date_open into a year only
input$year_open <- as.Date(input$C30, format = "%Y-%m-%d")

# Create segment_id
input$segment_id <- 
  ifelse( input$year_open < as.Date("2000", format = "%Y"), 1, 
  ifelse( input$year_open >= as.Date("2010", format = "%Y"), 3, 
          2 ))
table(input$segment_id)

# factorize segment_id
input$segment_id <- as.factor(input$segment_id)

# Generate the distributions
#library(dplyr)
x <- input %>% 
  group_by(segment_id) %>% 
  summarise_at(.cols = vars(c(C48:C71)), .funs = (Sum="sum"))
library(lsr)
w <- tFrame(x)

#write.csv(tFrame(x), file = "../Joins/M18_17segmentcounts.csv", row.names = TRUE, quote = FALSE) 

# Create the new columns that contain the Product Probabilities by segment (if they are not already on the input dataset...).

input$M18Pr1[input$segment_id == 1] <- as.numeric(x[1,"C48"])
input$M18Pr2[input$segment_id == 1] <- as.numeric(x[1,"C49"])
input$M18Pr3[input$segment_id == 1] <- as.numeric(x[1,"C50"])
input$M18Pr4[input$segment_id == 1] <- as.numeric(x[1,"C51"])
input$M18Pr5[input$segment_id == 1] <- as.numeric(x[1,"C52"])
input$M18Pr6[input$segment_id == 1] <- as.numeric(x[1,"C53"])
input$M18Pr7[input$segment_id == 1] <- as.numeric(x[1,"C54"])
input$M18Pr8[input$segment_id == 1] <- as.numeric(x[1,"C55"])
input$M18Pr9[input$segment_id == 1] <- as.numeric(x[1,"C56"])
input$M18Pr10[input$segment_id == 1] <- as.numeric(x[1,"C57"])
input$M18Pr11[input$segment_id == 1] <- as.numeric(x[1,"C58"])
input$M18Pr12[input$segment_id == 1] <- as.numeric(x[1,"C59"])
input$M18Pr13[input$segment_id == 1] <- as.numeric(x[1,"C60"])
input$M18Pr14[input$segment_id == 1] <- as.numeric(x[1,"C61"])
input$M18Pr15[input$segment_id == 1] <- as.numeric(x[1,"C62"])
input$M18Pr16[input$segment_id == 1] <- as.numeric(x[1,"C63"])
input$M18Pr17[input$segment_id == 1] <- as.numeric(x[1,"C64"])
input$M18Pr18[input$segment_id == 1] <- as.numeric(x[1,"C65"])
input$M18Pr19[input$segment_id == 1] <- as.numeric(x[1,"C66"])
input$M18Pr20[input$segment_id == 1] <- as.numeric(x[1,"C67"])
input$M18Pr21[input$segment_id == 1] <- as.numeric(x[1,"C68"])
input$M18Pr22[input$segment_id == 1] <- as.numeric(x[1,"C69"])
input$M18Pr23[input$segment_id == 1] <- as.numeric(x[1,"C70"])
input$M18Pr24[input$segment_id == 1] <- as.numeric(x[1,"C71"])

input$M18Pr1[input$segment_id == 2] <- as.numeric(x[2,"C48"])
input$M18Pr2[input$segment_id == 2] <- as.numeric(x[2,"C49"])
input$M18Pr3[input$segment_id == 2] <- as.numeric(x[2,"C50"])
input$M18Pr4[input$segment_id == 2] <- as.numeric(x[2,"C51"])
input$M18Pr5[input$segment_id == 2] <- as.numeric(x[2,"C52"])
input$M18Pr6[input$segment_id == 2] <- as.numeric(x[2,"C53"])
input$M18Pr7[input$segment_id == 2] <- as.numeric(x[2,"C54"])
input$M18Pr8[input$segment_id == 2] <- as.numeric(x[2,"C55"])
input$M18Pr9[input$segment_id == 2] <- as.numeric(x[2,"C56"])
input$M18Pr10[input$segment_id == 2] <- as.numeric(x[2,"C57"])
input$M18Pr11[input$segment_id == 2] <- as.numeric(x[2,"C58"])
input$M18Pr12[input$segment_id == 2] <- as.numeric(x[2,"C59"])
input$M18Pr13[input$segment_id == 2] <- as.numeric(x[2,"C60"])
input$M18Pr14[input$segment_id == 2] <- as.numeric(x[2,"C61"])
input$M18Pr15[input$segment_id == 2] <- as.numeric(x[2,"C62"])
input$M18Pr16[input$segment_id == 2] <- as.numeric(x[2,"C63"])
input$M18Pr17[input$segment_id == 2] <- as.numeric(x[2,"C64"])
input$M18Pr18[input$segment_id == 2] <- as.numeric(x[2,"C65"])
input$M18Pr19[input$segment_id == 2] <- as.numeric(x[2,"C66"])
input$M18Pr20[input$segment_id == 2] <- as.numeric(x[2,"C67"])
input$M18Pr21[input$segment_id == 2] <- as.numeric(x[2,"C68"])
input$M18Pr22[input$segment_id == 2] <- as.numeric(x[2,"C69"])
input$M18Pr23[input$segment_id == 2] <- as.numeric(x[2,"C70"])
input$M18Pr24[input$segment_id == 2] <- as.numeric(x[2,"C71"])

input$M18Pr1[input$segment_id == 3] <- as.numeric(x[3,"C48"])
input$M18Pr2[input$segment_id == 3] <- as.numeric(x[3,"C49"])
input$M18Pr3[input$segment_id == 3] <- as.numeric(x[3,"C50"])
input$M18Pr4[input$segment_id == 3] <- as.numeric(x[3,"C51"])
input$M18Pr5[input$segment_id == 3] <- as.numeric(x[3,"C52"])
input$M18Pr6[input$segment_id == 3] <- as.numeric(x[3,"C53"])
input$M18Pr7[input$segment_id == 3] <- as.numeric(x[3,"C54"])
input$M18Pr8[input$segment_id == 3] <- as.numeric(x[3,"C55"])
input$M18Pr9[input$segment_id == 3] <- as.numeric(x[3,"C56"])
input$M18Pr10[input$segment_id == 3] <- as.numeric(x[3,"C57"])
input$M18Pr11[input$segment_id == 3] <- as.numeric(x[3,"C58"])
input$M18Pr12[input$segment_id == 3] <- as.numeric(x[3,"C59"])
input$M18Pr13[input$segment_id == 3] <- as.numeric(x[3,"C60"])
input$M18Pr14[input$segment_id == 3] <- as.numeric(x[3,"C61"])
input$M18Pr15[input$segment_id == 3] <- as.numeric(x[3,"C62"])
input$M18Pr16[input$segment_id == 3] <- as.numeric(x[3,"C63"])
input$M18Pr17[input$segment_id == 3] <- as.numeric(x[3,"C64"])
input$M18Pr18[input$segment_id == 3] <- as.numeric(x[3,"C65"])
input$M18Pr19[input$segment_id == 3] <- as.numeric(x[3,"C66"])
input$M18Pr20[input$segment_id == 3] <- as.numeric(x[3,"C67"])
input$M18Pr21[input$segment_id == 3] <- as.numeric(x[3,"C68"])
input$M18Pr22[input$segment_id == 3] <- as.numeric(x[3,"C69"])
input$M18Pr23[input$segment_id == 3] <- as.numeric(x[3,"C70"])
input$M18Pr24[input$segment_id == 3] <- as.numeric(x[3,"C71"])

# input$M18Pr1[input$segment_id == 1] <- .01
# input$M18Pr2[input$segment_id == 1] <- .02
# input$M18Pr3[input$segment_id == 1] <- .24
# input$M18Pr4[input$segment_id == 1] <- .06
# input$M18Pr5[input$segment_id == 1] <- .22
# input$M18Pr6[input$segment_id == 1] <- .14
# input$M18Pr7[input$segment_id == 1] <- .15
# input$M18Pr8[input$segment_id == 1] <- .07
# input$M18Pr9[input$segment_id == 1] <- .08
# input$M18Pr10[input$segment_id == 1] <- .03
# input$M18Pr11[input$segment_id == 1] <- .05
# input$M18Pr12[input$segment_id == 1] <- .17
# input$M18Pr13[input$segment_id == 1] <- .21
# input$M18Pr14[input$segment_id == 1] <- .12
# input$M18Pr15[input$segment_id == 1] <- .09
# input$M18Pr16[input$segment_id == 1] <- .11
# input$M18Pr17[input$segment_id == 1] <- .10
# input$M18Pr18[input$segment_id == 1] <- .18
# input$M18Pr19[input$segment_id == 1] <- .16
# input$M18Pr20[input$segment_id == 1] <- .13
# input$M18Pr21[input$segment_id == 1] <- .04
# input$M18Pr22[input$segment_id == 1] <- .19
# input$M18Pr23[input$segment_id == 1] <- .20
# input$M18Pr24[input$segment_id == 1] <- .23

# input$M18Pr1[input$segment_id == 2] <- .02
# input$M18Pr2[input$segment_id == 2] <- .01
# input$M18Pr3[input$segment_id == 2] <- .24
# input$M18Pr4[input$segment_id == 2] <- .03
# input$M18Pr5[input$segment_id == 2] <- .20
# input$M18Pr6[input$segment_id == 2] <- .11
# input$M18Pr7[input$segment_id == 2] <- .08
# input$M18Pr8[input$segment_id == 2] <- .23
# input$M18Pr9[input$segment_id == 2] <- .19
# input$M18Pr10[input$segment_id == 2] <- .04
# input$M18Pr11[input$segment_id == 2] <- .05
# input$M18Pr12[input$segment_id == 2] <- .15
# input$M18Pr13[input$segment_id == 2] <- .21
# input$M18Pr14[input$segment_id == 2] <- .12
# input$M18Pr15[input$segment_id == 2] <- .09
# input$M18Pr16[input$segment_id == 2] <- .10
# input$M18Pr17[input$segment_id == 2] <- .06
# input$M18Pr18[input$segment_id == 2] <- .18
# input$M18Pr19[input$segment_id == 2] <- .14
# input$M18Pr20[input$segment_id == 2] <- .13
# input$M18Pr21[input$segment_id == 2] <- .07
# input$M18Pr22[input$segment_id == 2] <- .16
# input$M18Pr23[input$segment_id == 2] <- .17
# input$M18Pr24[input$segment_id == 2] <- .22
# 
# input$M18Pr1[input$segment_id == 3] <- .01
# input$M18Pr2[input$segment_id == 3] <- .02
# input$M18Pr3[input$segment_id == 3] <- .24
# input$M18Pr4[input$segment_id == 3] <- .06
# input$M18Pr5[input$segment_id == 3] <- .22
# input$M18Pr6[input$segment_id == 3] <- .14
# input$M18Pr7[input$segment_id == 3] <- .15
# input$M18Pr8[input$segment_id == 3] <- .07
# input$M18Pr9[input$segment_id == 3] <- .08
# input$M18Pr10[input$segment_id == 3] <- .03
# input$M18Pr11[input$segment_id == 3] <- .05
# input$M18Pr12[input$segment_id == 3] <- .17
# input$M18Pr13[input$segment_id == 3] <- .21
# input$M18Pr14[input$segment_id == 3] <- .12
# input$M18Pr15[input$segment_id == 3] <- .09
# input$M18Pr16[input$segment_id == 3] <- .11
# input$M18Pr17[input$segment_id == 3] <- .10
# input$M18Pr18[input$segment_id == 3] <- .18
# input$M18Pr19[input$segment_id == 3] <- .16
# input$M18Pr20[input$segment_id == 3] <- .13
# input$M18Pr21[input$segment_id == 3] <- .04
# input$M18Pr22[input$segment_id == 3] <- .19
# input$M18Pr23[input$segment_id == 3] <- .20
# input$M18Pr24[input$segment_id == 3] <- .23

head(input, n=50L)

# Create the dataframe with just the columns I need and name the columns.
df <- data.frame(ncodpers=input$C1, M17P1=input$C48, M17P2=input$C49, M17P3=input$C50, M17P4=input$C51, M17P5=input$C52, M17P6=input$C53, M17P7=input$C54, M17P8=input$C55, M17P9=input$C56, M17P10=input$C57, M17P11=input$C58, M17P12=input$C59, M17P13=input$C60, M17P14=input$C61, M17P15=input$C62, M17P16=input$C63, M17P17=input$C64, M17P18=input$C65, M17P19=input$C66, M17P20=input$C67, M17P21=input$C68, M17P22=input$C69, M17P23=input$C70, M17P24=input$C71,
	M18Pr1=input$M18Pr1, M18Pr2=input$M18Pr2, M18Pr3=input$M18Pr3, M18Pr4=input$M18Pr4, M18Pr5=input$M18Pr5, M18Pr6=input$M18Pr6, M18Pr7=input$M18Pr7, M18Pr8=input$M18Pr8, M18Pr9=input$M18Pr9, M18Pr10=input$M18Pr10, M18Pr11=input$M18Pr11, M18Pr12=input$M18Pr12, M18Pr13=input$M18Pr13, M18Pr14=input$M18Pr14, M18Pr15=input$M18Pr15, M18Pr16=input$M18Pr16, M18Pr17=input$M18Pr17, M18Pr18=input$M18Pr18, M18Pr19=input$M18Pr19, M18Pr20=input$M18Pr20, M18Pr21=input$M18Pr21, M18Pr22=input$M18Pr22, M18Pr23=input$M18Pr23, M18Pr24=input$M18Pr24)

head(df)

#rm(input)

# Overwrite Product Probabilities to zero if the customer already has the product in Month 17.
df$M18Pr1 <- (1-df$M17P1)*df$M18Pr1
df$M18Pr2 <- (1-df$M17P2)*df$M18Pr2
df$M18Pr3 <- (1-df$M17P3)*df$M18Pr3
df$M18Pr4 <- (1-df$M17P4)*df$M18Pr4
df$M18Pr5 <- (1-df$M17P5)*df$M18Pr5
df$M18Pr6 <- (1-df$M17P6)*df$M18Pr6
df$M18Pr7 <- (1-df$M17P7)*df$M18Pr7
df$M18Pr8 <- (1-df$M17P8)*df$M18Pr8
df$M18Pr9 <- (1-df$M17P9)*df$M18Pr9
df$M18Pr10 <- (1-df$M17P10)*df$M18Pr10
df$M18Pr11 <- (1-df$M17P11)*df$M18Pr11
df$M18Pr12 <- (1-df$M17P12)*df$M18Pr12
df$M18Pr13 <- (1-df$M17P13)*df$M18Pr13
df$M18Pr14 <- (1-df$M17P14)*df$M18Pr14
df$M18Pr15 <- (1-df$M17P15)*df$M18Pr15
df$M18Pr16 <- (1-df$M17P16)*df$M18Pr16
df$M18Pr17 <- (1-df$M17P17)*df$M18Pr17
df$M18Pr18 <- (1-df$M17P18)*df$M18Pr18
df$M18Pr19 <- (1-df$M17P19)*df$M18Pr19
df$M18Pr20 <- (1-df$M17P20)*df$M18Pr20
df$M18Pr21 <- (1-df$M17P21)*df$M18Pr21
df$M18Pr22 <- (1-df$M17P22)*df$M18Pr22
df$M18Pr23 <- (1-df$M17P23)*df$M18Pr23
df$M18Pr24 <- (1-df$M17P24)*df$M18Pr24

head(df)

# Define function to sort recommendations and return top 7.
sort.recommendations <- function(passed_vector) {
	# takes in a row of data for a single customer (of length exactly 24 (for 24 products)) creates a temp array, sorts the array, returns the array.
	x <- data.frame(Product = c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12","P13","P14","P15","P16","P17","P18","P19","P20","P21","P22","P23","P24"), Probs = passed_vector, stringsAsFactors=FALSE)
	y <- x[order(-x$Probs),1]
	w <- paste(y[1],y[2],y[3],y[4],y[5],y[6],y[7])
	return(w)
}

# Define function to return ordered recommendations from a scored test file.
get.ordered.recommendations <- function(df) {
	output <- vector(mode="character", length = nrow(df))
	for (i in 1:nrow(df)){
		temp <- c(df[i,26], df[i,27], df[i,28], df[i,29],  df[i,30],  df[i,31],  df[i,32],  df[i,33],  df[i,34],  df[i,35],  df[i,36],  df[i,37],  df[i,38],  df[i,39],  df[i,40],  df[i,41],  df[i,42],  df[i,43],  df[i,44],  df[i,45],  df[i,46],  df[i,47],  df[i,48],  df[i,49])
		output[i] <- sort.recommendations(temp)
	}
	return(output)
}

# Create the added_products column.
df$added_products <- get.ordered.recommendations(df)

# Take a look...
head(df)

# Write to my_solution.csv.
df <- df[,c(1,50)]
write.csv(df, file = "../ksb_model4_solution_temp.csv", row.names = FALSE, quote = FALSE) 
