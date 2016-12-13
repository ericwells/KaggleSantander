# trying out a massive melting operation on the entire train_ver2 dataset

setwd("/Users/ericwells/Documents/KaggleSantander/myCodeForKaggleSantander")

# Read in the whole dang thing.
location <- "../train_ver2.csv"
input2 <- read.csv(location, stringsAsFactors=FALSE, header = TRUE)

# melt
library(reshape2)
df2 <- reshape(as.data.frame(input2), idvar = 'ncodpers', timevar = 'fecha_dato', direction = 'wide')
# it worked!  and only took about 10-20 minutes!
df5 <- df2[,c(1:47,70:93, 162:185,116:139,208:231,254:277,300:323,346:369,392:415,438:461,484:507,530:553,576:599,622:645,668:691,714:737,760:783)]
df6 <- df5[complete.cases(df5[,24:431]),] # this removes all rows that have any NA in any of the product indicator fields.



### SCRATCHPAD
df[df$ncodpers==1050934,c(1:5,783)]
write.csv(df, file = "../Joins/junk.csv", row.names = FALSE, quote = FALSE) 
df4 <- df2[df2$ncodpers==1050934,c(1:47,70:93, 162:185,116:139,208:231,254:277,300:323,346:369,392:415,438:461,484:507,530:553,576:599,622:645,668:691,714:737,760:783)]
df3 <- head(df2,n=10L)
header_names <- colnames(df3)
df2[is.na(df2$`ind_recibo_ult1.2016-05-28`),c(1,431)]

