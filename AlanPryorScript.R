# a script posted by Alan Pryor to get the products added in June 2015.

# This script outputs a csv file containing a list of new products purchased 
# each month by each customer.
library(data.table)
setwd('~/kaggle/competition-sandtander/')
df     <- fread("cleaned_train.csv")
labels <- names(df)[grepl("ind_+.*_+.*_",names(df))]
cols   <- c("ncodpers","month.id","month.previous.id",labels)
df     <- df[,names(df) %in% cols,with=F]
df     <- merge(df,df,by.x=c("ncodpers","month.previous.id"),by.y=c("ncodpers","month.id"),all.x=TRUE)
df[is.na(df)] <- 0
products <- rep("",nrow(df))
for (label in labels){
  colx  <- paste0(label,".x")
  coly  <- paste0(label,".y")
  diffs <- df[,.(get(colx)-get(coly))]
  products[diffs>0] <- paste0(products[diffs>0],label,sep=" ")
}

df <- df[,.(ncodpers,month.id,products)]
write.csv(df,"purchased-products.csv",row.names=FALSE)
dim(df[df$month.id==6 & df$products!="",])