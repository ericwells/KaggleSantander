# trying out a massive melting operation on the entire train_ver2 dataset

setwd("/Users/ericwells/Documents/KaggleSantander/myCodeForKaggleSantander")

# Read in the whole dang thing.
location <- "../train_ver2.csv"
input <- read.csv(location, stringsAsFactors=FALSE, header = TRUE)

# melt
library(reshape2)
df2 <- reshape(as.data.frame(input), idvar = 'ncodpers', timevar = 'fecha_dato', direction = 'wide') # it worked!  and only took about 10-20 minutes!
headerdf2 <- colnames(df2)
write.csv(headerdf2,"../headerdf2.csv", row.names=FALSE, quote=FALSE)

# write out to CSV in case R crashes then I don't have to do the melt again...
#write.csv(df2, file = "../df2.csv", row.names = FALSE, quote = FALSE) 
#df2 <- read.csv("../df2.csv", stringsAsFactors=FALSE, header = TRUE)

# subselect columns. ncodpers, targets 2016-05, demographics 2016-04, products 2016-04.
df3 <- df2[,c(1,760:783, 692:713, 714:737)]

### DATA CLEANSING ####################

# The melt kept every single record.  If the month was missing, then it just filled in NA.  Instead, for my regression approach I really only want customers who exist on 2016-05. A rough approximation is to just merge to test_ver2 (customers who exist on 2016-06).
location <- "../test_ver2.csv"
input <- read.csv(location, stringsAsFactors=FALSE, header = TRUE)
merge_id <- data.frame(input$ncodpers)
library(plyr)
merge_id <- rename(merge_id, c("input.ncodpers"="ncodpers"))
x <- merge(x=merge_id,y=df3, by="ncodpers", all.x = TRUE, all.y = FALSE) # x now contains the train dataset but only for customers that existed on test.
library(tidyr)
# I'll do all my mods on df4...
df4 <- x
headerdf4 <- colnames(df4)

### This is where I left off as of Wed Dec 14 17:30:15 PST 2016.

# Sexo is Gender.
summary(df4$sexo.2016.04.28)
summary(as.factor(df4$sexo.2016.04.28))
# Sexo has 4 missing values and 4364 NA.  I'm going to turn this into M, V, and all other (ZZZZZ).
df4$sexo.2016.04.28[is.na(df4$sexo.2016.04.28)] <- "ZZZZZ"

# pais_residencia is country of residence.
summary(as.factor(df4$pais_residencia.2016.04.28))
# pais_residencia has lots of values.  probably will not use it in regressions, so skip for now.

# age is age.
summary(as.numeric(df4$age.2016.04.28))
hist(as.numeric(df4$age))
# cap everything above 90. There are zeros, but leave them for now.

# antiguedad is history as a customer.
hist(as.numeric(df4$antiguedad.2016.04.28))
summary(as.numeric(df4$antiguedad.2016.04.28))

# canal-entrada is the channel the came in through.
levels(as.factor(df4$canal_entrada.2016.04.28))
# this is very messy data. Convert into just the good values and stuff everything else into "all other" (ZZZZZ).

# renta is Income.
hist(as.integer(df4$renta.2016.04.28))
summary(as.integer(df4$renta.2016.04.28))
table(as.integer(df4$renta.2016.04.28))
# this has some outliers. Perhaps try capping the outliers at around 200K.  there are also 38990 zeros, but let's just try leaving them in there for now.

#df4 <- df3[complete.cases(df3[,384:431]),] # this removes all rows that have any NA in any of the product indicator fields.  I decided to do only months 2016-03 and 2016-04.

### END DATA CLEANSING ###############

# create helper function to random sample rows from a dataframe
randomRows = function(df,n){
  return(df[sample(nrow(df),n),])
}

df4_samp <- randomRows(df4,1000)
#df4_samp <- head(df4,n=5000L)

# write out to CSV in case R crashes then I don't have to do read a huge file in again...
#write.csv(df4_samp, file = "../df4_samp.csv", row.names = FALSE, quote = FALSE) 
#df4_samp <- read.csv("../df4_samp.csv", row.names = FALSE, quote = FALSE)

# just some quick test of the modeling sytnax
#model11 <- lm(df4[,408] ~ df4[,2] + df4[,3]) # Oddly, this ran when I tried it a second time....  Could try it again using header_names[6] approach.
#model11 <- lm(df4$'ind_ahor_fin_ult1.2016-05-28' ~ df4$`segmento.2015-01-28` + df4$`cod_prov.2015-01-28`) # this was a test of syntax and it ran.
#junk <- predict(model11, newdata = data.frame(df4$`segmento.2015-01-28`, df4$`cod_prov.2015-01-28`)) # this was a test of syntax and it ran. Named num [1:926663]

# Build a training dataset using new names so that the predict function won't get confused.
training <- data.frame(df4_samp[,c(1,408:431,2:23,384:407)]) # this is ncodpers, then 24 targets, then 22 demographic predictors, then 24 product indicator predictors.
library(plyr)
training <- rename(training, c(
  "ncodpers"="ncodpers", 
  "ind_ahor_fin_ult1.2016.05.28"="T1", "ind_aval_fin_ult1.2016.05.28"="T2", "ind_cco_fin_ult1.2016.05.28"="T3", "ind_cder_fin_ult1.2016.05.28"="T4", "ind_cno_fin_ult1.2016.05.28"="T5", "ind_ctju_fin_ult1.2016.05.28"="T6", "ind_ctma_fin_ult1.2016.05.28"="T7", "ind_ctop_fin_ult1.2016.05.28"="T8", "ind_ctpp_fin_ult1.2016.05.28"="T9", "ind_deco_fin_ult1.2016.05.28"="T10", "ind_deme_fin_ult1.2016.05.28"="T11", "ind_dela_fin_ult1.2016.05.28"="T12", "ind_ecue_fin_ult1.2016.05.28"="T13", "ind_fond_fin_ult1.2016.05.28"="T14", "ind_hip_fin_ult1.2016.05.28"="T15", "ind_plan_fin_ult1.2016.05.28"="T16", "ind_pres_fin_ult1.2016.05.28"="T17", "ind_reca_fin_ult1.2016.05.28"="T18", "ind_tjcr_fin_ult1.2016.05.28"="T19", "ind_valo_fin_ult1.2016.05.28"="T20", "ind_viv_fin_ult1.2016.05.28"="T21", "ind_nomina_ult1.2016.05.28"="T22", "ind_nom_pens_ult1.2016.05.28"="T23", "ind_recibo_ult1.2016.05.28"="T24", 
  "ind_empleado.2015.01.28"="D1", "pais_residencia.2015.01.28"="D2", "sexo.2015.01.28"="D3", "age.2015.01.28"="D4", "fecha_alta.2015.01.28"="D5", "ind_nuevo.2015.01.28"="D6", "antiguedad.2015.01.28"="D7", "indrel.2015.01.28"="D8", "ult_fec_cli_1t.2015.01.28"="D9", "indrel_1mes.2015.01.28"="D10", "tiprel_1mes.2015.01.28"="D11", "indresi.2015.01.28"="D12", "indext.2015.01.28"="D13", "conyuemp.2015.01.28"="D14", "canal_entrada.2015.01.28"="D15", "indfall.2015.01.28"="D16", "tipodom.2015.01.28"="D17", "cod_prov.2015.01.28"="D18", "nomprov.2015.01.28"="D19", "ind_actividad_cliente.2015.01.28"="D20", "renta.2015.01.28"="D21", "segmento.2015.01.28"="D22", 
  "ind_ahor_fin_ult1.2016.04.28"="P1", "ind_aval_fin_ult1.2016.04.28"="P2", "ind_cco_fin_ult1.2016.04.28"="P3", "ind_cder_fin_ult1.2016.04.28"="P4", "ind_cno_fin_ult1.2016.04.28"="P5", "ind_ctju_fin_ult1.2016.04.28"="P6", "ind_ctma_fin_ult1.2016.04.28"="P7", "ind_ctop_fin_ult1.2016.04.28"="P8", "ind_ctpp_fin_ult1.2016.04.28"="P9", "ind_deco_fin_ult1.2016.04.28"="P10", "ind_deme_fin_ult1.2016.04.28"="P11", "ind_dela_fin_ult1.2016.04.28"="P12", "ind_ecue_fin_ult1.2016.04.28"="P13", "ind_fond_fin_ult1.2016.04.28"="P14", "ind_hip_fin_ult1.2016.04.28"="P15", "ind_plan_fin_ult1.2016.04.28"="P16", "ind_pres_fin_ult1.2016.04.28"="P17", "ind_reca_fin_ult1.2016.04.28"="P18", "ind_tjcr_fin_ult1.2016.04.28"="P19", "ind_valo_fin_ult1.2016.04.28"="P20", "ind_viv_fin_ult1.2016.04.28"="P21", "ind_nomina_ult1.2016.04.28"="P22", "ind_nom_pens_ult1.2016.04.28"="P23", "ind_recibo_ult1.2016.04.28"="P24"                          
   ))
# And now build a scoring dataset that has appropriate names that the predict function expects.
scoring <- data.frame(df3[,c(1,2:23,408:431)]) # TODO: still need to merge to test_ver2 ncodpers.
scoring <- rename(scoring, c(
  "ncodpers"="ncodpers",
  "ind_empleado.2015.01.28"="D1", "pais_residencia.2015.01.28"="D2", "sexo.2015.01.28"="D3", "age.2015.01.28"="D4", "fecha_alta.2015.01.28"="D5", "ind_nuevo.2015.01.28"="D6", "antiguedad.2015.01.28"="D7", "indrel.2015.01.28"="D8", "ult_fec_cli_1t.2015.01.28"="D9", "indrel_1mes.2015.01.28"="D10", "tiprel_1mes.2015.01.28"="D11", "indresi.2015.01.28"="D12", "indext.2015.01.28"="D13", "conyuemp.2015.01.28"="D14", "canal_entrada.2015.01.28"="D15", "indfall.2015.01.28"="D16", "tipodom.2015.01.28"="D17", "cod_prov.2015.01.28"="D18", "nomprov.2015.01.28"="D19", "ind_actividad_cliente.2015.01.28"="D20", "renta.2015.01.28"="D21", "segmento.2015.01.28"="D22",
  "ind_ahor_fin_ult1.2016.05.28"="P1", "ind_aval_fin_ult1.2016.05.28"="P2", "ind_cco_fin_ult1.2016.05.28"="P3", "ind_cder_fin_ult1.2016.05.28"="P4", "ind_cno_fin_ult1.2016.05.28"="P5", "ind_ctju_fin_ult1.2016.05.28"="P6", "ind_ctma_fin_ult1.2016.05.28"="P7", "ind_ctop_fin_ult1.2016.05.28"="P8", "ind_ctpp_fin_ult1.2016.05.28"="P9", "ind_deco_fin_ult1.2016.05.28"="P10", "ind_deme_fin_ult1.2016.05.28"="P11", "ind_dela_fin_ult1.2016.05.28"="P12", "ind_ecue_fin_ult1.2016.05.28"="P13", "ind_fond_fin_ult1.2016.05.28"="P14", "ind_hip_fin_ult1.2016.05.28"="P15", "ind_plan_fin_ult1.2016.05.28"="P16", "ind_pres_fin_ult1.2016.05.28"="P17", "ind_reca_fin_ult1.2016.05.28"="P18", "ind_tjcr_fin_ult1.2016.05.28"="P19", "ind_valo_fin_ult1.2016.05.28"="P20", "ind_viv_fin_ult1.2016.05.28"="P21", "ind_nomina_ult1.2016.05.28"="P22", "ind_nom_pens_ult1.2016.05.28"="P23", "ind_recibo_ult1.2016.05.28"="P24"
))

# Now run a logistic regression on the training dataset to predict just Product 1 (target=T1) using all the demographic predictors and all the product predictors.
#my_formula <- as.factor(T3)~D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11+D12+D13+D14+D15+D16+D17+D18+D19+D20+D21+D22+P1+P2+P3+P4+P5+P6+P7+P8+P9+P10+P11+P12+P13+P14+P15+P16+P17+P18+P19+P20+P21+P22+P23+P24
my_formula <- as.factor(T3)~D1+D2 # a baby model to test the syntax
model12 <- glm(formula = my_formula, family = binomial(link = "logit"), data = training, trace=TRUE, maxit=25, na.action=na.omit)
summary(model12)

# In order to get the predict function to work, need to check for any factor levels that didn't exist in training and convert to NA.
id <- which(!(scoring$D1 %in% levels(training$D1)))
scoring$D1[id] <- NA
# And now try predict syntax
junk <- predict(model12, newdata = scoring, type="response", na.action=na.omit)
# Crap.  The NA issue needs some more thinking.  Probably I should only drop NA rows when forming df6 from the predictor columns that I am actually using.  Then if there are any records that are on test_ver2 but that this predict function *does not output*, then I fill those in with a simpler freq based "model".

### SCRATCHPAD
df[df$ncodpers==1050934,c(1:5,783)]
write.csv(df, file = "../Joins/junk.csv", row.names = FALSE, quote = FALSE) 
df4 <- df2[df2$ncodpers==1050934,c(1:47,70:93, 162:185,116:139,208:231,254:277,300:323,346:369,392:415,438:461,484:507,530:553,576:599,622:645,668:691,714:737,760:783)]
df3 <- head(df2,n=10L)
header_names <- colnames(df6)
df2[is.na(df2$`ind_recibo_ult1.2016-05-28`),c(1,431)]
head(df6$'segmento.2015-01-28')
which(header_names=='ind_ahor_fin_ult1.2016-05-28')
attributes(model11)
model11$coefficients
write.csv(header_names, file = "../headernamesofdf6.csv", row.names = FALSE, quote = FALSE) 
l<-sapply(training,function(x)is.factor(x))
l
