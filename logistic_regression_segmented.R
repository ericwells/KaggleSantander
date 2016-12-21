# this is the block of code that melts train_ver2, creates a training dataset and scoring dataset, cleans the data, and runs a logistic regression.
# this version grows from logistic_regressions.R by segmenting the training dataset.  Whereas the previous version asked the regression to predict the 
#  probability of a customer getting a product in Month 18 regardless of whether they had it or not in Month 17, this version will only try to predict 
#  Month 18 if they don't already have the product in Month 17.

setwd("/Users/ericwells/Documents/KaggleSantander/myCodeForKaggleSantander")

# Read in the whole dang thing.
location <- "../train_ver2.csv"
input <- read.csv(location, stringsAsFactors=FALSE, header = TRUE)
# Note to self: on Dec 15, I double-checked that all the product indicators on train_ver2 are indeed 1/0.

# melt
library(reshape2)
df2 <- reshape(as.data.frame(input), idvar = 'ncodpers', timevar = 'fecha_dato', direction = 'wide') # it worked!  and only took about 10-20 minutes!
headerdf2 <- colnames(df2)
write.csv(headerdf2,"../headerdf2.csv", row.names=FALSE, quote=FALSE)

# write out to CSV in case R crashes then I don't have to do the melt again...
#write.csv(df2, file = "../df2.csv", row.names = FALSE, quote = FALSE) # strong suspicion that the quote parameter caused problems...
#df2 <- read.csv("../df2.csv", stringsAsFactors=FALSE, header = TRUE)

# subselect columns. For training, will need ncodpers, targets 2016-05, demographics 2016-04, products 2016-04. For scoring, will need ncodpers, demographics 2016-05, products 2016-05.
df3 <- df2[,c(1,760:783, 692:713, 714:737, 738:759)] 

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

# Sexo is Gender.
summary(df4$`sexo.2016-04-28`)
summary(as.factor(df4$`sexo.2016-04-28`))
# Sexo has 4 missing values and 4364 NA.  I'm going to turn this into H, V, and all other (ZZZZZ).
df4$`sexo.2016-04-28`[is.na(df4$`sexo.2016-04-28`)] <- "ZZZZZ"
df4$`sexo.2016-04-28`[df4$`sexo.2016-04-28` %in% c("","0","1","ES","N")] <- "ZZZZZ"
df4$`sexo.2016-04-28` <- as.factor(df4$`sexo.2016-04-28`)
# repeat for Scoring.
df4$`sexo.2016-05-28`[is.na(df4$`sexo.2016-05-28`)] <- "ZZZZZ"
df4$`sexo.2016-05-28`[df4$`sexo.2016-05-28` %in% c("","0","1","ES","N","US")] <- "ZZZZZ"
df4$`sexo.2016-05-28` <- as.factor(df4$`sexo.2016-05-28`)
# double-check
levels(df4$`sexo.2016-04-28`)
levels(df4$`sexo.2016-05-28`)
# sexo is ready.

# pais_residencia is country of residence.
summary(as.factor(df4$`pais_residencia.2016-04-28`))
# pais_residencia has lots of values.  probably will not use it in regressions, so skip for now.

# age is age.
summary(as.numeric(df4$`age.2016-04-28`))
hist(as.numeric(df4$`age.2016-04-28`))
# Convert to categorical variable.
df4$`age.2016-04-28` <- as.numeric(df4$`age.2016-04-28`)
df4$`age.2016-04-28`[is.na(df4$`age.2016-04-28`)] <- 0
df4$`age.2016-04-28` <- cut(df4$`age.2016-04-28`, breaks = c(-Inf, 1, 25, 35, 45, 55, 65, Inf), labels = c("0","1-24","25-34","35-44","45-54","55-64","65+"), right = FALSE)
# repeat for Scoring.
df4$`age.2016-05-28` <- as.numeric(df4$`age.2016-05-28`)
df4$`age.2016-05-28`[is.na(df4$`age.2016-05-28`)] <- 0
df4$`age.2016-05-28` <- cut(df4$`age.2016-05-28`, breaks = c(-Inf, 1, 25, 35, 45, 55, 65, Inf), labels = c("0","1-24","25-34","35-44","45-54","55-64","65+"), right = FALSE)
# double-check
levels(df4$`age.2016-04-28`)
levels(df4$`age.2016-05-28`)
# age is ready.

# antiguedad is history as a customer.
hist(as.numeric(df4$`antiguedad.2016-04-28`))
summary(as.numeric(df4$`antiguedad.2016-04-28`))
table(df4$`antiguedad.2016-04-28`)
# this is very messy data. Convert into just the good values and stuff everything else into "all other" (ZZZZZ).
df4$`antiguedad.2016-04-28` <- as.numeric(df4$`antiguedad.2016-04-28`)
df4$`antiguedad.2016-04-28`[is.na(df4$`antiguedad.2016-04-28`)] <- 0
df4$`antiguedad.2016-04-28` <- cut(df4$`antiguedad.2016-04-28`, breaks = c(-Inf, 1, 6, 12, 24, 48, 100, Inf), labels = c("0","1-5","6-11","12-23","24-47","48-99","ZZZZZ"))
# repeat for Scoring.
df4$`antiguedad.2016-05-28` <- as.numeric(df4$`antiguedad.2016-05-28`)
df4$`antiguedad.2016-05-28`[is.na(df4$`antiguedad.2016-05-28`)] <- 0
df4$`antiguedad.2016-05-28` <- cut(df4$`antiguedad.2016-05-28`, breaks = c(-Inf, 1, 6, 12, 24, 48, 100, Inf), labels = c("0","1-5","6-11","12-23","24-47","48-99","ZZZZZ"))
# double-check
levels(df4$`antiguedad.2016-04-28`)
levels(df4$`antiguedad.2016-05-28`)
# antiguedad is ready.

# canal-entrada is the channel the came in through.
levels(as.factor(df4$`canal_entrada.2016-04-28`))
table(df4$`canal_entrada.2016-04-28`)
# this is very messy data. Convert into just the values with heavy counts and stuff everything else into "all other" (ZZZZZ).
df4$`canal_entrada.2016-04-28`[is.na(df4$`canal_entrada.2016-04-28`)] <- "ZZZZZ"
df4$`canal_entrada.2016-04-28`[!(df4$`canal_entrada.2016-04-28` %in% c("KAT","KHM","KHN","KHQ","KFC","KFA"))] <- "ZZZZZ"
df4$`canal_entrada.2016-04-28` <- as.factor(df4$`canal_entrada.2016-04-28`)
# repeat for Scoring.
df4$`canal_entrada.2016-05-28`[is.na(df4$`canal_entrada.2016-05-28`)] <- "ZZZZZ"
df4$`canal_entrada.2016-05-28`[!(df4$`canal_entrada.2016-05-28` %in% c("KAT","KHM","KHN","KHQ","KFC","KFA"))] <- "ZZZZZ"
df4$`canal_entrada.2016-05-28` <- as.factor(df4$`canal_entrada.2016-05-28`)
# double-check
levels(df4$`canal_entrada.2016-04-28`)
levels(df4$`canal_entrada.2016-05-28`)
# canal-entrada is ready.

# renta is Income.
hist(as.integer(df4$`renta.2016-04-28`))
summary(as.integer(df4$`renta.2016-04-28`))
table(as.integer(df4$`renta.2016-04-28`))
# this has some outliers. Perhaps try capping the outliers at around 200K.  there are also 38990 zeros, but let's just try leaving them in there for now.
df4$`renta.2016-04-28` <- as.integer(df4$`renta.2016-04-28`)
df4$`renta.2016-04-28`[is.na(df4$`renta.2016-04-28`)] <- 0
df4$`renta.2016-04-28`[df4$`renta.2016-04-28` >= 200000] <- 200000
# Convert to categorical variable.
df4$`renta.2016-04-28` <- cut(df4$`renta.2016-04-28`, breaks = c(-Inf, 1, 30000, 50000, 80000, 120000, 150000, 200000, Inf), labels = c("0","1-29K","30K-49K","50K-79K","80K-119K","120K-149K","150K-199K", "200K+"), right = FALSE)
# repeat for Scoring.
df4$`renta.2016-05-28` <- as.integer(df4$`renta.2016-05-28`)
df4$`renta.2016-05-28`[is.na(df4$`renta.2016-05-28`)] <- 0
df4$`renta.2016-05-28`[df4$`renta.2016-05-28` >= 200000] <- 200000
# Convert to categorical variable.
df4$`renta.2016-05-28` <- cut(df4$`renta.2016-05-28`, breaks = c(-Inf, 1, 30000, 50000, 80000, 120000, 150000, 200000, Inf), labels = c("0","1-29K","30K-49K","50K-79K","80K-119K","120K-149K","150K-199K", "200K+"), right = FALSE)
# double-check
summary(df4$`renta.2016-04-28`)
summary(df4$`renta.2016-05-28`)
# renta is ready. 

### END DATA CLEANSING ###############

# create helper function to random sample rows from a dataframe
randomRows = function(df,n){
  return(df[sample(nrow(df),n),])
}

# Build a training dataset using new names so that the predict function won't get confused.
#saving this version for when I build more predictors off the whole 12 month history... training <- data.frame(df4_samp[,c(1,408:431,2:23,384:407)]) # this is ncodpers, then 24 targets, then 22 demographic predictors, then 24 product indicator predictors. 71 variables.
set.seed(42)
training <- df4[,c(1:71)] # this is ncodpers, then 24 targets, then 22 demographic predictors, then 24 product indicator predictors. 71 variables.
library(plyr)
training <- rename(training, c(
  "ncodpers"="ncodpers", 
  "ind_ahor_fin_ult1.2016-05-28"="T1", "ind_aval_fin_ult1.2016-05-28"="T2", "ind_cco_fin_ult1.2016-05-28"="T3", "ind_cder_fin_ult1.2016-05-28"="T4", "ind_cno_fin_ult1.2016-05-28"="T5", "ind_ctju_fin_ult1.2016-05-28"="T6", "ind_ctma_fin_ult1.2016-05-28"="T7", "ind_ctop_fin_ult1.2016-05-28"="T8", "ind_ctpp_fin_ult1.2016-05-28"="T9", "ind_deco_fin_ult1.2016-05-28"="T10", "ind_deme_fin_ult1.2016-05-28"="T11", "ind_dela_fin_ult1.2016-05-28"="T12", "ind_ecue_fin_ult1.2016-05-28"="T13", "ind_fond_fin_ult1.2016-05-28"="T14", "ind_hip_fin_ult1.2016-05-28"="T15", "ind_plan_fin_ult1.2016-05-28"="T16", "ind_pres_fin_ult1.2016-05-28"="T17", "ind_reca_fin_ult1.2016-05-28"="T18", "ind_tjcr_fin_ult1.2016-05-28"="T19", "ind_valo_fin_ult1.2016-05-28"="T20", "ind_viv_fin_ult1.2016-05-28"="T21", "ind_nomina_ult1.2016-05-28"="T22", "ind_nom_pens_ult1.2016-05-28"="T23", "ind_recibo_ult1.2016-05-28"="T24", 
  "ind_empleado.2016-04-28"="D1", "pais_residencia.2016-04-28"="D2", "sexo.2016-04-28"="D3", "age.2016-04-28"="D4", "fecha_alta.2016-04-28"="D5", "ind_nuevo.2016-04-28"="D6", "antiguedad.2016-04-28"="D7", "indrel.2016-04-28"="D8", "ult_fec_cli_1t.2016-04-28"="D9", "indrel_1mes.2016-04-28"="D10", "tiprel_1mes.2016-04-28"="D11", "indresi.2016-04-28"="D12", "indext.2016-04-28"="D13", "conyuemp.2016-04-28"="D14", "canal_entrada.2016-04-28"="D15", "indfall.2016-04-28"="D16", "tipodom.2016-04-28"="D17", "cod_prov.2016-04-28"="D18", "nomprov.2016-04-28"="D19", "ind_actividad_cliente.2016-04-28"="D20", "renta.2016-04-28"="D21", "segmento.2016-04-28"="D22", 
  "ind_ahor_fin_ult1.2016-04-28"="P1", "ind_aval_fin_ult1.2016-04-28"="P2", "ind_cco_fin_ult1.2016-04-28"="P3", "ind_cder_fin_ult1.2016-04-28"="P4", "ind_cno_fin_ult1.2016-04-28"="P5", "ind_ctju_fin_ult1.2016-04-28"="P6", "ind_ctma_fin_ult1.2016-04-28"="P7", "ind_ctop_fin_ult1.2016-04-28"="P8", "ind_ctpp_fin_ult1.2016-04-28"="P9", "ind_deco_fin_ult1.2016-04-28"="P10", "ind_deme_fin_ult1.2016-04-28"="P11", "ind_dela_fin_ult1.2016-04-28"="P12", "ind_ecue_fin_ult1.2016-04-28"="P13", "ind_fond_fin_ult1.2016-04-28"="P14", "ind_hip_fin_ult1.2016-04-28"="P15", "ind_plan_fin_ult1.2016-04-28"="P16", "ind_pres_fin_ult1.2016-04-28"="P17", "ind_reca_fin_ult1.2016-04-28"="P18", "ind_tjcr_fin_ult1.2016-04-28"="P19", "ind_valo_fin_ult1.2016-04-28"="P20", "ind_viv_fin_ult1.2016-04-28"="P21", "ind_nomina_ult1.2016-04-28"="P22", "ind_nom_pens_ult1.2016-04-28"="P23", "ind_recibo_ult1.2016-04-28"="P24"                          
))

# Right here is the change to segmented. 
# get just the rows of customers who DON'T have Product 3 in Month 17.
# and down-sample.
training_seg3 <- randomRows(training[(training$P3==0),],100000)
training_seg5 <- randomRows(training[(training$P5==0),],100000)
training_seg8 <- randomRows(training[(training$P8==0),],100000)
training_seg9 <- randomRows(training[(training$P9==0),],100000)
training_seg12 <- randomRows(training[(training$P12==0),],100000)
training_seg13 <- randomRows(training[(training$P13==0),],100000)
training_seg14 <- randomRows(training[(training$P14==0),],100000)
training_seg18 <- randomRows(training[(training$P18==0),],100000)
training_seg19 <- randomRows(training[(training$P19==0),],100000)
training_seg20 <- randomRows(training[(training$P20==0),],100000)
training_seg22 <- randomRows(training[(training$P22==0),],100000)
training_seg23 <- randomRows(training[(training$P23==0),],100000)
training_seg24 <- randomRows(training[(training$P24==0),],100000)

# And now build a scoring dataset that has appropriate names that the predict function expects.
# All I need from test_ver2 is the list of IDs.  The predictors all come from months that are found on train_ver2.  Therefore I can build scoring from df4 (which was already merged against test_ver2 IDs).
scoring <- data.frame(df4[,c(1,72:93,2:25)])
scoring <- rename(scoring, c(
  "ncodpers"="ncodpers",
  "ind_empleado.2016.05.28"="D1", "pais_residencia.2016.05.28"="D2", "sexo.2016.05.28"="D3", "age.2016.05.28"="D4", "fecha_alta.2016.05.28"="D5", "ind_nuevo.2016.05.28"="D6", "antiguedad.2016.05.28"="D7", "indrel.2016.05.28"="D8", "ult_fec_cli_1t.2016.05.28"="D9", "indrel_1mes.2016.05.28"="D10", "tiprel_1mes.2016.05.28"="D11", "indresi.2016.05.28"="D12", "indext.2016.05.28"="D13", "conyuemp.2016.05.28"="D14", "canal_entrada.2016.05.28"="D15", "indfall.2016.05.28"="D16", "tipodom.2016.05.28"="D17", "cod_prov.2016.05.28"="D18", "nomprov.2016.05.28"="D19", "ind_actividad_cliente.2016.05.28"="D20", "renta.2016.05.28"="D21", "segmento.2016.05.28"="D22",
  "ind_ahor_fin_ult1.2016.05.28"="P1", "ind_aval_fin_ult1.2016.05.28"="P2", "ind_cco_fin_ult1.2016.05.28"="P3", "ind_cder_fin_ult1.2016.05.28"="P4", "ind_cno_fin_ult1.2016.05.28"="P5", "ind_ctju_fin_ult1.2016.05.28"="P6", "ind_ctma_fin_ult1.2016.05.28"="P7", "ind_ctop_fin_ult1.2016.05.28"="P8", "ind_ctpp_fin_ult1.2016.05.28"="P9", "ind_deco_fin_ult1.2016.05.28"="P10", "ind_deme_fin_ult1.2016.05.28"="P11", "ind_dela_fin_ult1.2016.05.28"="P12", "ind_ecue_fin_ult1.2016.05.28"="P13", "ind_fond_fin_ult1.2016.05.28"="P14", "ind_hip_fin_ult1.2016.05.28"="P15", "ind_plan_fin_ult1.2016.05.28"="P16", "ind_pres_fin_ult1.2016.05.28"="P17", "ind_reca_fin_ult1.2016.05.28"="P18", "ind_tjcr_fin_ult1.2016.05.28"="P19", "ind_valo_fin_ult1.2016.05.28"="P20", "ind_viv_fin_ult1.2016.05.28"="P21", "ind_nomina_ult1.2016.05.28"="P22", "ind_nom_pens_ult1.2016.05.28"="P23", "ind_recibo_ult1.2016.05.28"="P24"
))

# Note to self: remember to save the workspace at this point in the code.

# May need to strip out the products that don't have enough data in the target.  Could then basically assume their prob is zero.
# let's take a quick check...
table(training[,2])
for (i in 2:25){
  print(paste("T",i-1,table(df4[,i])))
}
# the only Targets that have interesting counts are T3, T8, and T24.  
# definitely teeny counts are T1, T2, T4, T10, T11.
# so I am going to try logistic regressions only on T3, T5, T8, T9, T12, T13, T14, T18, T19, T20, T22, T23, T24.
# [1] "T 1 929537" "T 1 78"    
# [1] "T 2 929599" "T 2 16"    
# [1] "T 3 368004" "T 3 561611"
# [1] "T 4 929299" "T 4 316"   
# [1] "T 5 856554" "T 5 73061" 
# [1] "T 6 922034" "T 6 7581"  
# [1] "T 7 921518" "T 7 8097"  
# [1] "T 8 829026" "T 8 100589"
# [1] "T 9 896344" "T 9 33271" 
# [1] "T 10 929298" "T 10 317"   
# [1] "T 11 928594" "T 11 1021"  
# [1] "T 12 898398" "T 12 31217" 
# [1] "T 13 853772" "T 13 75843" 
# [1] "T 14 914928" "T 14 14687" 
# [1] "T 15 925087" "T 15 4528"  
# [1] "T 16 922256" "T 16 7359"  
# [1] "T 17 927624" "T 17 1991"  
# [1] "T 18 884116" "T 18 45499" 
# [1] "T 19 894793" "T 19 34822" 
# [1] "T 20 908189" "T 20 21426" 
# [1] "T 21 926656" "T 21 2959"  
# [1] "T 22 881311" "T 22 48304" 
# [1] "T 23 876772" "T 23 52843" 
# [1] "T 24 816600" "T 24 113015"
# In retrospect, it probably would have been a better approach to create a target variable of "added this product in the target month" rather than the approach I chose (dedup against the product set they aleady have *after* making the prediction).

# IMPROVEMENT: many of the target variables have heavily skewed counts of 1 vs 0.  could probably do better if we sampled differently - a stratified random sample.

table(training_seg3$T3)
# Now run a logistic regression on the training dataset to predict just one product using various predictors from demographics and product.
#my_formula <- as.factor(T3)~D3+D4 # a baby model to test the syntax. D3 is Sexo, D4 is Age. 
#my_formula <- as.factor(T3)~D3+D4+D7+D15+D21 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
my_formula <- as.factor(T3)~D3+D4+D7+D15+D21+P1+P2+P4+P5+P6+P7+P8+P9+P10+P11+P12+P13+P14+P15+P16+P17+P18+P19+P20+P21+P22+P23+P24 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
#my_formula <- as.factor(T3)~D4+D7+D15+P5+P8+P9+P11+P12+P13+P14+P18+P19+P20+P21+P22+P23+P24 # final vars in the model.
model13_T3 <- glm(formula = my_formula, family = binomial(link = "logit"), data = training_seg3, trace=TRUE, maxit=25, na.action=na.omit)
summary(model13_T3) # Note: Deviance - lower is better. "Null deviance" is just using the intercept. "Residual deviance" includes using the predictors.  AIC: only useful for comparing models on the same target - lower is better.

# In order to get the predict function to work, we generally need to check for any factor levels that didn't exist in training and convert to NA.
# I think I can skip this step in my program since I did data cleansing on scoring along with training.
#id <- which(!(scoring$D1 %in% levels(training$D1)))
#scoring$D1[id] <- NA

# And now try predict syntax
PrT3 <- predict(model13_T3, newdata = scoring, type="response", na.action=na.omit)
# I'll need to have 24 predictions vectors here.  Then I'll stitch them together into a dataframe for the recommendation sorting/deduping phase.  WRONG!  Easier is to just run predict on every single
#  record in the scoring dataset.  A bunch of the rows will be invalid scores, but those exact rows will be overwritten to zero later anyways!  Nice shortcut...

# need to check if I have enough rows in the output.  Should be 929615.
length(PrT3)
# Yes it does.

# confirm that the predict function output 24 probabilities between 1 and 24.
summary(PrT3)
# Yes, for T3 they are in range 0-1.

# Need to figure out how to run all 24 predictions and get them in the right place.
# My initial thought here is that each of the 24 predictions ultimately be selecting different predictors.  Even though is will add many lines of code, perhaps it would be good to build each of the 24 models manually.
# So could have "model13_T1", "model13_T2", ..., "model13_T24".

# Here is the section where I will run the logistic regression for products 3 (already done above), 5, 8, 9, 12, 13, 14, 18, 19, 20, 22, 23, and 24.

# Product 5
table(training_seg5$T5)
#my_formula <- as.factor(T5)~D3+D4 # a baby model to test the syntax. D3 is Sexo, D4 is Age. 
#my_formula <- as.factor(T5)~D3+D4+D7+D15+D21 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
#my_formula <- as.factor(T5)~D3+D4+D7+D15+D21+P1+P2+P3+P4+P6+P7+P8+P9+P10+P11+P12+P13+P14+P15+P16+P17+P18+P19+P20+P21+P22+P23+P24 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
my_formula <- as.factor(T5)~D4+D7+D15+P13+P19+P20+P22+P23+P24 
#my_formula <- as.factor(T5)~D4+D7+D15+P3+P8+P9+P12+P13+P18+P19+P23+P24 # final vars in the model.
model13_T5 <- glm(formula = my_formula, family = binomial(link = "logit"), data = training_seg5, trace=TRUE, maxit=25, na.action=na.omit)
summary(model13_T5)
PrT5 <- predict(model13_T5, newdata = scoring, type="response", na.action=na.omit)
summary(PrT5)
# rank-deficient warning. Fixed by removing some of the predictors.

# Product 8
table(training_seg8$T8)
#my_formula <- as.factor(T8)~D3+D4 # a baby model to test the syntax. D3 is Sexo, D4 is Age. 
#my_formula <- as.factor(T8)~D3+D4+D7+D15+D21 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
#my_formula <- as.factor(T8)~D3+D4+D7+D15+D21+P1+P2+P3+P4+P5+P6+P7+P9+P10+P11+P12+P13+P14+P15+P16+P17+P18+P19+P20+P21+P22+P23+P24 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
my_formula <- as.factor(T8)~D4+D7+D15+D21+P1+P2+P3+P4+P5+P6+P7+P9+P10+P11+P12+P13+P14+P15+P16+P17+P18+P19+P20+P21+P22+P23+P24 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
#my_formula <- as.factor(T8)~D4+D15+D21+P3+P5+P7+P10+P11+P12+P13+P14+P15+P17+P20+P21+P22+P23 # final vars in the model.
model13_T8 <- glm(formula = my_formula, family = binomial(link = "logit"), data = training_seg8, trace=TRUE, maxit=25, na.action=na.omit)
summary(model13_T8)
PrT8 <- predict(model13_T8, newdata = scoring, type="response", na.action=na.omit)
summary(PrT8)
# rank-deficient warning.  This is probably because there are only 28 targets out of 99K+...
# For this model 13 version, I'll set them all to zero.

# Product 9
table(training_seg9$T9)
#my_formula <- as.factor(T9)~D3+D4 # a baby model to test the syntax. D3 is Sexo, D4 is Age. 
#my_formula <- as.factor(T9)~D3+D4+D7+D15+D21 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
my_formula <- as.factor(T9)~D3+D4+D7+D15+D21+P1+P2+P3+P4+P5+P6+P7+P8+P10+P11+P12+P13+P14+P15+P16+P17+P18+P19+P20+P21+P22+P23+P24 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
#my_formula <- as.factor(T9)~D15+D21+P3+P5+P7+P12+P13+P14+P15+P16+P18+P19+P20+P21+P24 # final vars in the model.
model13_T9 <- glm(formula = my_formula, family = binomial(link = "logit"), data = training_seg9, trace=TRUE, maxit=25, na.action=na.omit)
summary(model13_T9)
PrT9 <- predict(model13_T9, newdata = scoring, type="response", na.action=na.omit)
head(PrT9,n=50L)
summary(PrT9)
# rank-deficient warning.  This is probably because there are only 21 targets out of 99K+...
# For this model 13 version, I'll set them all to zero.

# Product 12
table(training_seg12$T12)
#my_formula <- as.factor(T12)~D3+D4 # a baby model to test the syntax. D3 is Sexo, D4 is Age. 
#my_formula <- as.factor(T12)~D3+D4+D7+D15+D21 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
my_formula <- as.factor(T12)~D3+D4+D7+D15+D21+P1+P2+P3+P4+P5+P6+P7+P8+P9+P10+P11+P13+P14+P15+P16+P17+P18+P19+P20+P21+P22+P23+P24 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
#my_formula <- as.factor(T12)~D4+D7+D15+D21+P1+P3+P5+P8+P9+P10+P11+P13+P14+P17+P18+P19+P24 # final vars in the model.
model13_T12 <- glm(formula = my_formula, family = binomial(link = "logit"), data = training_seg12, trace=TRUE, maxit=25, na.action=na.omit)
summary(model13_T12)
PrT12 <- predict(model13_T12, newdata = scoring, type="response", na.action=na.omit)
head(PrT12,n=50L)
summary(PrT12)
# rank-deficient warning.  This is probably because there are only 28 targets out of 99K+...
# For this model 13 version, I'll set them all to zero.

# Product 13
table(training_seg13$T13)
#my_formula <- as.factor(T13)~D3+D4 # a baby model to test the syntax. D3 is Sexo, D4 is Age. 
#my_formula <- as.factor(T13)~D3+D4+D7+D15+D21 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
my_formula <- as.factor(T13)~D4+D7+D15+P3+P5+P6+P7+P12+P14+P19+P20+P24 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
#my_formula <- as.factor(T13)~D4+D7+D15+D21+P3+P5+P6+P7+P8+P9+P12+P14+P16+P17+P18+P19+P20+P21+P22+P23+P24 # final vars in the model.
model13_T13 <- glm(formula = my_formula, family = binomial(link = "logit"), data = training_seg13, trace=TRUE, maxit=25, na.action=na.omit)
summary(model13_T13)
PrT13 <- predict(model13_T13, newdata = scoring, type="response", na.action=na.omit)
summary(PrT13)

# Product 14
table(training_seg14$T14)
#my_formula <- as.factor(T14)~D3+D4 # a baby model to test the syntax. D3 is Sexo, D4 is Age. 
#my_formula <- as.factor(T14)~D3+D4+D7+D15+D21 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
my_formula <- as.factor(T14)~D3+D4+D7+D15+D21+P1+P2+P3+P4+P5+P6+P7+P8+P9+P10+P11+P12+P13+P15+P16+P17+P18+P19+P20+P21+P22+P23+P24 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
#my_formula <- as.factor(T14)~D4+D15+D21+P3+P5+P8+P9+P12+P13+P15+P16+P20 # final vars in the model.
model13_T14 <- glm(formula = my_formula, family = binomial(link = "logit"), data = training_seg14, trace=TRUE, maxit=25, na.action=na.omit)
summary(model13_T14)
PrT14 <- predict(model13_T14, newdata = scoring, type="response", na.action=na.omit)
head(PrT14,n=50L)
summary(PrT14)
# rank-deficient warning.  This is probably because there are only 7 targets out of 99K+...
# For this model 13 version, I'll set them all to zero.

# Product 18
table(training_seg18$T18)
#my_formula <- as.factor(T18)~D3+D4 # a baby model to test the syntax. D3 is Sexo, D4 is Age. 
#my_formula <- as.factor(T18)~D3+D4+D7+D15+D21 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
#my_formula <- as.factor(T18)~D4+D7+D15+D21+P1+P2+P3+P4+P5+P6+P7+P8+P9+P10+P11+P12+P13+P14+P15+P16+P17+P19+P20+P21+P22+P23+P24 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
my_formula <- as.factor(T18)~D4+P3+P19+P24 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
#my_formula <- as.factor(T18)~D4+D7+D15+D21+P3+P5+P9+P12+P13+P15+P17+P19+P20+P21+P22+P23+P24 # final vars in the model.
model13_T18 <- glm(formula = my_formula, family = binomial(link = "logit"), data = training_seg18, trace=TRUE, maxit=25, na.action=na.omit)
summary(model13_T18)
PrT18 <- predict(model13_T18, newdata = scoring, type="response", na.action=na.omit)
summary(PrT18)
# rank-deficient warning.  This is probably because there are only 42 targets out of 99K+...
# Reducing number of variables a lot...

# Product 19
table(training_seg19$T19)
#my_formula <- as.factor(T19)~D3+D4 # a baby model to test the syntax. D3 is Sexo, D4 is Age. 
#my_formula <- as.factor(T19)~D3+D4+D7+D15+D21 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
my_formula <- as.factor(T19)~D4+D7+D15+D21+P1+P2+P3+P4+P5+P6+P7+P8+P9+P10+P11+P12+P13+P14+P15+P16+P17+P18+P20+P21+P22+P23+P24 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
#my_formula <- as.factor(T19)~D4+D15+D21+P3+P5+P7+P8+P9+P12+P13+P16+P18+P20+P21+P23+P24 # final vars in the model.
model13_T19 <- glm(formula = my_formula, family = binomial(link = "logit"), data = training_seg19, trace=TRUE, maxit=25, na.action=na.omit)
summary(model13_T19)
PrT19 <- predict(model13_T19, newdata = scoring, type="response", na.action=na.omit)
summary(PrT19)

# Product 20
table(training_seg20$T20)
#my_formula <- as.factor(T20)~D3+D4 # a baby model to test the syntax. D3 is Sexo, D4 is Age. 
#my_formula <- as.factor(T20)~D3+D4+D7+D15+D21 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
my_formula <- as.factor(T20)~D3+D4+D7+D15+D21+P1+P2+P3+P4+P5+P6+P7+P8+P9+P10+P11+P12+P13+P14+P15+P16+P17+P18+P19+P21+P22+P23+P24 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
#my_formula <- as.factor(T20)~D4+D15+D21+P3+P4+P5+P7+P8+P9+P10+P13+P14+P16+P18+P19+P22+P23+P24 # final vars in the model.
model13_T20 <- glm(formula = my_formula, family = binomial(link = "logit"), data = training_seg20, trace=TRUE, maxit=25, na.action=na.omit)
summary(model13_T20)
PrT20 <- predict(model13_T20, newdata = scoring, type="response", na.action=na.omit)
head(PrT20,n=50L)
summary(PrT20)
# rank-deficient warning.  This is probably because there are only 20 targets out of 99K+...
# For this model 13 version, I'll set them all to zero.

# Product 22
table(training_seg22$T22)
#my_formula <- as.factor(T22)~D3+D4 # a baby model to test the syntax. D3 is Sexo, D4 is Age. 
#my_formula <- as.factor(T22)~D3+D4+D7+D15+D21 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
#my_formula <- as.factor(T22)~D3+D4+D7+D15+D21+P1+P2+P3+P4+P5+P6+P7+P8+P9+P10+P11+P12+P13+P14+P15+P16+P17+P18+P19+P20+P21+P23+P24 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
my_formula <- as.factor(T22)~D4+D7+D15+D21+P3+P5+P12+P19+P21+P23+P24 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
#my_formula <- as.factor(T22)~D4+D7+D15+P5+P7+P8+P9+P13+P15+P18+P19+P23+P24 # final vars in the model.
model13_T22 <- glm(formula = my_formula, family = binomial(link = "logit"), data = training_seg22, trace=TRUE, maxit=25, na.action=na.omit)
summary(model13_T22)
PrT22 <- predict(model13_T22, newdata = scoring, type="response", na.action=na.omit)
summary(PrT22)

# Product 23
table(training_seg23$T23)
#my_formula <- as.factor(T23)~D3+D4 # a baby model to test the syntax. D3 is Sexo, D4 is Age. 
#my_formula <- as.factor(T23)~D3+D4+D7+D15+D21 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
#my_formula <- as.factor(T23)~D3+D4+D7+D15+D21+P1+P2+P3+P4+P5+P6+P7+P8+P9+P10+P11+P12+P13+P14+P15+P16+P17+P18+P19+P20+P21+P22+P24 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
my_formula <- as.factor(T23)~D4+D7+D15+P3+P5+P6+P7+P10+P19+P24 # final vars in the model.
model13_T23 <- glm(formula = my_formula, family = binomial(link = "logit"), data = training_seg23, trace=TRUE, maxit=25, na.action=na.omit)
summary(model13_T23)
PrT23 <- predict(model13_T23, newdata = scoring, type="response", na.action=na.omit)
summary(PrT23)

# Product 24
table(training_seg24$T24)
#my_formula <- as.factor(T24)~D3+D4 # a baby model to test the syntax. D3 is Sexo, D4 is Age. 
#my_formula <- as.factor(T24)~D3+D4+D7+D15+D21 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
#my_formula <- as.factor(T24)~D4+D7+D15+D21+P1+P2+P3+P4+P5+P6+P7+P8+P9+P10+P11+P12+P13+P14+P15+P16+P17+P18+P19+P20+P21+P22+P23 # a model to test what happens if just using the good demographic predictors. D3 is Sexo, D4 is Age, D7 is Antiguedad (time as customer), D15 is canal-entrada (channel), D21 is renta (income).
my_formula <- as.factor(T24)~D4+D7+D15+D21+P3+P5+P9+P13+P15+P18+P19+P20 # final vars in the model.
model13_T24 <- glm(formula = my_formula, family = binomial(link = "logit"), data = training_seg24, trace=TRUE, maxit=25, na.action=na.omit)
summary(model13_T24)
PrT24 <- predict(model13_T24, newdata = scoring, type="response", na.action=na.omit)
summary(PrT24)

# For my initial pass at the logistic regression approach, some of the targets are useless so I will just fill the Probabilities for those products manually to all zeros.
PrT1 <- vector(mode = "numeric", length = 929615)
PrT2 <- vector(mode = "numeric", length = 929615)
PrT4 <- vector(mode = "numeric", length = 929615)
PrT6 <- vector(mode = "numeric", length = 929615)
PrT7 <- vector(mode = "numeric", length = 929615)
PrT10 <- vector(mode = "numeric", length = 929615)
PrT11 <- vector(mode = "numeric", length = 929615)
PrT15 <- vector(mode = "numeric", length = 929615)
PrT16 <- vector(mode = "numeric", length = 929615)
PrT17 <- vector(mode = "numeric", length = 929615)
PrT21 <- vector(mode = "numeric", length = 929615)
# and need to add a few additional zeros for the targets that were just too small in the model13 segmented version...
PrT8 <- vector(mode = "numeric", length = 929615)
PrT9 <- vector(mode = "numeric", length = 929615)
PrT12 <- vector(mode = "numeric", length = 929615)
PrT14 <- vector(mode = "numeric", length = 929615)
PrT20 <- vector(mode = "numeric", length = 929615)

# And now put it all onto Scoring.
scoring$Pr1 <- PrT1
scoring$Pr2 <- PrT2
scoring$Pr3 <- PrT3
scoring$Pr4 <- PrT4
scoring$Pr5 <- PrT5
scoring$Pr6 <- PrT6
scoring$Pr7 <- PrT7
scoring$Pr8 <- PrT8
scoring$Pr9 <- PrT9
scoring$Pr10 <- PrT10
scoring$Pr11 <- PrT11
scoring$Pr12 <- PrT12
scoring$Pr13 <- PrT13
scoring$Pr14 <- PrT14
scoring$Pr15 <- PrT15
scoring$Pr16 <- PrT16
scoring$Pr17 <- PrT17
scoring$Pr18 <- PrT18
scoring$Pr19 <- PrT19
scoring$Pr20 <- PrT20
scoring$Pr21 <- PrT21
scoring$Pr22 <- PrT22
scoring$Pr23 <- PrT23
scoring$Pr24 <- PrT24

# Note to self: this would be a good place to save off the workspace.

# Clean up workspace before running the last heavy computation.
rm(model13_T12); rm(model13_T13); rm(model13_T14); rm(model13_T18); rm(model13_T19); rm(model13_T20); rm(model13_T22); rm(model13_T23); rm(model13_T24); rm(model13_T3); rm(model13_T5); rm(model13_T8); rm(model13_T9)
rm(list=ls(pattern='^PrT'))
rm(list=ls(pattern = '^training_seg'))

# got the next section from ksb5and6and7.R which was already working on my earlier non-regression models.
scoring_copy <- scoring
# Overwrite Product Probabilities to zero if the customer already has the product in Month 2016-05.
scoring_copy$Pr1 <- (1-scoring_copy$P1)*scoring_copy$Pr1
scoring_copy$Pr2 <- (1-scoring_copy$P2)*scoring_copy$Pr2
scoring_copy$Pr3 <- (1-scoring_copy$P3)*scoring_copy$Pr3
scoring_copy$Pr4 <- (1-scoring_copy$P4)*scoring_copy$Pr4
scoring_copy$Pr5 <- (1-scoring_copy$P5)*scoring_copy$Pr5
scoring_copy$Pr6 <- (1-scoring_copy$P6)*scoring_copy$Pr6
scoring_copy$Pr7 <- (1-scoring_copy$P7)*scoring_copy$Pr7
scoring_copy$Pr8 <- (1-scoring_copy$P8)*scoring_copy$Pr8
scoring_copy$Pr9 <- (1-scoring_copy$P9)*scoring_copy$Pr9
scoring_copy$Pr10 <- (1-scoring_copy$P10)*scoring_copy$Pr10
scoring_copy$Pr11 <- (1-scoring_copy$P11)*scoring_copy$Pr11
scoring_copy$Pr12 <- (1-scoring_copy$P12)*scoring_copy$Pr12
scoring_copy$Pr13 <- (1-scoring_copy$P13)*scoring_copy$Pr13
scoring_copy$Pr14 <- (1-scoring_copy$P14)*scoring_copy$Pr14
scoring_copy$Pr15 <- (1-scoring_copy$P15)*scoring_copy$Pr15
scoring_copy$Pr16 <- (1-scoring_copy$P16)*scoring_copy$Pr16
scoring_copy$Pr17 <- (1-scoring_copy$P17)*scoring_copy$Pr17
scoring_copy$Pr18 <- (1-scoring_copy$P18)*scoring_copy$Pr18
scoring_copy$Pr19 <- (1-scoring_copy$P19)*scoring_copy$Pr19
scoring_copy$Pr20 <- (1-scoring_copy$P20)*scoring_copy$Pr20
scoring_copy$Pr21 <- (1-scoring_copy$P21)*scoring_copy$Pr21
scoring_copy$Pr22 <- (1-scoring_copy$P22)*scoring_copy$Pr22
scoring_copy$Pr23 <- (1-scoring_copy$P23)*scoring_copy$Pr23
scoring_copy$Pr24 <- (1-scoring_copy$P24)*scoring_copy$Pr24
head(scoring_copy, n=10L)

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
    temp <- c(df[i,48], df[i,49], df[i,50], df[i,51],  df[i,52],  df[i,53],  df[i,54],  df[i,55],  df[i,56],  df[i,57],  df[i,58],  df[i,59],  df[i,60],  df[i,61],  df[i,62],  df[i,63],  df[i,64],  df[i,65],  df[i,66],  df[i,67],  df[i,68],  df[i,69],  df[i,70],  df[i,71])
    output[i] <- sort.recommendations(temp)
  }
  return(output)
}

# Create the added_products column.
scoring_copy$added_products <- get.ordered.recommendations(scoring_copy)

# Take a look...
head(scoring_copy, n=2L)

# Write to my_solution.csv.
# grab just ncodpers and added_products
my_solution <- data.frame(scoring_copy$ncodpers, scoring_copy$added_products, stringsAsFactors = FALSE)
write.csv(my_solution, file = "../ksb_model13_solution_temp.csv", row.names = FALSE, quote = FALSE) 

# Note to self: save the workspace one more time (model13_finished_workspace)

# Don't forget the final clean-up step using postprocess_mysolutioncsv.awk!!


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

