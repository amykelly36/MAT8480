############################
####### Quiz Eight #########
############################

## Function: return updated input indices ##
inx <- function (data, inp.n) { # data: current dataframe; inp.n: position for non-inputs
  # numeric input indicator
  indx <- sapply(data, is.numeric)
  indx[inp.n]<-FALSE
  
  # nominal input indicator
  index.cat<-sapply(data, is.factor)
  index.cat[inp.n]<-FALSE
  
  # missing value indicator
  index.na<-sapply(data,function(x) any(is.na(x)))
  index.na[inp.n]<-FALSE
  
  data.frame(indx, index.cat, index.na)
}

impute <- function(x,y) {
  x[is.na(x)]<-y
  x
}

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

means <- function(x) { mean(x, na.rm = TRUE) }

############# Functions above this line ###################

organics<-read.csv("organics.csv", header=TRUE, na.strings=c(".", "NA", "", "?"))

organics$ID<-as.factor(organics$ID)
organics$DemCluster<-as.factor(organics$DemCluster)
organics$TargetBuy<-as.factor(organics$TargetBuy)

library(caTools)
set.seed(4321)
split = sample.split(organics$TargetBuy, SplitRatio = 0.5)  



#################################
####### Transformation  #########
#################################

organics.xf<-organics

vars.xf<-c("PromTime", "PromSpend")
organics.xf[vars.xf]<-log(organics.xf[vars.xf]+1)



#############################
####### Imputation  #########
#############################

organics.imp<-organics.xf

######## Routine: Update Input Info ########
inp.n <- grep('^(ID|TargetAmt|DemCluster$|TargetBuy)', names(organics.imp)) 
inx3<-inx(organics.imp, inp.n)
indx<-inx3$indx
index.cat<-inx3$index.cat
index.na<-inx3$index.na
#########################################


# Numeric Input: By Mean #
Mean<-sapply(organics.imp[split,indx],means)

organics.imp[indx]<-as.data.frame(mapply(impute,x=organics.imp[indx],y = Mean))


# Nominal Input: By Mode #
Mode<-sapply(organics.imp[split, index.cat],mode)
organics.imp[index.cat]<-as.data.frame(mapply(impute,x=organics.imp[index.cat],y = Mode))


# Create Missing Value Flag #
organics.imp[paste(names(organics)[index.na], "NA", sep=".")] <- ifelse(
  is.na(organics[index.na]), 1, 0)

organics.imp[grep("NA$",names(organics.imp))]<-lapply(
  organics.imp[grep("NA$",names(organics.imp))], as.factor) 



#######################
#### Random Forest ####
#######################

## part 1 ##
organics.rf<-organics.imp
vars <- - grep('^(ID|TargetAmt|DemCluster$)', names(organics.rf)) 

minor<-unname(summary(organics.rf$TargetBuy[split])[2])

library(randomForest)
set.seed(4321)
RF <- randomForest(TargetBuy ~., data=organics.rf[split,vars],
                   ntree = 201,
                   strata= organics.rf$TargetBuy[split], 
                   sampsize=c(minor,minor),
                   importance = TRUE)


# Make predictions #
library(caret)
RF.class<- predict(RF, newdata=organics.rf[!split,], type="response")
fscore<-confusionMatrix(table(RF.class,organics.rf$TargetBuy[!split]),
                        positive = "1")$byClass["F1"]   
fscore


# Variable importance #
RF$importance
varImpPlot(RF)  



## part 2 ##

#### Parameter Tuning: mtry ####
m<-seq(2,5)
fscore.seq<-numeric()

for(i in 1:length(m)){ 
  set.seed(4321)
  rf <- randomForest(TargetBuy ~., data=organics.rf[split,vars],
                     ntree = 201,
                     strata= organics.rf$TargetBuy[split], 
                     sampsize=c(minor,minor),
                     mtry=m[i])
  
  rf.class<- predict(rf, newdata=organics.rf[!split,], type="response")
  fscore.seq[i]<-confusionMatrix(table(rf.class,organics.rf$TargetBuy[!split]),
                                 positive = "1")$byClass["F1"]
}

plot(m, fscore.seq, pch=19 , col="blue", type="b",
     ylab="F-score",xlab="Number of Predictors considered at each split")


m.best<- m[which.max(fscore.seq)] 
max(fscore.seq)




## part 3 ##
set.seed(4321)
RF.final <- randomForest(TargetBuy ~., data=organics.rf[split,vars],
                         ntree = 201,
                         strata= organics.rf$TargetBuy[split], 
                         sampsize=c(minor,minor),
                         mtry=m.best)


############ Scoring #############

save(Mean, Mode, RF.final, file="myscore_organics2.RData")
load("myscore_organics2.RData")

scoring<- function(organics,Mean, Mode, RF) {
  raw<-organics
  
  organics$ID<-as.factor(organics$ID)
  organics$DemCluster<-as.factor(organics$DemCluster)
  
  #################################
  ####### Transformation  #########
  #################################
  
  organics.xf<-organics
  
  vars.xf<-c("PromTime", "PromSpend")
  organics.xf[vars.xf]<-log(organics.xf[vars.xf]+1)
  
  
  #############################
  ####### Imputation  #########
  #############################
  
  organics.imp<-organics.xf
  
  ######## Routine: Update Input Info ########
  inp.n <- grep('^(ID|DemCluster$)', names(organics.imp)) 
  inx3<-inx(organics.imp, inp.n)
  indx<-inx3$indx
  index.cat<-inx3$index.cat
  index.na<-inx3$index.na
  #########################################
  
  # Numeric Input: By Mean #
  organics.imp[indx]<-as.data.frame(mapply(impute,x=organics.imp[indx],y = Mean))
  
  # Nominal Input: By Mode #
  organics.imp[index.cat]<-as.data.frame(mapply(impute,x=organics.imp[index.cat],y = Mode))
  
  # Create Missing Value Flag #
  organics.imp[paste(names(organics)[index.na], "NA", sep=".")] <- ifelse(
    is.na(organics[index.na]), 1, 0)
  
  organics.imp[grep("NA$",names(organics.imp))]<-lapply(
    organics.imp[grep("NA$",names(organics.imp))], as.factor) 
  
  
  ########################
  ##### Random Forest ####
  ########################
  
  organics.rf<-organics.imp
  
  # Make predictions #
  library(caret)
  RF.class<- predict(RF, newdata=organics.rf, type="response")
  RF.prob<- predict(RF, newdata=organics.rf, type="prob")[2]
  
  raw[c("pred.prob","pred.class")] <- data.frame(RF.prob, RF.class)
  raw
}


organics.score <-read.csv("scoreorganics.csv", header=TRUE, na.strings=c(".", "NA", "", "?"))

organics.score.pred<-scoring(organics.score, Mean, Mode, RF.final)

summary(organics.score.pred$pred.class)
prop.table(table(organics.score.pred$pred.class))






