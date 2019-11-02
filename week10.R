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

## Impute function ##
impute <- function(x,y) {
  x[is.na(x)]<-y
  x
}

## Mean function ##
means <- function(x) { mean(x, na.rm = TRUE) }

############# Functions above this line ################



###########################################
######## Week 10   Random Forest ##########
###########################################

data<-read.csv("pva97nk_raw.csv", header=TRUE, na.strings=c(".", "NA", "", "?"))

indx <- sapply(data, is.factor) 
indx[c("StatusCat96NK","DemGender","DemHomeOwner")]<-c(FALSE,FALSE,FALSE)
data[indx]<-sapply(data[indx], function(x) gsub("\\,", "", x))             # modified slightly
data[indx] <- sapply(data[indx], function(x) as.numeric(gsub("\\$", "", x)))

data$TargetB<-as.factor(data$TargetB)
data$StatusCatStarAll<-as.factor(data$StatusCatStarAll) 
data$DemCluster<-as.factor(data$DemCluster)

data$DemMedIncome[data$DemMedIncome==0]<-NA



### Partitioning ###
library(caTools)
set.seed(1234)
split = sample.split(data$TargetB, SplitRatio = 0.5) 

## Under-Sampling ##
library(caret)
vars <- -grep("^(TargetB)", names(data))
downSampledTrain <- downSample(x = data[split ,vars],
                               y = data$TargetB[split],
                               ## keep the class variable name the same:
                               yname = "TargetB")

summary(downSampledTrain)

## Combine downsampled train with validation ##
data.down<-data[(data$ID %in% downSampledTrain$ID) | (!split),]

# Indicator for balanced train data #
split.down<-ifelse(data.down$ID %in% downSampledTrain$ID, TRUE, FALSE)

sum(split.down)


######################
##### Imputation #####
######################

data.imp<-data.down # pass on data

######## Routine: Update Input Info ########
inp.n <- grep("^(ID|TargetD|TargetB)", names(data.imp)) 
inx3<-inx(data.imp, inp.n) 
indx<-inx3$indx
index.cat<-inx3$index.cat
index.na<-inx3$index.na
############################################


# numeric impute: By mean #
Mean<-sapply(data.imp[split.down,indx],means)

data.imp[indx]<-as.data.frame(mapply(impute,x=data.imp[indx],y = Mean))


# create missing value flag #
data.imp[paste(names(data.down)[index.na], "NA", sep=".")] <- ifelse(
  is.na(data.down[index.na]), 1, 0)

data.imp[grep("NA$",names(data.imp))]<-lapply(
  data.imp[grep("NA$",names(data.imp))], as.factor) 




#######################
#### Random Forest ####
#######################

data.rf<-data.imp

vars1<--grep("^(ID|TargetD|DemCluster)", names(data.rf))

library(randomForest)
set.seed(1234)
RF <- randomForest(TargetB ~., data=data.rf[split.down,vars1],
                   ntree = 1000,
                   importance = True)

print(RF)
plot(RF)  # OOB error is OOB MISC (in black)



# Make predictions #
RF.class<- predict(RF, newdata=data.rf[!split.down,], type="response")
fscore<-confusionMatrix(table(RF.class,data.rf[!split.down,]$TargetB),
                        positive = "1")$byClass["F1"]  
fscore

# Variable importance #
RF$importance
varImpPlot(RF)  




#### Parameter Tuning: mtry ####

# m<-round(seq(from=2, to=28, length.out = 5))
m<-seq(2,7)
fscore.seq<-numeric()

for(i in 1:length(m)){ 
  set.seed(1234)
  rf <- randomForest(TargetB ~., data=data.rf[split.down,vars1],
                     ntree=1000, mtry=m[i])
  
  rf.class<- predict(rf, newdata=data.rf[!split.down,], type="response")
  fscore.seq[i]<-confusionMatrix(table(rf.class,data.rf[!split.down,]$TargetB),
                                 positive = "1")$byClass["F1"]
}

plot(m, fscore.seq, pch=19 , col="blue", type="b",
     ylab="F-score",xlab="Number of Predictors considered at each split")


m.best<- m[which.max(fscore.seq)] 
# assign m.best to mtry in the above RF 
# and then pick appropriate ntree



#######################################################################
#######################    Without Under-Sampling   ###################
#######################################################################

######################
##### Imputation #####
######################

data.imp<-data # pass on data

######## Routine: Update Input Info ########
inp.n <- grep("^(ID|TargetD|TargetB)", names(data.imp)) 
inx3<-inx(data.imp, inp.n) 
indx<-inx3$indx
index.cat<-inx3$index.cat
index.na<-inx3$index.na
############################################


# numeric impute: By mean #
Mean<-sapply(data.imp[split,indx],means)

data.imp[indx]<-as.data.frame(mapply(impute,x=data.imp[indx],y = Mean))


# create missing value flag #
data.imp[paste(names(data)[index.na], "NA", sep=".")] <- ifelse(
  is.na(data[index.na]), 1, 0)

data.imp[grep("NA$",names(data.imp))]<-lapply(
  data.imp[grep("NA$",names(data.imp))], as.factor) 



#######################
#### Random Forest ####
#######################

data.rf<-data.imp

vars1<--grep("^(ID|TargetD|DemCluster)", names(data.rf))

minor<-unname(summary(data.rf$TargetB[split])[2])

library(randomForest)
set.seed(1234)
RF <- randomForest(TargetB ~., data=data.rf[split,vars1],
                   ntree = 500, 
                   strata= data.rf$TargetB[split], 
                   sampsize=c(minor,minor),
                   importance =TRUE)
print(RF)
plot(RF)  



# Make predictions #
library(caret)
RF.class<- predict(RF, newdata=data.rf[!split,], type="response")
fscore<-confusionMatrix(table(RF.class,data.rf[!split,]$TargetB),
                        positive = "1")$byClass["F1"]  
fscore


# Variable importance #
RF$importance
varImpPlot(RF)  


















