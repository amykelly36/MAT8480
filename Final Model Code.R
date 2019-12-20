# RF Dec 13

########################
#### Email Included ####
########################


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

## Mode function ##
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## Mean function ##
means <- function(x) { mean(x, na.rm = TRUE) }

####### Functions above this line ###########


########################
#### Import Dataset ####
########################

model_data = read.csv("C:/Users/cherr/Desktop/SASUniversityEdition/MAT 8480 data mining/modeling_data.csv", 
                      header=TRUE, na.strings = c(".", "NA", "", "?"))


##########################
#### Data Preparation ####
##########################

# Book_12Mo
model_data$Book_12Mo<-as.factor(model_data$Book_12Mo)

# Email
levels(model_data$Email)<-c("A","B","U")

# Past_Trips
levels(model_data$Past_Trips)<-c("0","1","2")
model_data$Past_Trips<-sort(model_data$Past_Trips)
model_data$Past_Trips<-ordered(model_data$Past_Trips,levels=c(0,1,2))

# TourPriceCat
model_data$TourPriceCat<-relevel(model_data$TourPriceCat,"Under 2000")
levels(model_data$TourPriceCat)<-c(1,2,3,4,5,6,7,8)
model_data$TourPriceCat<-sort(model_data$TourPriceCat)
model_data$TourPriceCat<-ordered(model_data$TourPriceCat,levels=c(1,2,3,4,5,6,7,8))

# Hour_Intr_Arrival_Time
model_data$Hour_Intr_Arrival_Time <- as.factor(format(as.POSIXct(model_data$Intr_Arrival_Time,format="%H:%M:%S"),"%H"))
levels(model_data$Hour_Intr_Arrival_Time)<-c("N","N","N","N","M","M","M","M","M","M","M","A","A","A","A","A","E","E","E","E","N","N","N")
sort(model_data$Hour_Intr_Arrival_Time)
model_data$Hour_Intr_Arrival_Time<-factor(model_data$Hour_Intr_Arrival_Time,order=TRUE,levels=c("M","A","E","N"))

# Hotels Rated Excellent or Good - Weighted Average
model_data$Hotels_Excellent_or_Good=((2/3)*model_data$Excellent_Hotels)+((1/3)*model_data$Good_Hotels)

# Meals Rated Excellent or Good - Weighted Average
model_data$Meals_Excellent_or_Good=((2/3)*model_data$Excellent_Meals)+((1/3)*model_data$Good_Meals)

# Optionals Rated Excellent or Good - Weighted Average
model_data$Optionals_Excellent_or_Good=((2/3)*model_data$Excellent_Optionals)+((1/3)*model_data$Good_Optionals)

# GUSS Rated Excellent or Good - Weighted Average
model_data$GUSS_Excellent_or_Good=((2/3)*model_data$Excellent_GUSS)+((1/3)*model_data$Good_GUSS)


# Buses Rated Excellent or Good - Weighted Average
model_data$Buses_Excellent_or_Good=((2/3)*model_data$Excellent_Buses)+((1/3)*model_data$Good_Buses)

# Tour_Region
levels(model_data$Tour_Region) = c("5", "1", "4", "5", "4", "1", "2", "5", "5",  "4", "2", "3", "3", "5", "2", "4", "5", "5", "1", "3", "4")

# TourCode
levels(model_data$TourCode) = c("7", "5", "9", "1", "3", "6", "6", "6", "4", "6", "3", "7", "1", "3", "1", "7", "7", "6", "6", "4", "7", "6", "4", "6", "3", "6", "2", "7", "5", "3", "5", "9", "5", "8", "9", "6", "9", "4", "6", "6", "3", "3", "3", "4", "6", "4", "5", "4", "5", "4", "9", "6", "4", "4", "7", "9", "4", "1", "6", "5", "7", "4", "3", "5", "5", "8", "7", "5", "6", "5", "5", "5", "7", "3", "5", "4", "1", "6", "8", "7", "6", "7", "7", "5", "6", "1", "7", "1", "6", "1", "1", "2", "1", "1", "1", "9", "2", "4", "6", "5", "8", "2", "3", "2", "3", "1", "1", "1", "1", "1", "1", "7", "1", "1", "1", "2", "8", "8", "5", "1", "3", "4", "1", "4", "2", "6", "1")

# Recommend_GAT
model_data$Recommend_GAT[model_data$Recommend_GAT==0] = NA
model_data$Recommend_GAT = as.factor(model_data$Recommend_GAT)

# TravelAgain
model_data$TravelAgain[model_data$TravelAgain==0] = NA
model_data$TravelAgain = as.factor(model_data$TravelAgain)

# Groups_Interest
model_data$Groups_Interest[model_data$Groups_Interest==0] = NA
model_data$Groups_Interest = as.factor(model_data$Groups_Interest)

# Reference
model_data$Reference[model_data$Reference==0] = NA
model_data$Reference = as.factor(model_data$Reference)

# Overall_Impression
model_data$Overall_Impression[model_data$Overall_Impression==0] = NA
model_data$Overall_Impression<-ordered(model_data$Overall_Impression,levels=c(1,2,3,4,5))

# Pre_Departure
model_data$Pre_Departure[model_data$Pre_Departure==0] = NA
model_data$Pre_Departure<-ordered(model_data$Pre_Departure,levels=c(1,2,3,4,5))

# Flight_Itin
model_data$Flight_Itin[model_data$Flight_Itin==0] = NA

# TD_Overall
model_data$TD_Overall[model_data$TD_Overall==0] = NA

# Hotels_Avg
model_data$Hotels_Avg[model_data$Hotels_Avg<1] = NA

# Meals_Avg
model_data$Meals_Avg[model_data$Meals_Avg<1] = NA

# GUSS_Avg
model_data$GUSS_Avg[model_data$GUSS_Avg<1] = NA

# Optionals_Avg
model_data$Optionals_Avg[model_data$Optionals_Avg<1] = NA

# Bus_Avg
model_data$Bus_Avg[model_data$Bus_Avg<1] = NA



######################
#### Partitioning ####
######################

library(caTools)
set.seed(69012) #set.seed(69012)
split=sample.split(model_data$Book_12Mo,SplitRatio=0.5)
split.valid<-!split
split.test<-!split
split2<-sample.split(model_data$Book_12Mo[!split],SplitRatio=0.5)
split.valid[split.valid==TRUE]=split2
split.test[split.test==TRUE]=!split2





#############################################
#### Input Variables that are Being Used ####
#############################################
model_data2<-model_data # make this so only the top 20 variables are used in this dataset and the target variable
#model_data<-read.csv("/Users/jenniferminor/Data Mining/Project/modeling_data.csv",header=TRUE, na.strings=c(".", "NA", "", "?"))

# included EvalID since it is the unqiue ID variable. Not being used as a predictor
input_vars <- grep('^(EvalID|Email|Book_12Mo|Past_Trips|TourPriceCat|DB_Enter_Months|Tour_Region|Overall_Impression|Hotels_Excellent_or_Good|Optionals_Excellent_or_Good|Pre_Departure|Pax_Category|Hotels_Avg|Book_Months|TourCode|Meals_Avg|Reference|Meals_Avg|Meals_2orUnder|Capacity|Grp_Size$|Meals_Excellent_or_Good|Hotel_2orUnder|Optionals$|TravelAgain|Hour_Intr_Arrival_Time)', names(model_data))
model_data_inputs<-model_data[input_vars]


model_data<-model_data2[input_vars]

str(model_data)

######################
##### Imputation #####
######################

model_data.imp<-model_data # pass on data

######## Routine: Update Input Info ########
inp.n <- grep("^(EvalID|Book_12Mo)", names(model_data.imp)) # no change
inx3<-inx(model_data.imp, inp.n) 
indx<-inx3$indx
index.cat<-inx3$index.cat
index.na<-inx3$index.na
############################################


# check missing data
names(model_data.imp)[index.na==TRUE] # eight missing variables


# numeric impute: By mean #
Mean<-sapply(model_data.imp[split,indx],means)

model_data.imp[indx]<-as.data.frame(mapply(impute,x=model_data.imp[indx],y = Mean))



# nominal impute: By mode #
Mode<-sapply(model_data.imp[split, index.cat],mode)
model_data.imp[index.cat]<-as.data.frame(mapply(impute,x=model_data.imp[index.cat],y = Mode))


# create missing value flag #
model_data.imp[paste(names(model_data)[index.na], "NA", sep=".")] <- ifelse(
  is.na(model_data[index.na]), 1, 0)

model_data.imp[grep("NA$",names(model_data.imp))]<-lapply(
  model_data.imp[grep("NA$",names(model_data.imp))], as.factor) 







################################
##### Random Forest ############
################################

model_data.rf<-model_data.imp
str(model_data.rf)


vars1<--grep("^(EvalID)", names(model_data.rf))
minor <-unname(summary(model_data.rf$Book_12Mo[split])[2])

library(caret)
library(pROC)

library(randomForest)
set.seed(69012)
RF.final<-randomForest(Book_12Mo ~., data=model_data.rf[split,vars1],
                       ntree = 500,
                       strata = model_data.rf$Book_12Mo[split],
                       sampsize = c(minor, minor),
                       mtry=19)
RF.final.class<- predict(RF.final, newdata=model_data.rf[split.valid,], type="response")
fscore<-confusionMatrix(table(RF.final.class,model_data.rf[split.valid,]$Book_12Mo),
                        positive = "1")$byClass["F1"]
fscore #0.8915942

varImp(RF.final)
varImpPlot(RF.final)


str(model_data.rf$Overall_Impression)
str(model_data.rf$Pre_Departure)



save(Mean, Mode, RF.final, file="myscore_projfinalmodel.RData")







############### RF.final is our final model #################
### The code below is the code used to transform the RF into a DT for interpretation








#############################################################
#### Make a DT based on the Final Random Forest Model #######
#############################################################

# First make a predicted into a target variable.
RF.final.predict_class<- predict(RF.final, newdata=model_data.rf, type="response")
str(RF.final.predict_class)
model_data_DT<-cbind.data.frame(model_data.rf,RF.final.predict_class)

# Split the Data based on the predicted target variable
library(caTools)
set.seed(69012)
split_DT=sample.split(model_data_DT$RF.final.predict_class,SplitRatio=0.5)
split.valid_DT<-!split_DT
split.test_DT<-!split_DT
split.valid_DT<-sample.split(model_data_DT$RF.final.predict_class[!split_DT],SplitRatio=0.5)
split.valid_DT[split.valid_DT==TRUE]=split.valid_DT
split.test_DT[split.test_DT==TRUE]=!split.valid_DT

vars_DT<--grep("^(EvalID|Book_12Mo)", names(model_data_DT))


##########################################
#### Forest into DT: Based on Accuracy ###
##########################################

# to get accuracy do 1 - misclassification rate

library(rpart)
set.seed(69012)
tree <- rpart(formula = RF.final.predict_class ~ .,data = model_data_DT[split_DT,vars_DT]
              ,control=rpart.control(cp=0))

cp.seq=tree$cptable[,1]
misc<-numeric()
acc<-numeric()
for (i in 1:length(cp.seq)) {
  tree.predict = predict(prune(tree, cp=cp.seq[i]), model_data_DT[split.valid_DT,vars_DT], type="class")
  cm=table(tree.predict,model_data_DT[split.valid_DT,vars_DT]$RF.final.predict_class)
  misc[i]<-(cm[1,2]+cm[2,1])/sum(cm)
  acc[i]<-1-misc[i]
}

plot(tree$cptable[,'nsplit']+1,acc,
     type="o", xlab="Number of Leaves", ylab="Accuracy",main="Random Forest Explained by Decision Tree")

tree$cptable[,'nsplit']+1
acc

### Final model ###

tree.final=prune(tree,cp=cp.seq[15])
tree.final$cptable
max(acc) #0.9722
acc
tree$cptable
library (partykit)

