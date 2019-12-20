# DM Project Final Model Scoring

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

library(tidyverse)



# Scoring Dataset

############ Scoring #############

load("myscore_projfinalmodel.RData")

scoring<- function(model_data,Mean, Mode, RF) {
  raw<-model_data
  ##########################
  #### Data Preparation ####
  ##########################
  
  # Past_Trips
  levels(model_data$Past_Trips)<-c("0","1","2")
  model_data$Past_Trips<-sort(model_data$Past_Trips)
  model_data$Past_Trips<-ordered(model_data$Past_Trips,levels=c(0,1,2))
  
  
  # Email
  levels(model_data$Email)<-c("A","B","U")
  
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
  levels(model_data$TourCode) = c("7", "5", "9", "1", "3", "6", "6", "6", "4", "6", "3", "7", "1", "3", "1", "7", 
                                  "7", "6", "6", "4", "7", "6", "4", "6", "3", "6", "2", "7", "5", "3", "5", "9", 
                                  "5", "8", "9", "6", "9", "4", "6", "6", "3", "3", "3", "4", "6", "4", "5", "4", 
                                  "5", "4", "9", "6", "4", "4", "7", "9", "4", "1", "6", "5", "7", "4", "3", "5", 
                                  "5", "8", "7", "5", "6", "5", "5", "5", "7", "3", "5", "4", "1", "6", "8", "7", 
                                  "6", "9","7", "7", "5", "6", "1", "7", "1", "6", "1", "1", "2", "1", "1", "1", "9", "2", "4", "6", "5", "8", "2", "3", "2", "3", "1", "1", "1", "1", "1", "1", "7", "1", "1", "1", "2", "8", "8", "5", "1", "3", "4", "1", "4", "2", "6", "1")
  
  # Recommend_GAT
  model_data$Recommend_GAT[model_data$Recommend_GAT==0] = NA
  model_data$Recommend_GAT = as.factor(model_data$Recommend_GAT)
  
  # TravelAgain
  key.TravelAgain <- c(`1` = "1",
                       `2` = "2") 
  
  
  model_data$TravelAgain[model_data$TravelAgain==0] = NA
  model_data$TravelAgain = as.factor(model_data$TravelAgain)
  
  model_data <- model_data %>%
    mutate(TravelAgain = recode(TravelAgain, !!!key.TravelAgain,
                                .default = NA_character_))    
  
  
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
  
  #################################
  ### Variables Being used ########
  #################################
  
  input_vars <- grep('^(EvalID|Email|Book_12Mo|TourCode|Past_Trips|TourPriceCat|DB_Enter_Months|Tour_Region|Overall_Impression|Hotels_Excellent_or_Good|Optionals_Excellent_or_Good|Pre_Departure|Pax_Category|Hotels_Avg|Book_Months|Meals_Avg|Reference|Meals_Avg|Meals_2orUnder|Capacity|Grp_Size$|Meals_Excellent_or_Good|Hotel_2orUnder|Optionals$|TravelAgain|Hour_Intr_Arrival_Time)', names(model_data))
  
  model_data<-model_data[input_vars]
  
  #############################
  ####### Imputation  #########
  #############################
  
  model_data.imp<-model_data
  
  ######## Routine: Update Input Info ########
  inp.n <- grep("^(EvalID)", names(model_data.imp))
  inx3<-inx(model_data.imp, inp.n) 
  indx<-inx3$indx
  index.cat<-inx3$index.cat
  index.na<-inx3$index.na
  ############################################
  
  # Numeric Input: By Mean #
  model_data.imp[indx]<-as.data.frame(mapply(impute,x=model_data.imp[indx],y = Mean))
  
  # Nominal Input: By Mode #
  model_data.imp[index.cat]<-as.data.frame(mapply(impute,x=model_data.imp[index.cat],y = Mode))
  
  # create missing value flag #
  model_data.imp[paste(names(model_data)[index.na], "NA", sep=".")] <- ifelse(
    is.na(model_data[index.na]), 1, 0)
  
  model_data.imp[grep("NA$",names(model_data.imp))]<-lapply(
    model_data.imp[grep("NA$",names(model_data.imp))], as.factor) 
  
  
  ########################
  ##### Random Forest ####
  ########################
  
  model_data.rf<-model_data.imp
  vars1<--grep("^(EvalID)", names(model_data.rf))
  
  # Make predictions #
  library(caret)
  library(randomForest)
  RF.class<- predict(RF, newdata=model_data.rf, type="response")
  RF.prob<- predict(RF, newdata=model_data.rf, type="prob")[2]
  
  raw[c("pred.prob","pred.class")] <- data.frame(RF.prob, RF.class)
  raw
}


model_data.score <-read.csv("C:/Users/cherr/Desktop/SASUniversityEdition/MAT 8480 data mining/scoring_data.csv", header=TRUE, na.strings=c(".", "NA", "", "?"))

model_data.score.pred<-scoring(model_data.score, Mean, Mode, RF.final)

summary(model_data.score.pred$pred.class)
# 0    1 
# 7677 2377

prop.table(table(model_data.score.pred$pred.class))

#write.csv(model_data.score.pred, file = "C:/Users/cherr/Documents/Project_Final_Scoring_Data.csv",row.names=FALSE)

