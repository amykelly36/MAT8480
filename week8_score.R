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

####### Functions above this line ###########


#############################################
######## Scoring (class imbalance) ##########
#############################################

load("myscore_raw.RData")

# Create scoring function
scoring<- function(data, Mean, reg.bwd, regThresh) {
  raw<-data
  
  ###################
  ### Preparation ###
  ###################
  indx <- sapply(data, is.factor) 
  indx[c("StatusCat96NK","DemGender","DemHomeOwner")]<-c(FALSE,FALSE,FALSE)
  data[indx]<-sapply(data[indx], function(x) gsub("\\,", "", x))  
  data[indx] <- sapply(data[indx], function(x) as.numeric(gsub("\\$", "", x)))
  
  data$StatusCatStarAll<-as.factor(data$StatusCatStarAll) 
  data$DemCluster<-as.factor(data$DemCluster)
  
  data$DemMedIncome[data$DemMedIncome==0]<-NA
  
  
  ######################
  ### Transformation ###
  ######################
  
  data.xf <- data  # pass on data
  
  # numeric input xform
  vars.xf <- grep("^(GiftCnt|GiftAvg)", names(data.xf))
  data.xf[vars.xf]<-log(data.xf[vars.xf]+1)
  
  
  # nominal input xform
  levels(data.xf$StatusCat96NK)<-c("A", "L", "N", "L", "N", "A")
  
  
  ######################
  ##### Imputation #####
  ######################
  
  data.imp<-data.xf # pass on data
  
  ######## Routine: Update Input Info ########
  inp.n <- grep("^(ID|TargetD|TargetB)", names(data.imp)) 
  inx3<-inx(data.imp, inp.n) 
  indx<-inx3$indx
  index.cat<-inx3$index.cat
  index.na<-inx3$index.na
  ############################################

  # numeric impute: By mean #
  data.imp[indx]<-as.data.frame(mapply(impute,x=data.imp[indx],y = Mean))
  
  # create missing value flag #
  data.imp[paste(names(data.xf)[index.na], "NA", sep=".")] <- ifelse(
    is.na(data.xf[index.na]), 1, 0)
  
  data.imp[grep("NA$",names(data.imp))]<-lapply(
    data.imp[grep("NA$",names(data.imp))], as.factor) 
  
  
  ##################################
  ###### Logistic Regression #######
  ##################################
  
  data.mdl<-data.imp # pass on data
  
  reg.bwd.prob<-predict(reg.bwd,data.mdl, type = "response")
  reg.bwd.class <- as.factor(ifelse(reg.bwd.prob >= regThresh$threshold, "1","0"))
  
  
  raw[c("pred.prob","pred.class")] <- data.frame(reg.bwd.prob, reg.bwd.class)
  raw
}


############### Model Implementation ##############

data.score<-read.csv("pva97nk_raw.csv", header=TRUE, na.strings=c(".", "NA", "", "?"))

data.score.pred<- scoring(data.score, Mean, reg.bwd, regThresh)
summary(data.score.pred)


# confusionMatrix(table(data.score.pred$pred.class[!split],data.score[!split,]$TargetB),
#                             positive = "1")$byClass["F1"]
# reg.fscore  # best f-score=0.1303823

# write.csv(data.score.pred, file = "test.csv",row.names=FALSE)



































