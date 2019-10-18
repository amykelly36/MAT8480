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

####### Functions above this line #########


###########################################
######## Scoring (Balanced Data) ##########
###########################################

# Save essential parameters and model(s) into .RData
# save(Mean, reg.bwd, ScaleParams, file="myscore.RData")  # Include at end of training script

load("myscore.RData")


# Create scoring function
scoring<- function(data, Mean, reg.bwd, ScaleParams) { # data: data for scoring
  raw<-data # save an original copy
  
  ### Now, follow in training process' footsteps ###
  
  ###################
  ### Preparation ###
  ###################
  
  indx <- sapply(data, is.factor) 
  indx[c("StatusCat96NK","DemGender","DemHomeOwner")]<-c(FALSE,FALSE,FALSE)
  data[c("DemMedHomeValue", "DemMedIncome")]<-sapply(data[c("DemMedHomeValue", "DemMedIncome")], 
                                                     function(x) gsub("\\,", "", x))
  data[indx] <- sapply(data[indx], function(x) as.numeric(gsub("\\$", "", x)))
  
  data$TargetB<-as.factor(data$TargetB)
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
  vars<--grep("^(ID|TargetD)", names(data.mdl)) # exclude rejected vars

  reg.bwd.prob<-predict(reg.bwd,data.mdl[, vars], type = "response")
  reg.bwd.class <- as.factor(ifelse(reg.bwd.prob > 0.5, 1, 0))

  raw[c("pred.prob","pred.class")] <- data.frame(reg.bwd.prob, reg.bwd.class)
  raw
  
  ##################
  ###### ANN #######
  ##################
  
  data.ann<-data.imp

  vars.ann<-attr(terms(reg.bwd), "term.labels") # extract variable names from bwd model
  vars.ann<-c(vars.ann,"StatusCat96NK")

  ## Standardization: numeric inputs ##
  library(caret)
  data.ann[vars.ann]<-predict(ScaleParams, data.ann[vars.ann])

  ## Dummy Encoding: nominal inputs ##
  dummy <- dummyVars( ~ ., data = data.ann[vars.ann], fullRank = TRUE)
  data.ann.encode<-as.data.frame(predict(dummy,  data.ann[vars.ann]))

  ## Prepare score set as matrix ##
  x.valid<-as.matrix(data.ann.encode)

  library(keras)
  ann.select <-load_model_hdf5("my_ann.h5")
  ann.prob<-predict_proba(ann.select,x.valid)
  ann.class<-as.factor(predict_classes(ann.select,x.valid))

  raw[c("pred.prob","pred.class")] <- data.frame(ann.prob, ann.class)
  raw
}



############### Model Implementation ##############

data.score<-read.csv("scorepva97nk.csv", header=TRUE, na.strings=c(".", "NA", "", "?"))
summary(data.score)

data.score.pred<- scoring(data.score, Mean, reg.bwd, ScaleParams)
summary(data.score.pred)

write.csv(data.score.pred, file = "pva97nk_scored.csv",row.names=FALSE)









