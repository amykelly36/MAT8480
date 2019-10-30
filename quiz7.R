############################
####### Quiz Seven #########
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


###############################
####### Decision Tree #########
###############################

vars <- -grep('^(ID|TargetAmt|DemCluster$)', names(organics)) 

## part 1 ##

prop<-1/prop.table(table(organics$TargetBuy))
costMatrix <-matrix(c(0,prop[2],prop[1],0), nrow=2) 
costMatrix

library(rpart)
DT.001<- rpart(formula = TargetBuy ~ .,data = organics[split,vars],
               parms=list(loss=costMatrix),
               control=rpart.control(cp=0.001))

library(caret)
library(pROC)
cp.seq=DT.001$cptable[,1]
fscore<-numeric(0)
fscore[1]<-0
for (i in 2:length(cp.seq)) {
  tree.prob = predict(prune(DT.001, cp=cp.seq[i]), organics[!split,vars],type="prob")[,2] 
  rocCurve.tree <- roc(organics[!split,]$TargetBuy, tree.prob, quiet=TRUE)
  treeThresh <-  coords(rocCurve.tree, x = "best", best.method = "closest.topleft", transpose = FALSE)
  tree.class <- as.factor(ifelse(tree.prob >= treeThresh$threshold, 1,0))
  fscore[i]<-confusionMatrix(table(tree.class,organics[!split,vars]$TargetBuy),
                             positive = "1")$byClass["F1"]
}


plot(DT.001$cptable[,'nsplit']+1,fscore,
     type="o", xlab="Number of Leaves", ylab="F-score")


DT.001$cptable[which(fscore==max(fscore)),] # number of splits in the optimal tree
max(fscore) # max F-score=0.577435


# Final model
tree.final=prune(DT.001,cp=cp.seq[fscore==max(fscore)])  
library(partykit)
plot(as.party(tree.final))




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




#####################################
####### Logistic Regression #########
#####################################

organics.mdl<-organics.imp

vars <- - grep('^(ID|TargetAmt|DemCluster$)', names(organics.mdl)) 


full = glm(TargetBuy ~., family=binomial, data=organics.mdl[split, vars])
summary(full)

null<-glm(TargetBuy ~1, family=binomial, data=organics.mdl[split, vars])

# Stepwise selection
reg.step <- step(null, scope=formula(full), direction="both", trace = FALSE)
summary(reg.step)


## part 2 ##

# Validation F-score
reg.step.prob<-predict(reg.step,organics.mdl[!split, vars], type = "response") 

rocCurve.reg <- roc(organics[!split,]$TargetBuy, reg.step.prob, quiet = TRUE)
regThresh <-  coords(rocCurve.reg, x = "best", best.method = "closest.topleft", transpose = FALSE)
regThresh
reg.class <- as.factor(ifelse(reg.step.prob >= regThresh$threshold, 1,0))
reg.fscore<-confusionMatrix(table(reg.class,organics[!split,vars]$TargetBuy),
                            positive = "1")$byClass["F1"]

reg.fscore  # best f-score=0.561067




#####################
####### ANN  ########
#####################

organics.ann<-organics.imp

vars.ann<-attr(terms(reg.step), "term.labels") # extract variable names from bwd model
vars.ann<-c(vars.ann, "PromSpend")


## Standardization ## 
library(caret)
ScaleParams <- preProcess(organics.ann[split, vars.ann], method=c("center", "scale"))
organics.ann[vars.ann]<-predict(ScaleParams, organics.ann[vars.ann])

## Hot encoding ##
str(organics.ann)
dummy <- dummyVars( ~ ., data = organics.ann[split, vars.ann], fullRank = TRUE)
organics.ann.encode<-as.data.frame(predict(dummy, newdata = organics.ann[vars.ann])) 
organics.ann.encode$TargetBuy<-organics.ann$TargetBuy


inp.n <- grep("^(TargetBuy)", names(organics.ann.encode)) 

x.train <- as.matrix(organics.ann.encode[split,-inp.n])
y.train<- as.matrix(organics.ann.encode[split,"TargetBuy"])
x.valid<-as.matrix(organics.ann.encode[!split,-inp.n])
y.valid<-as.matrix(organics.ann.encode[!split,"TargetBuy"])


library(keras)
use_session_with_seed(42)

ann <- keras_model_sequential() 
ann %>% 
  layer_dense(units = 4, activation = "tanh", input_shape = c(8)) %>% 
  layer_dense(units = 1, activation = "sigmoid")



ann %>% compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = c("accuracy")
)

callbacks.list = list(
  callback_model_checkpoint(filepath="my_ann_ex7.h5", 
                            monitor = "val_loss", 
                            save_best_only = TRUE, 
  ))


history <- ann %>% fit(
  x= x.train,
  y= y.train,
  epochs = 40,
  validation_data = list(x.valid,y.valid),
  verbose = 1,
  callbacks = callbacks.list
)



ann.select<- load_model_hdf5("my_ann_ex7.h5") 

## part 3 ##

## Prediction ##
ann.prob<-predict_proba(ann.select,x.valid)

rocCurve.ann <- roc(organics[!split,]$TargetBuy, ann.prob, quiet=TRUE)
annThresh <-  coords(rocCurve.ann, x = "best", best.method = "closest.topleft", transpose = FALSE)
ann.class <- as.factor(ifelse(ann.prob >= annThresh$threshold, 1,0))
ann.fscore<-confusionMatrix(table(ann.class,organics[!split,vars]$TargetBuy),
                            positive = "1")$byClass["F1"]

ann.fscore  # best f-score=0.5838115

confusionMatrix(table(ann.class,organics[!split,vars]$TargetBuy),
                positive = "1")


##################################
############ Scoring #############
##################################

## part 5 ##


save(Mean, Mode, reg.step, ScaleParams,annThresh, file="myscore_organics.RData")
load("myscore_organics.RData")

scoring<- function(organics,Mean, Mode, reg.step, ScaleParams,annThresh) {
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


  #####################
  ####### ANN  ########
  #####################
  
  organics.ann<-organics.imp
  
  vars.ann<-attr(terms(reg.step), "term.labels") # extract variable names from bwd model
  vars.ann<-c(vars.ann, "PromSpend")
  
  
  ## Standardization ## 
  library(caret)
  organics.ann[vars.ann]<-predict(ScaleParams, organics.ann[vars.ann])
  
  ## Hot encoding ##
  dummy <- dummyVars( ~ ., data = organics.ann[vars.ann], fullRank = TRUE)
  organics.ann.encode<-as.data.frame(predict(dummy, newdata = organics.ann[vars.ann])) 
  
  
  x.valid<-as.matrix(organics.ann.encode)
  
  
  ann.select<- load_model_hdf5("my_ann_ex7.h5") 
  
  ## Prediction ##
  ann.prob<-predict_proba(ann.select,x.valid)
  ann.class <- as.factor(ifelse(ann.prob >= annThresh$threshold, 1,0))
  
  raw[c("pred.prob","pred.class")] <- data.frame(ann.prob, ann.class)
  raw
  }


organics.score <-read.csv("scoreorganics.csv", header=TRUE, na.strings=c(".", "NA", "", "?"))

organics.score.pred<-scoring(organics.score, Mean, Mode, reg.step, ScaleParams,annThresh)

summary(organics.score.pred$pred.class)
prop.table(table(organics.score.pred$pred.class))






