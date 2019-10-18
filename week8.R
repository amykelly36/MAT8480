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


########################################################
######## Predicted Modeling (Class Imbalance) ##########
########################################################

data<-read.csv("pva97nk_raw.csv", header=TRUE, na.strings=c(".", "NA", "", "?"))

indx <- sapply(data, is.factor) 
indx[c("StatusCat96NK","DemGender","DemHomeOwner")]<-c(FALSE,FALSE,FALSE)
data[indx]<-sapply(data[indx], function(x) gsub("\\,", "", x))             # modified slightly
data[indx] <- sapply(data[indx], function(x) as.numeric(gsub("\\$", "", x)))

data$TargetB<-as.factor(data$TargetB)
data$StatusCatStarAll<-as.factor(data$StatusCatStarAll) 
data$DemCluster<-as.factor(data$DemCluster)

data$DemMedIncome[data$DemMedIncome==0]<-NA



## Partitioning ##
library(caTools)
set.seed(1234)
split = sample.split(data$TargetB, SplitRatio = 0.5) 

summary(data[split,])
summary(data[!split,])


####################################
######### Decision Tree ############
####################################

# Model building
vars <- -grep("^(ID|TargetD)", names(data))

library(rpart)
tree <- rpart(formula = TargetB ~ .,data = data[split,vars]
              ,control=rpart.control(cp=0))
summary(tree)


# # Model pruning on MISC
# cp.seq=tree$cptable[,1]
# misc<-numeric()
# for (i in 1:length(cp.seq)) {
#   tree.predict = predict(prune(tree, cp=cp.seq[i]), data[!split,vars],type="class")
#   cm=table(data[!split,vars]$TargetB, tree.predict) # confusion matrix
#   misc[i]=(cm[1,2]+cm[2,1])/sum(cm) # misclassification rate
# }
# 
# plot(tree$cptable[,'nsplit']+1,misc,
#      type="o", xlab="Number of Leaves", ylab="Misclassification Rate")



# Model pruning on F-score USING alternative cutoff
library(pROC)
library(caret)
cp.seq=tree$cptable[,1]
fscore<-numeric()
fscore[1]<-0  # Set root node F-score zero
for (i in 2:length(cp.seq)) {
  tree.prob = predict(prune(tree, cp=cp.seq[i]), data[!split,vars],type="prob")[,2] 
  rocCurve.tree <- roc(data[!split,]$TargetB, tree.prob, quiet=TRUE)
  treeThresh <-  coords(rocCurve.tree, x = "best", best.method = "closest.topleft", transpose = FALSE)
  tree.class <- as.factor(ifelse(tree.prob >= treeThresh$threshold, 1,0))
  fscore[i]<-confusionMatrix(table(tree.class,data[!split,vars]$TargetB),
                             positive = "1")$byClass["F1"]
}

plot(tree$cptable[,'nsplit']+1,fscore,
     type="o", xlab="Number of Leaves", ylab="F-score")


# Final model
tree.final=prune(tree,cp=cp.seq[fscore==max(fscore)])  # max F-score=0.1165179
library(partykit)
plot(as.party(tree.final))



######################
### Transformation ###
######################

data.xf <- data  # pass on data

######## Routine: Update Input Info ########
inp.n <- grep("^(ID|TargetD|TargetB)", names(data.xf)) # obtain non-input pos.(use current dataframe)
inx3<-inx(data.xf, inp.n) # generate num/factor/missing input indices
indx<-inx3$indx
index.cat<-inx3$index.cat
index.na<-inx3$index.na
############################################

# numeric input xform
library(caret)
TransformParams <- preProcess(data.xf[split,indx], method=c("YeoJohnson"))
TransformParams$yj


vars.xf <- grep("^(GiftCnt|GiftAvg)", names(data.xf))
data.xf[vars.xf]<-log(data.xf[vars.xf]+1)



# nominal input xform
levels(data.xf$StatusCat96NK)
levels(data.xf$StatusCat96NK)<-c("A", "L", "N", "L", "N", "A")



######################
##### Imputation #####
######################

data.imp<-data.xf # pass on data

######## Routine: Update Input Info ########
# inp.n <- grep("^(ID|TargetD|TargetB)", names(data.imp)) # no change
inx3<-inx(data.imp, inp.n) 
indx<-inx3$indx
index.cat<-inx3$index.cat
index.na<-inx3$index.na
############################################

# check missing data
names(data.imp)[index.na==TRUE]


# numeric impute: By mean #
Mean<-sapply(data.imp[split,indx],means)

data.imp[indx]<-as.data.frame(mapply(impute,x=data.imp[indx],y = Mean))


# create missing value flag #
data.imp[paste(names(data.xf)[index.na], "NA", sep=".")] <- ifelse(
  is.na(data.xf[index.na]), 1, 0)

data.imp[grep("NA$",names(data.imp))]<-lapply(
  data.imp[grep("NA$",names(data.imp))], as.factor) 



########################################
######### Logistic Regression ##########
########################################

data.mdl<-data.imp # pass on data
vars<--grep("^(ID|TargetD|GiftTimeFirst.NA)", names(data.mdl)) # exclude rejected vars and GiftTimeFirst.NA
# sum(is.na(data.xf$GiftTimeFirst))
# sum(is.na(data.xf$GiftTimeFirst[split]))

# Build full model
full = glm(TargetB ~., family=binomial, data=data.mdl[split, vars])
summary(full)
n<-sum(split)


# Backward
reg.bwd <- step(full, direction="backward",k=log(n), trace = FALSE)
summary(reg.bwd)
reg.bwd.prob<-predict(reg.bwd,data.mdl[!split, vars], type = "response")

# Use alternative cutoff
rocCurve.reg <- roc(data[!split,]$TargetB, reg.bwd.prob, quiet = TRUE)
regThresh <-  coords(rocCurve.reg, x = "best", best.method = "closest.topleft", transpose = FALSE)
reg.class <- as.factor(ifelse(reg.bwd.prob >= regThresh$threshold, 1,0))
reg.fscore<-confusionMatrix(table(reg.class,data[!split,vars]$TargetB),
                           positive = "1")$byClass["F1"]

reg.fscore  # f-score=0.1303823

confusionMatrix(table(reg.class,data[!split,vars]$TargetB),
                positive = "1", mode= "everything")


########################################
####### Artificial Neural Network ######
########################################


#####################
## ANN Preparation ##
#####################

data.ann<-data.imp


vars.ann<-attr(terms(reg.bwd), "term.labels") # extract variable names from bwd model
vars.ann<-c(vars.ann,"StatusCat96NK")


## Standardization: numeric inputs ## 
library(caret)
ScaleParams <- preProcess(data.ann[split, vars.ann], method=c("center", "scale"))
data.ann[vars.ann]<-predict(ScaleParams, data.ann[vars.ann])


## Dummy Encoding: nominal inputs ##
dummy <- dummyVars( ~ ., data = data.ann[split, vars.ann], fullRank = TRUE)
data.ann.encode<-as.data.frame(predict(dummy,  data.ann[vars.ann])) 
data.ann.encode$TargetB<-data.ann$TargetB



## Prepare train/validation sets as matrices ##
inp.n <- grep("^(TargetB)", names(data.ann.encode)) 

x.train <- as.matrix(data.ann.encode[split,-inp.n])
y.train<- as.matrix(data.ann.encode[split,"TargetB"])
x.valid<-as.matrix(data.ann.encode[!split,-inp.n])
y.valid<-as.matrix(data.ann.encode[!split,"TargetB"])



####################
### ANN Building ###
####################
library(keras)


use_session_with_seed(27)
ann <- keras_model_sequential()
ann %>%
  layer_dense(units = 6, activation = "tanh", input_shape = c(9)) %>%   # update input shape
  layer_dense(units = 6, activation = "tanh") %>%
  layer_dense(units = 1, activation = "sigmoid")



ann %>% compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)



callbacks.list = list(
  callback_early_stopping(
    monitor = "val_loss", # change
    patience = 5
  ),
  callback_model_checkpoint(
    filepath="my_ann_raw.h5",
    monitor = "val_loss",  # change
    save_best_only = TRUE
  )
)



history <- ann %>% fit(
  x= x.train,
  y= y.train,
  epochs = 40,
  validation_data = list(x.valid,y.valid),
  verbose = 1,
  callbacks = callbacks.list
)



ann.select <-load_model_hdf5("my_ann_raw.h5") 


## Prediction ##
ann.prob<-predict_proba(ann.select,x.valid)

# Use alternative cutoff
rocCurve.ann <- roc(data[!split,]$TargetB, ann.prob, quiet=TRUE)
annThresh <-  coords(rocCurve.ann, x = "best", best.method = "closest.topleft", transpose = FALSE)
ann.class <- as.factor(ifelse(ann.prob >= annThresh$threshold, 1,0))
ann.fscore<-confusionMatrix(table(ann.class,data[!split,vars]$TargetB),
                            positive = "1")$byClass["F1"]

ann.fscore  # f-score=0.1275249

confusionMatrix(table(ann.class,data[!split,vars]$TargetB),
                positive = "1", mode= "everything")


##################################
######### Profit Tree ############
##################################

# Model building
vars <- -grep("^(ID|TargetD)", names(data))

prop<-1/prop.table(table(data$TargetB)) # Obtain inverse prior probs

# Define cost matrix
costMatrix <-matrix(c(0,prop[2],prop[1],0), nrow=2) 
costMatrix

library(rpart)
tree <- rpart(formula = TargetB ~ .,data = data[split,vars],
              parms=list(loss=costMatrix),
              control=rpart.control(cp=0.001))



# Model pruning on F-score
library(pROC)
library(caret)
cp.seq=tree$cptable[,1]
fscore<-numeric(0)
fscore[1]<-0
for (i in 2:length(cp.seq)) {
  tree.prob = predict(prune(tree, cp=cp.seq[i]), data[!split,vars],type="prob")[,2] 
  rocCurve.tree <- roc(data[!split,]$TargetB, tree.prob, quiet=TRUE)
  treeThresh <-  coords(rocCurve.tree, x = "best", best.method = "closest.topleft", transpose = FALSE)
  tree.class <- as.factor(ifelse(tree.prob >= treeThresh$threshold, 1,0))
  fscore[i]<-confusionMatrix(table(tree.class,data[!split,vars]$TargetB),
                             positive = "1")$byClass["F1"]
}


plot(tree$cptable[,'nsplit']+1,fscore,
     type="o", xlab="Number of Leaves", ylab="F-score")


# Final model
tree.final=prune(tree,cp=cp.seq[fscore==max(fscore)])  # max F-score=0.121434
library(partykit)
plot(as.party(tree.final))






########################
##### For Scoring ######
########################

save(Mean, reg.bwd,  regThresh, file="myscore_raw.RData")



