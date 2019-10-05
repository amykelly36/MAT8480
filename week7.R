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

############# Functions above this line ##############

data<-read.csv("pva97nko.csv", header=TRUE, na.strings=c(".", "NA", "", "?"))

indx <- sapply(data, is.factor) 
indx[c("StatusCat96NK","DemGender","DemHomeOwner")]<-c(FALSE,FALSE,FALSE)
data[c("DemMedHomeValue", "DemMedIncome")]<-sapply(data[c("DemMedHomeValue", "DemMedIncome")], 
                                                   function(x) gsub("\\,", "", x))
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
tree <- rpart(formula = TargetB ~ .,data = data[split,vars],
              control=rpart.control(cp=0.005)     )



# Model pruning
cp.seq=tree$cptable[,1]
misc<-numeric()
for (i in 1:length(cp.seq)) {
  tree.predict = predict(prune(tree, cp=cp.seq[i]), data[!split,vars],type="class") 
  cm=table(data[!split,vars]$TargetB, tree.predict) # confusion matrix 
  misc[i]=(cm[1,2]+cm[2,1])/sum(cm) # misclassification rate
}

plot(tree$cptable[,'nsplit']+1,misc,
     type="o", xlab="Number of Leaves", ylab="Misclassification Rate")


# Final model
tree.final=prune(tree,cp=cp.seq[misc==min(misc)])
plot(as.party(tree.final))
tree.misc<-min(misc)



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
vars<--grep("^(ID|TargetD)", names(data.mdl)) # exclude rejected vars


levels(data.mdl$TargetB) # check primary outcome 
# glm function uses 2nd factor level as primary

# Build full model
full = glm(TargetB ~., family=binomial, data=data.mdl[split, vars])
summary(full)
n<-sum(split)


# Backward
reg.bwd <- step(full, direction="backward",k=log(n), trace = FALSE)
summary(reg.bwd)
reg.bwd.prob<-predict(reg.bwd,data.mdl[!split, vars], type = "response")
reg.bwd.class <- ifelse(reg.bwd.prob > 0.5, 1, 0)
reg.bwd.misc<- 1-mean(reg.bwd.class == data.mdl[!split,]$TargetB)
reg.bwd.misc



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
  layer_dense(units = 6, activation = "tanh", input_shape = c(7)) %>%
  layer_dense(units = 6, activation = "tanh") %>%
  layer_dense(units = 1, activation = "sigmoid")



ann %>% compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)



callbacks.list = list(
  callback_early_stopping(
    monitor = "val_acc",
    patience = 5
  ),
  callback_model_checkpoint(
    filepath="my_ann.h5",
    monitor = "val_acc",
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



ann.select <-load_model_hdf5("my_ann.h5") 
results.valid <- evaluate(ann.select, x.valid,y.valid)
ann.misc<-1-results.valid$acc


## Prediction ##
ann.class<-predict_classes(ann.select,x.valid)
ann.prob<-predict_proba(ann.select,x.valid)


############################################
################ Week 7 ####################
############################################

### Misclassification Rate ###
c(tree.misc,  reg.bwd.misc, ann.misc)



### Confusion Matrix ###
table(ann.class, y.valid)


library(gmodels)
CrossTable(ann.class, y.valid)



### Evaluations related to Confusion Matrix ###
library(caret)
ann.class <- as.factor(ann.class)
y.valid <- as.factor(y.valid)
confusionMatrix(data = ann.class, 
                reference = y.valid,
                positive = "1",      # Default is 1st level
                mode= "everything")



### ROC curves ###
library(pROC)
rocCurve.ann<- roc(response =  y.valid, 
                   predictor = ann.prob,
                   levels = levels(y.valid))  # This function assumes that 2nd
                                              # level is the event of interest, 
                                              # and 1st level is control.
                   

# ANN: area under curve
auc(rocCurve.ann)

# DT
tree.prob <- predict(tree.final, data[!split,vars], type = "prob")[,2]
rocCurve.tree <- roc(data[!split,]$TargetB, tree.prob)

# Reg
rocCurve.reg<- roc(data.mdl[!split,]$TargetB, reg.bwd.prob)

## Compare Area Under Curve
c(auc(rocCurve.tree), auc(rocCurve.reg), auc(rocCurve.ann))



## Plot ROC Curves
plot(rocCurve.tree, legacy.axes = TRUE, col= 'blue')
plot.roc(rocCurve.reg, add=TRUE, legacy.axes = TRUE, col= 'red')
plot.roc(rocCurve.ann, add=TRUE, legacy.axes = TRUE, col= 'green')
legend('topleft', legend=c('valid.ann', 'valid.reg', 'valid.tree'), 
       col=c("green","red","blue"),lty=c(1,1,1))




### Profit ###
data.profit <- data

levels(data.profit$TargetB)<-rev(levels(data.profit$TargetB)) 

# Define profit matrix
profitMatrix<- matrix(c(0.55,-0.45,0,0), nrow=2)
## profitMatrix<- matrix(c(1,0,0,1),nrow=2)
rownames(profitMatrix) <- levels(data.profit$TargetB)
colnames(profitMatrix) <- levels(data.profit$TargetB)
profitMatrix


# Calculate decision threshold
N <- profitMatrix[2,2]-profitMatrix[2,1] 
D <- N - (profitMatrix[1,2]-profitMatrix[1,1]) 
threshold <- N/D
threshold


# ANN
ann.pclass <- as.factor(ifelse(ann.prob >= threshold, 1, 0)) # decision based on profit
levels(ann.pclass) <- rev(levels(ann.pclass))
ann.cm=table(data.profit[!split,vars]$TargetB, ann.pclass) # confusion matrix 
ann.profit=sum(profitMatrix*ann.cm)/sum(ann.cm) 


# Function: calculate model average profit
avgprofit<- function(pmatrix, prob, target){ 
  # pmatrix: profit matrix
  #    prob: predicted prob.
  # target:  actual target, assuming 2nd level 
  #          is of primary interest
  
  N <- pmatrix[2,2]-pmatrix[2,1] 
  D <- N - (pmatrix[1,2]-pmatrix[1,1]) 
  threshold <- N/D
  pclass<-as.factor(ifelse(prob >= threshold, 1, 0))
  levels(pclass) <- rev(levels(pclass))
  levels(target) <- rev(levels(target))
  cm <- table(target,pclass)
  profit <- sum(pmatrix*cm)/sum(cm) 
  profit
}



# Reg
reg.profit <- avgprofit(profitMatrix, reg.bwd.prob, data.mdl[!split,]$TargetB)


# DT
tree.profit=avgprofit(profitMatrix, tree.prob, data[!split,]$TargetB)


# Compare average profits
c(tree.profit, reg.profit, ann.profit)

## misclassification rate
## 1-c(tree.profit, reg.profit, ann.profit)






























 


















