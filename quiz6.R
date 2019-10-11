##########################
####### Quiz Six #########
##########################

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

library(rpart)
DT.001<- rpart(formula = TargetBuy ~ .,data = organics[split,vars],
               control=rpart.control(cp=0.001))

cp.seq=DT.001$cptable[,1]
MISC<-numeric()
for (i in 1:length(cp.seq)) {
  DT.predict = predict(prune(DT.001, cp=cp.seq[i]), organics[!split,vars],type="class")
  cm=table(DT.predict, organics[!split,vars]$TargetBuy)
  MISC[i]=(cm[1,2]+cm[2,1])/sum(cm)}

  
tree.final=prune(DT.001,cp=cp.seq[MISC==min(MISC)])
tree.class<-predict(tree.final, organics[!split,vars],type="class")
tree.prob <-predict(tree.final, organics[!split,vars], type = "prob")[,2]


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
inp.n <- grep('^(ID|TargetAmt|DemCluster$)', names(organics.imp)) 
inx3<-inx(organics.imp, inp.n)
indx<-inx3$indx
index.cat<-inx3$index.cat
index.na<-inx3$index.na
#########################################

impute <- function(x,y) {
  x[is.na(x)]<-y
  x
}

# Numeric Input: By Mean #
means <- function(x) { mean(x, na.rm = TRUE) }
Mean<-sapply(organics.imp[split,indx],means)

organics.imp[indx]<-as.data.frame(mapply(impute,x=organics.imp[indx],y = Mean))

# Nominal Input: By Mode #
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

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

levels(organics.mdl$TargetBuy) 

full = glm(TargetBuy ~., family=binomial, data=organics.mdl[split, vars])
summary(full)

null<-glm(TargetBuy ~1, family=binomial, data=organics.mdl[split, vars])

# Stepwise selection
reg.step <- step(null, scope=formula(full), direction="both", trace = FALSE)
summary(reg.step)

# Validation MISC
reg.step.prob<-predict(reg.step,organics.mdl[!split, vars], type = "response") 
reg.step.class <- ifelse(reg.step.prob > 0.5, 1, 0)
reg.step.misc<- 1-mean(reg.step.class == organics.mdl[!split,]$TargetBuy)
reg.step.misc




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
  callback_early_stopping(monitor = "val_acc", 
                          patience = 5),
  callback_model_checkpoint(filepath="my_ann_ex.h5", 
                            monitor = "val_acc", 
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



ann.select<- load_model_hdf5("my_ann_ex.h5") 

## Prediction ##
ann.class<-predict_classes(ann.select,x.valid)
ann.prob<-predict_proba(ann.select,x.valid)


############################### Continue from here #################################
TargetBuy.valid <- organics[!split,]$TargetBuy


# part b.1 #
ann.misc<-1-evaluate(ann.select, x.valid,y.valid)$acc 
tree.misc<-min(MISC)

c(tree.misc, reg.step.misc, ann.misc)


# part b.2 #
library(caret)
sapply(data.frame(tree.class, reg.step.class, ann.class),
       function(x) confusionMatrix(table(x, TargetBuy.valid),
                                   positive = "1")$byClass["F1"]
      )


# part b.3 #
library(pROC)
rocCurve.tree <- roc(TargetBuy.valid, tree.prob)
rocCurve.reg<- roc(TargetBuy.valid, reg.step.prob)
rocCurve.ann<- roc(TargetBuy.valid, ann.prob)  


## Plot ROC Curves
plot(rocCurve.tree, legacy.axes = TRUE, col= 'blue')
plot.roc(rocCurve.reg, add=TRUE, legacy.axes = TRUE, col= 'red')
plot.roc(rocCurve.ann, add=TRUE, legacy.axes = TRUE, col= 'green')
legend('topleft', legend=c('valid.ann', 'valid.reg', 'valid.tree'), 
       col=c("green","red","blue"),lty=c(1,1,1))

## Area under curve
auc(rocCurve.ann)


# part b.4 #

# Define profit matrix
profitMatrix<- matrix(c(4,0,0,4/3), nrow=2)
profitMatrix


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
  pclass <- factor(pclass,levels = rev(levels(pclass)))
  target <- factor(target,levels = rev(levels(target)) )
  cm <- table(target,pclass)
  profit <- sum(pmatrix*cm)/sum(cm) 
  profit
}

# ANN
ann.profit <- avgprofit(profitMatrix, ann.prob, TargetBuy.valid)

# Reg
reg.profit <- avgprofit(profitMatrix, reg.step.prob, TargetBuy.valid)


# DT
tree.profit<- avgprofit(profitMatrix, tree.prob, TargetBuy.valid)


# Compare average profits
c(tree.profit, reg.profit, ann.profit)
















