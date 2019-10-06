##########################
####### Quiz Five ########
##########################

organics<-read.csv("organics.csv", header=TRUE, na.strings=c(".", "NA", "", "?"))

organics$ID<-as.factor(organics$ID)
organics$DemCluster<-as.factor(organics$DemCluster)
organics$TargetBuy<-as.factor(organics$TargetBuy)

library(caTools)
set.seed(4321)
split = sample.split(organics$TargetBuy, SplitRatio = 0.5)  


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




# part a #

######## Routine: Update Input Info ########
inp.n <- grep('^(ID|TargetAmt|DemCluster$|TargetBuy)', names(organics)) 
inx3<-inx(organics, inp.n) 
indx<-inx3$indx
index.cat<-inx3$index.cat
index.na<-inx3$index.na
############################################

# check missing data
names(organics)[index.na==TRUE]




# part c: Imputation #

organics.imp<-organics

######## Routine: Update Input Info ########
# inp.n <- grep('^(ID|TargetAmt|DemCluster$)', names(organics.imp)) 
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





# part d #

organics.mdl<-organics.imp

vars <- - grep('^(ID|TargetAmt|DemCluster$)', names(organics.mdl)) 

levels(organics.mdl$TargetBuy) 

full = glm(TargetBuy ~., family=binomial, data=organics.mdl[split, vars])
summary(full)

null<-glm(TargetBuy ~1, family=binomial, data=organics.mdl[split, vars])



# Stepwise selection
reg.step <- step(null, scope=formula(full), direction="both", trace = FALSE)
summary(reg.step)

# Variable importance
library(caret)
varImp(reg.step) 
varImp(reg.step)[order(-varImp(reg.step)),,drop=FALSE]

# Validation MISC
reg.step.prob<-predict(reg.step,organics.mdl[!split, vars], type = "response") 
reg.step.class <- ifelse(reg.step.prob > 0.5, 1, 0)
reg.step.misc<- 1-mean(reg.step.class == organics.mdl[!split,]$TargetBuy)
reg.step.misc






# part e-f: Transformation #

organics.xf<-organics

######## Routine: Update Input Info ########
# inp.n <- grep('^(ID|TargetAmt|DemCluster$|TargetBuy)', names(organics.xf))
inx3<-inx(organics.xf, inp.n)
indx<-inx3$indx
index.cat<-inx3$index.cat
index.na<-inx3$index.na
############################################

vars.xf<-c("PromTime", "PromSpend")
organics.xf[vars.xf]<-log(organics.xf[vars.xf]+1)


par(mfrow=c(2,2))
hist(organics[split,]$PromSpend)
hist(organics.xf[split,]$PromSpend)
hist(organics[split,]$PromTime)
hist(organics.xf[split,]$PromTime)
par(mfrow=c(1,1))

library(fBasics)
skewness(organics[split,vars.xf],na.rm = TRUE)
skewness(organics.xf[split,vars.xf],na.rm = TRUE)
kurtosis(organics[split,vars.xf],na.rm = TRUE)
kurtosis(organics.xf[split,vars.xf],na.rm = TRUE)




# part g #

#### Imputation #####

organics.imp<-organics.xf

######## Routine: Update Input Info ########
# inp.n <- grep('^(ID|TargetAmt|DemCluster$)', names(organics.imp)) 
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

#### Modeling #####

organics.mdl<-organics.imp

vars <- - grep('^(ID|TargetAmt|DemCluster$)', names(organics.mdl)) 

levels(organics.mdl$TargetBuy) 

full = glm(TargetBuy ~., family=binomial, data=organics.mdl[split, vars])
summary(full)

# variable importance #
varImp(full) # absolute value of z stat
varImp(full)[order(-varImp(full)),,drop=FALSE]


null<-glm(TargetBuy ~1, family=binomial, data=organics.mdl[split, vars])



# Stepwise selection
reg.step <- step(null, scope=formula(full), direction="both", trace = FALSE)
summary(reg.step)

# Validation MISC
reg.step.prob<-predict(reg.step,organics.mdl[!split, vars], type = "response") 
reg.step.class <- ifelse(reg.step.prob > 0.5, 1, 0)
reg.step.misc<- 1-mean(reg.step.class == organics.mdl[!split,]$TargetBuy)
reg.step.misc




###### ANN #########


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


###########################
## ANN Preparation ##
###########################

inp.n <- grep("^(TargetBuy)", names(organics.ann.encode)) # no change



x.train <- as.matrix(organics.ann.encode[split,-inp.n])
y.train<- as.matrix(organics.ann.encode[split,"TargetBuy"])
x.valid<-as.matrix(organics.ann.encode[!split,-inp.n])
y.valid<-as.matrix(organics.ann.encode[!split,"TargetBuy"])









library(keras)
#install_keras()
use_session_with_seed(42)

ann <- keras_model_sequential() 
ann %>% 
  layer_dense(units = 4, activation = "tanh", input_shape = c(8)) %>% 
  layer_dense(units = 4, activation = "tanh") %>% 
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
evaluate(ann.select, x.valid,y.valid)$acc








