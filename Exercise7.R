################################
####### Exercise Seven #########
################################

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

library(rpart)
DT.001<- rpart(formula = TargetBuy ~ .,data = organics[split,vars])








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











##################################
############ Scoring #############
##################################





















