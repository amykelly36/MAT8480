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


impute <- function(x,y) {
  x[is.na(x)]<-y
  x
}

# numeric impute: By mean #
means <- function(x) { mean(x, na.rm = TRUE) }
Mean<-sapply(data.imp[split,indx],means)

data.imp[indx]<-as.data.frame(mapply(impute,x=data.imp[indx],y = Mean))


# create missing value flag #
data.imp[paste(names(data.xf)[index.na], "NA", sep=".")] <- ifelse(
  is.na(data.xf[index.na]), 1, 0)


### FIX: set missing flags as nominal inputs ###
### Use lapply!!! ###
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



############################################
################ Week 6 ####################
############################################


########################################
####### Artificial Neural Network ######
########################################

### FIX: set missing flags as nominal inputs ###

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
#install_keras()



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
#summary(ann.select)
results.valid <- evaluate(ann.select, x.valid,y.valid)
results.valid 


## Prediction ##
evaluate.class<-predict_classes(ann.select,x.valid)
evaluate.prob<-predict_proba(ann.select,x.valid)
train.prob <- predict_proba(ann.select,x.train)





##############
# Lift Graph #
##############

## Model Evaluation: Lift Graph ##
evaluate.prob1 <- predict(reg.bwd, data.mdl[!split,vars], type = "response")


library(ROCR)
pred.eva1 <- prediction(evaluate.prob1, data.mdl[!split,]$TargetB)
pred.eva <- prediction(evaluate.prob, data.mdl[!split,]$TargetB)
pred<-prediction(train.prob, data.mdl[split,]$TargetB)

perf.eva1 <- performance(pred.eva1,"lift","rpp")
perf.eva <- performance(pred.eva,"lift","rpp")
perf <- performance(pred,"lift","rpp")

plot(perf, col='blue',  main="Lift Curve")
plot(perf.eva, col= 'red', add = TRUE,main="Lift Curve")
plot(perf.eva1, col= 'green', add = TRUE,main="Lift Curve")
legend('topright', legend=c('train.ann', 'valid.ann', 'valid.bwd'), col=c("blue","red","green"),lty=c(1,1))
