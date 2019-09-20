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


############################################
################ Week 5 ####################
############################################


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
  is.na(data.xf[index.na]), 1, 0)  # insepct missing values from preivous dataframe



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


# Set up null model
null<-glm(TargetB ~1, family=binomial, data=data.mdl[split, vars])
n<-sum(split) # training size (for BIC)


# Stepwise selection
reg.step <- step(null, scope=formula(full), direction="both", k=log(n))
summary(reg.step)
reg.step.prob<-predict(reg.step,data.mdl[!split, vars], type = "response") 
reg.step.class <- ifelse(reg.step.prob > 0.5, 1, 0)
reg.step.misc<- 1-mean(reg.step.class == data.mdl[!split,]$TargetB)
reg.step.misc


# Forward
reg.fwd <- step(null, scope=formula(full), direction="forward", k=log(n), trace = FALSE)
summary(reg.fwd)
reg.fwd.prob<-predict(reg.fwd,data.mdl[!split, vars], type = "response")
reg.fwd.class <- ifelse(reg.fwd.prob > 0.5, 1, 0)
reg.fwd.misc<- 1-mean(reg.fwd.class == data.mdl[!split, ]$TargetB)
reg.fwd.misc


# Backward
reg.bwd <- step(full, direction="backward",k=log(n), trace = FALSE)
summary(reg.bwd)
reg.bwd.prob<-predict(reg.bwd,data.mdl[!split, vars], type = "response")
reg.bwd.class <- ifelse(reg.bwd.prob > 0.5, 1, 0)
reg.bwd.misc<- 1-mean(reg.bwd.class == data.mdl[!split,]$TargetB)

c(reg.step.misc, reg.fwd.misc, reg.bwd.misc)


# odds ratio estimate #
exp(coef(reg.bwd))


# variable importance #
varImp(reg.bwd) # absolute value of z stat
varImp(reg.bwd)[order(-varImp(reg.bwd)),,drop=FALSE]



###########################
## Polynomial Regression ##
###########################

vars.poly<-attr(terms(reg.bwd), "term.labels") # extract variable names from bwd model
vars.poly<-c(vars.poly, "TargetB")


reg.poly <- glm(TargetB ~.+I(DemMedHomeValue*GiftTimeLast)+I(DemMedHomeValue^2),
               family=binomial, data=data.mdl[split, vars.poly])
summary(reg.poly)


reg.poly <- glm(TargetB ~.*.,
               family=binomial, data=data.mdl[split, vars.poly])
summary(reg.poly)


# Stepwise selection
reg.poly.step <- step(reg.poly, k=log(n), trace = FALSE)
summary(reg.poly.step)
reg.poly.step.prob<-predict(reg.poly.step,data.mdl[!split, vars.poly], type = "response")
reg.poly.step.class <- ifelse(reg.poly.step.prob > 0.5, 1, 0)
reg.poly.step.misc<- 1-mean(reg.poly.step.class == data.mdl[!split,]$TargetB)
reg.poly.step.misc


##############
# Lift Graph #
##############

## Model Evaluation: Lift Graph ##
evaluate.prob1 <- predict(reg.poly.step, data.mdl[!split,vars], type = "response")
evaluate.prob <- predict(reg.bwd, data.mdl[!split,vars], type = "response")
train.prob <- predict(reg.bwd, data.mdl[split,vars], type = "response")

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
legend('topright', legend=c('train.bwd', 'valid.bwd', 'valid.poly'), col=c("blue","red","green"),lty=c(1,1))
