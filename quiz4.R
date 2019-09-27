##########################
####### Quiz Four #######
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



#### Modeling #####

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

