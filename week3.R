## import data ##
data<-read.csv("pva97nko.csv", header=TRUE, na.strings=c(".", "NA", "", "?"))

## data cleaning and define measure levels ##
indx <- sapply(data, is.factor) 
indx[c("StatusCat96NK","DemGender","DemHomeOwner")]<-c(FALSE,FALSE,FALSE)
data[c("DemMedHomeValue", "DemMedIncome")]<-sapply(data[c("DemMedHomeValue", "DemMedIncome")], 
                                                   function(x) gsub("\\,", "", x))
data[indx] <- sapply(data[indx], function(x) as.numeric(gsub("\\$", "", x)))
data$TargetB<-as.factor(data$TargetB)
data$StatusCatStarAll<-as.factor(data$StatusCatStarAll)

## data modification/correction ##
## Replace DemMedIncome ##
data$DemMedIncome[data$DemMedIncome==0]<-NA

############################################
################ Week 3 ####################
############################################

## Correction ##
data$DemCluster<-as.factor(data$DemCluster)

################################################
## Data Exploration II: Bivariate Exploration ##
################################################

# Use Rattle #
# install.packages("RGtk2")
# library(RGtk2)
# Restart R session
# install.packages("rattle")
data.rattle<-data
library(rattle)
rattle()
par(mfrow=c(1,1)) # reset graphic layout


######## Stacked Bar Plot ##########
# nominal input v.s. binary target #

counts = table(data$TargetB, data$StatusCatStarAll)
barplot(counts, col=c("darkblue","red"), legend.text = TRUE, 
        main = "StatusCatStarAll by TargetB",
        xlab="StatusCatStarAll", ylab="Freq")

counts = table(data$TargetB, data$DemGender)
barplot(counts, col=c("darkblue","red"), legend.text = TRUE,
        main = "DemGender by TargetB",
        xlab="DemGender", ylab="Freq")

# Default Factor Order: Alphabetical #
levels(data$TargetB)
levels(data$DemGender)


## Summary statistics by Target variable ##
by(data, data$TargetB, summary)

# Detailed statistics: for numeric var only #
# install.packages("psych")
library(psych)
indx <- sapply(data, is.numeric)
describeBy(data[indx], data$TargetB)  



#################################
## Variance Importance Measure ##
#################################

indx <- sapply(data, is.numeric)
indx[1:3]<-FALSE

index.cat<-sapply(data, is.factor)
index.cat[1]<-FALSE

## Binary Target ##

# nominal input - chi-square
chi2pvalues<- sapply(data[index.cat], 
                     function(x) chisq.test(x, data$TargetB)$p.value)
sort(chi2pvalues)


# numeric input - t stat
tTestpvalues<-sapply(data[indx], 
              function(x) t.test(x ~ data$TargetB)$p.value)
sort(tTestpvalues)


# Any input - area under ROC curve for predicting target
# install.packages("caret")
library(caret)
rocValues<- filterVarImp(x = data[-(1:3)], y = data$TargetB)
rocValues
rocValues[order(-rocValues$X1),]



## Numeric Target ##

# numeric input - Pearson correlation
corrValues <- sapply(data[indx],
                     function(x) abs(cor(x, data$TargetD, use = "complete.obs")))
sort(corrValues, decreasing = TRUE)


# categorical input - ANOVA F-stat
anovapvalues<-sapply(data[index.cat], 
                     function(x) summary(aov(data$TargetD ~ x))[[1]][["Pr(>F)"]][[1]])
sort(anovapvalues)


# Any input - Lowess R^2
library(caret)
data.loess<-data[complete.cases(data$TargetD), ]
loessResults<- filterVarImp(x = data.loess[-(1:3)], y = data.loess$TargetD, 
                            nonpara = TRUE)
loessResults
loessResults[order(-loessResults),, drop = FALSE]




################################
#### Variable Transformation ###
################################

# Box-Cox Transformation on numeric inputs

library(caret)
# install.package("e1071")
TransformParams <- preProcess(data[indx], method=c("BoxCox"))
TransformParams$bc
# preprocessParams1 <- preProcess(data[indx], method=c("YeoJohnson"))
# preprocessParams1$yj

TransformParams <- preProcess(data[c("GiftAvgAll", "GiftAvgCard36")], method=c("BoxCox"))
data.xf<-predict(TransformParams, data)


# Histograms before/after transformation
par(mfrow=c(2,2))
hist(data$GiftAvgAll)
hist(data.xf$GiftAvgAll)
hist(data$GiftAvgCard36)
hist(data.xf$GiftAvgCard36)
par(mfrow=c(1,1))

# Stats before/after transformation
library(fBasics)
basicStats(data[c("GiftAvgAll", "GiftAvgCard36")])
basicStats(data.xf[c("GiftAvgAll", "GiftAvgCard36")])


# Transformation on nominal inputs
data.xf<-data
levels(data.xf$StatusCat96NK)
levels(data.xf$StatusCat96NK)[levels(data$StatusCat96NK)=="S"]<-"A"
levels(data.xf$StatusCat96NK)[levels(data$StatusCat96NK)=="F"]<-"N"
levels(data.xf$StatusCat96NK)[levels(data$StatusCat96NK)=="E"]<-"L"



## Filter Obs ##
data.fltr<-data[data$DemGender!="U",] 
data.fltr<-data[data$DemAge>=18,]
data.fltr<-data[data$DemAge>=18 & data$DemGender!="U",]

# logical operators
# “|” is the logical operation OR
# “&” is the logical operation AND

str(data)
str(data.fltr)


#####################
##### Imputation ####
#####################

library(Amelia)
missmap(data, main="Missing Map")

# Nominal Input: By Mode #
impute <- function(x,y) {
  x[is.na(x)]<-y
  x
}

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode<-sapply(data[index.cat],mode)
data.imp<-data
data.imp[index.cat]<-as.data.frame(mapply(impute,x=data[index.cat],y = Mode))


# Numeric Input: By Mean #
means <- function(x) { mean(x, na.rm = TRUE) }
Mean<-sapply(data[indx],means)
data.imp<-data
data.imp[indx]<-as.data.frame(mapply(impute,x=data[indx],y = Mean))


# Numeric Input: By Median #
library(caret)
ImputeParams<-preProcess(data[-(1:3)], method = "medianImpute")
data.imp<-predict(ImputeParams,data)


# Create Missing Value Flag #
data.imp[c("GiftAvgCard36.NA","DemAge.NA","DemMedIncome.NA")] <- ifelse(
  is.na(data[c("GiftAvgCard36","DemAge","DemMedIncome")]), 1, 0)












