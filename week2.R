## set up working directory ##
# setwd("your_path")
# getwd()

## import data ##
data<-read.csv("pva97nko.csv", header=TRUE, na.strings=c(".", "NA", "", "?"))

str(data)
names(data)
head(data,10)  # display top six observations


## data cleaning and define measure levels ##
indx <- sapply(data, is.factor) 
indx[c("StatusCat96NK","DemGender","DemHomeOwner")]<-c(FALSE,FALSE,FALSE)


data[c("DemMedHomeValue", "DemMedIncome")]<-sapply(data[c("DemMedHomeValue", "DemMedIncome")], 
                                                   function(x) gsub("\\,", "", x))
data[indx] <- sapply(data[indx], function(x) as.numeric(gsub("\\$", "", x)))


str(data)
data$TargetB<-as.factor(data$TargetB)
data$StatusCatStarAll<-as.factor(data$StatusCatStarAll)


### Data Exploration ###

## summary statistics ##

# basic summary statistics
summary(data)

# detailed summary statistics
#install.packages("fBasics")
library(fBasics)
basicStats(data$DemMedHomeValue) # only for numeric var.

indx <- sapply(data, is.numeric) 
skewness(data[indx],na.rm=TRUE)
kurtosis(data[indx],na.rm=TRUE)


## distribution plots ##

# histogram
hist(data$DemMedIncome, breaks=100, xlab="DemMedIncome", main="Histogram of DemMedIncome")

# bar chart
fq<-summary(data$StatusCat96NK)
ord <- order(fq, decreasing=TRUE)
bp<-barplot(fq[ord], ylab="Frequency", xlab="StatusCat96NK", ylim=c(0,max(fq)*1.1),
                 main="Distribution of StatusCat96NK")
text(bp,y=fq[ord], label=fq[ord], pos = 3) 

# hitograms and dot plots from dataframe
#install.packages("Hmisc")
library(Hmisc)
hist.data.frame(data)
par(mfrow=c(1,1)) # reset graphic layout



## missing values exploration ##
sapply(data, function(df) {
  sum(is.na(df)==TRUE)/ length(df)
})


#install.packages("Amelia")
library(Amelia)
missmap(data, main="Missing Map")


### data modification/correction ###

## Replace DemMedIncome ##
data$DemMedIncome[data$DemMedIncome==0]<-NA



### Optional: save selective objects ###
save(data,indx, file="mysession.RData")
load("mysession.RData")
