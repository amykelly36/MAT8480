########################
####### Quiz Two #######
########################

# import organics data #
organics<-read.csv("organics.csv", header=TRUE, na.strings=c(".", "NA", "", "?"))

# fix measurement levels
organics$ID<-as.factor(organics$ID)
organics$DemCluster<-as.factor(organics$DemCluster)
organics$TargetBuy<-as.factor(organics$TargetBuy)

# part 1a #

# Stacked bar plot  #
counts = table(organics$TargetBuy, organics$DemGender)
barplot(counts, col=c("darkblue","red"), legend.text = TRUE, 
        main = "DemGender by TargetBuy", 
        xlab="DemGender", ylab="Freq")

# Or, generate mosaic plot via Rattle


# part 1b #
by(organics$PromTime, organics$TargetBuy, summary)

# part 1c #
by(organics$PromClass, organics$TargetBuy, summary)



# part 2a #
index.cat<-sapply(organics, is.factor)
index.cat[c("ID","TargetBuy")]<-FALSE
chi2pvalues<- sapply(organics[index.cat], 
                     function(x) chisq.test(x, organics$TargetBuy)$p.value)
sort(chi2pvalues)
chisq.test(organics$DemGender, organics$TargetBuy)$statistic


# part 2b #
library(caret)
rocValues<- filterVarImp(x = organics[!(names(organics) %in% "TargetBuy")], y = organics$TargetBuy)
rocValues[order(-rocValues$X1),]


# part 2c #
indx <- sapply(organics, is.numeric)
indx["TargetAmt"]<-FALSE
corrValues <- sapply(organics[indx],
                     function(x) abs(cor(x, organics$TargetAmt, use = "complete.obs")))
sort(corrValues, decreasing = TRUE)

cor(organics$DemAffl, organics$TargetAmt, use = "complete.obs")
cor(organics$DemAge, organics$TargetAmt, use = "complete.obs")


# part 3a #
library(caret)
TransformParams <- preProcess(organics["PromSpend"], method=c("BoxCox"))
TransformParams$bc
organics.xf<-predict(TransformParams, organics)


# part 3b #
par(mfrow=c(1,2))
hist(organics$PromSpend)
hist(organics.xf$PromSpend)
par(mfrow=c(1,1))


# part 3c #
library(fBasics)
basicStats(organics$PromSpend)
basicStats(organics.xf$PromSpend)









