########################
####### Quiz One #######
########################

# import organics data #
organics<-read.csv("organics.csv", header=TRUE, na.strings=c(".", "NA", "", "?"))

# check measurement levels
str(organics)

# fix measurement levels
organics$ID<-as.factor(organics$ID)
organics$DemCluster<-as.factor(organics$DemCluster)
organics$TargetBuy<-as.factor(organics$TargetBuy)



# part c #
names(organics)[sapply(organics[1:26,], function(df) {any(is.na(df))})] # check ?any for description
# or,
head(organics,26) 



# part d #
# 1)
fq<-summary(organics$TargetBuy)
ord <- order(fq, decreasing=TRUE)
bp<-barplot(fq[ord], ylab="Frequency", xlab="TargetBuy", ylim=c(0,max(fq)*1.1),
            main="Distribution of TargetBuy")
text(bp,y = fq[ord], label = fq[ord], pos = 3 ) 


# 2)
fq<-summary(organics$DemGender)
ord <- order(fq, decreasing=TRUE)
bp<-barplot(fq[ord], ylab="Frequency", xlab="DemGender", ylim=c(0,max(fq)*1.1),
        main="Distribution of DemGender")
text(bp,y = fq[ord], label = fq[ord], pos = 3 ) 


# 3)
hist(organics$DemAge, xlab="DemAge", main="Histogram of DemAge")
summary(organics$DemAge) # check missing values


# 4)
hist(organics$PromSpend, breaks=50, xlab="PromSpend", main="Histogram of PromSpend")


# 5)
misspcnt<-sapply(organics, function(df) {
  sum(is.na(df)==TRUE)/ length(df)
})

sort(misspcnt,decreasing = TRUE) # sort missing percent 



# part e #
organics1<-organics[-(10:85), ] 
library(fBasics)
basicStats(organics1$PromSpend) 





