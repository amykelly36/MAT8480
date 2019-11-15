############################
####### Quiz Nine  #########
############################

# parts a & b #
dungaree <- read.csv('dungaree.csv', header=TRUE, na.strings=c(".", "NA", "", "?"))
str(dungaree)


# part c #
library(Amelia)
missmap(dungaree)
dev.off()

library(Hmisc)
hist.data.frame(dungaree)
par(mfrow=c(1,1)) 



# part e #
dungaree.scale<-dungaree
vars<- -grep("^(STOREID|SALESTOT)", names(dungaree))
dungaree.scale[vars]<-scale(dungaree[vars])


# part f #
set.seed(4321)
km <- kmeans(x = dungaree.scale[vars], centers = 6, nstart=25)
table(km$cluster)  


# part g #
dungaree$cluster <-as.factor(km$cluster)

library(rattle)
rattle()












