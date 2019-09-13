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


############################################
################ Week 4 ####################
############################################


## Partitioning ##
library(caTools)
set.seed(1234)
split = sample.split(data$TargetB, SplitRatio = 0.5) 

summary(data[split,])
summary(data[!split,])

# Challenge: How to split data into three sets
# say, training/validation/test with ratio 70:20:10,
# while maintaining the stratification of Target 1/0




####################################
######### Decision Tree ############
####################################

# Model building
vars <- -grep("^(ID|TargetD)", names(data))

library(rpart)sum
tree <- rpart(formula = TargetB ~ .,data = data[split,vars],
              control=rpart.control(cp=0.005))

summary(tree)
print(tree$cptable)
printcp(tree)

# Model visualization
library(partykit)
#plot(as.party(tree))
print(tree)
#dev.off() # reset graphic options

# Model pruning
cp.seq=tree$cptable[,1]
misc<-numeric()
for (i in 1:length(cp.seq)) {
  tree.predict = predict(prune(tree, cp=cp.seq[i]), data[!split,vars],type="class") 
  cm=table(data[!split,vars]$TargetB, tree.predict) # confusion matrix 
  misc[i]=(cm[1,2]+cm[2,1])/sum(cm) # misclassification rate
}

plot(tree$cptable[,'nsplit']+1,misc,
     type="o", xlab="Number of Leaves", ylab="Misclassification Rate")





# Final model
tree.final=prune(tree,cp=cp.seq[misc==min(misc)])
plot(as.party(tree.final))


tree.final$variable.importance 
names(tree.final$variable.importance) # extract useful variable names





## Model Evaluation: Lift Graph ##

evaluate.prob <- predict(tree.final, data[!split,vars], type = "prob")[,2]
train.prob <- predict(tree.final, data[split,vars], type = "prob")[,2]

library(ROCR)
pred.eva <- prediction(evaluate.prob, data[!split,vars]$TargetB)
pred<-prediction(train.prob, data[split,vars]$TargetB)

perf.eva <- performance(pred.eva,"lift","rpp")
perf <- performance(pred,"lift","rpp")

plot(perf, col='blue', type="b", main="Lift Curve")
plot(perf.eva, col= 'red', type="b",add = TRUE,main="Lift Curve")
legend('topright', legend=c('train', 'validation'), col=c("blue","red"),lty=c(1,1))


##  Ordinal Input Variables ##
index.cat<-sapply(data, is.factor)
index.cat[1]<-FALSE
summary(data[index.cat])

data$DemCluster<-factor(data$DemCluster, order = TRUE)
str(data$DemCluster)

tree.ordinal<- rpart(formula = TargetB ~ DemCluster,data = data[split,vars])
plot(as.party(tree.ordinal))
print(tree.ordinal)



data$StatusCat96NK <- factor(data$StatusCat96NK, order = TRUE,
                                 levels = c("S", "N", "L","F","E","A"))
str(data$StatusCat96NK)
tree.ordinal<- rpart(formula = TargetB ~ StatusCat96NK,data = data[split,vars])
plot(as.party(tree.ordinal))
print(tree.ordinal)



