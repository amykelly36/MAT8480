##########################
####### Quiz Three #######
##########################

# parts a-b #
organics<-read.csv("organics.csv", header=TRUE, na.strings=c(".", "NA", "", "?"))

organics$ID<-as.factor(organics$ID)
organics$DemCluster<-as.factor(organics$DemCluster)
organics$TargetBuy<-as.factor(organics$TargetBuy)

# part d #
vars <- -grep('^(ID|TargetAmt|DemCluster)', names(organics)) 
vars<- vars[-3] # undo DemClusterGroup


# part e #
library(caTools)
set.seed(4321)
split = sample.split(organics$TargetBuy, SplitRatio = 0.5)  


# part f #
library(rpart)
DT<- rpart(formula = TargetBuy ~ .,data = organics[split,vars])
summary(DT)
print(DT$cptable)

library(partykit)
plot(as.party(DT))

# part g.1 #
DT.001<- rpart(formula = TargetBuy ~ .,data = organics[split,vars],
           control=rpart.control(cp=0.001))

cp.seq=DT.001$cptable[,1]
MISC<-numeric()
for (i in 1:length(cp.seq)) {
  DT.predict = predict(prune(DT.001, cp=cp.seq[i]), organics[!split,vars],type="class")
  cm=table(DT.predict, organics[!split,vars]$TargetBuy)
  MISC[i]=(cm[1,2]+cm[2,1])/sum(cm)
}

plot(DT.001$cptable[,'nsplit']+1,MISC,
     type="o", xlab="Number of Leaves", ylab="Misclassification Rate")

min(MISC) # validation MISC


# part g.2 #
tree.final=prune(DT.001,cp=cp.seq[MISC==min(MISC)])

# Lift Graph #
evaluate.prob <- predict(tree.final, organics[!split,vars], type = "prob")[,2]
train.prob <- predict(tree.final, organics[split,vars], type = "prob")[,2]

require(ROCR)
pred<-prediction(train.prob, organics[split,vars]$TargetBuy)
pred.eva <- prediction(evaluate.prob, organics[!split,vars]$TargetBuy)

perf <- performance(pred,"lift","rpp")
perf.eva <- performance(pred.eva,"lift","rpp")

plot(perf, col='blue', type="b", main="Lift Curve")
plot(perf.eva, col= 'red', type="b",add = TRUE,main="Lift Curve")
legend('topright', legend=c('train', 'validation'), col=c("blue","red"),lty=c(1,1))


# part g.3 #
rpp<-perf.eva@x.values[[1]] # perf.eva is S4 class. Elements are accessed using "@"
lift<-perf.eva@y.values[[1]]
lift[which.min(abs(rpp-0.2))]




