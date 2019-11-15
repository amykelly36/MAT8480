# install.packages('arules')
library(arules)

# Import the data
bank <- read.csv("bank.csv")

bank$ACCOUNT<-as.factor(bank$ACCOUNT)

# Convert into transaction data
Transaction<-as(split(bank$SERVICE,bank$ACCOUNT), "transactions")
summary(Transaction)
itemFrequencyPlot(Transaction, topN = 10)

# Train Apriori on the data
rules <- apriori(data = Transaction, parameter = list(support = 0.05, confidence = 0.1))
rules

# Visualize the results
inspect(sort(rules, by = 'lift')[1:10])

library(arulesViz)
plot(rules,  measure=c("confidence", "support"), shading="lift", engine = "interactive")



