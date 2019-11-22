library(arules)

# part a
store <- read.csv("transactions.csv")

# part b
store$Transaction<-as.factor(store$Transaction)

# part c
Transaction<-as(split(store$Product,store$Transaction), "transactions")

# part d
rules = apriori(data = Transaction, parameter = list(support = 0.01, confidence = 0.1))
rules

# part e
inspect(sort(rules, by = 'support')[1:10])

inspect(sort(rules, by = 'lift')[1:55])

inspect(sort(rules, by = 'confidence')[1:10])






