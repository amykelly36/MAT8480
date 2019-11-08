# K-Means Clustering

# Importing the dataset
census <- read.csv('census2000.csv', header=TRUE, na.strings=c(".", "NA", "", "?"))
str(census)
census[c("RegPop","MedHHInc")]<-sapply(census[c("RegPop","MedHHInc")], 
                                       function(x) gsub("\\,", "", x))           
census[c("RegPop","MedHHInc")]<-sapply(census[c("RegPop","MedHHInc")], 
                                       function(x) as.numeric(gsub("\\$", "", x)))
str(census)


# Exploring the dataset
library(Amelia)
missmap(census)
dev.off() 

library(Hmisc)
hist.data.frame(census)
par(mfrow=c(1,1)) # reset graphic layout

# interactive sorting

# Data Filtering
census.fltr <- census[census$MeanHHSz > 0,]
missmap(census.fltr)
dev.off() 


# Variable Standardization 
census.scale<-census.fltr
vars<-grep("^(RegDens|Me)", names(census.fltr))
census.scale[vars]<-scale(census.fltr[vars])




# Using the elbow method to find the optimal number of clusters
ncluster<-20
wcss <- numeric()
for (i in 1:ncluster) {
  set.seed(1234)
  wcss[i] <- kmeans(census.scale[vars], i, iter.max = 100)$tot.withinss
}

plot(1:ncluster, wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')


# Fitting K-Means to the dataset
set.seed(1234)
km <- kmeans(x = census.scale[vars], centers = 5, nstart=25) # Have 'nstart' sets of starting and ending centers.
                                                             # Pick the center set that have the lowest distance 
                                                             # from the data points to the centroids.

table(km$cluster)  # display cluster size


# Visualising the clusters
library(factoextra)
fviz_cluster(km, data = census.scale[vars])


# Profiling
census.fltr$cluster <-as.factor(km$cluster) 

library(rattle)
rattle()

