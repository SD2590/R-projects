library(ISLR)
library(ggplot2)
head(iris)

#Checking how the species are grouped
ggplot(iris,aes(Petal.Length,Petal.Width)) + geom_point(aes(color=Species), size=2)

#Build model
set.seed(101)
irisCluster <- kmeans(iris[,1:4],centers = 3, nstart = 20)
irisCluster

#Test the model
table(irisCluster$cluster,iris$Species)

#Viz
library(cluster)
clusplot(iris,irisCluster$cluster,color=T,shade=T, lines=0,labels=0)
