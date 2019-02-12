flowers <- iris
str(flowers)

#standardize features in your data
stand.features <- scale(iris[1:4])
var(stand.features[,1])

#Join the standardized data with the response column
final.flowers <- cbind(stand.features,flowers[5])
head(final.flowers)


#Split data into train and test
set.seed(101)

library(caTools)

sample <- sample.split(final.flowers$Species, SplitRatio = .70)
train <- subset(final.flowers, sample == TRUE)
test <- subset(final.flowers, sample == FALSE)

#Build KNN model
library(class)
predicted.species <- knn(train[1:4],test[1:4],train$Species,k=1)

#Choosing K value
predicted.species <- NULL
error.rate <- NULL

for(i in 1:10){
  set.seed(101)
  predicted.species <- knn(train[1:4],test[1:4],train$Species,k=i)
  error.rate[i] <- mean(test$Species != predicted.species)
}

library(ggplot2)
k.values <- 1:10
error.df <- data.frame(error.rate,k.values)

pl <- ggplot(error.df,aes(x=k.values,y=error.rate)) + geom_point() + geom_line(lty="dotted",color='red')
print(pl)
