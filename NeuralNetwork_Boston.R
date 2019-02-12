library("MASS", lib.loc="/usr/lib/R/library")

head(Boston)
str(Boston)

#is any data missing
any(is.na(Boston))

data <- Boston

#Normalize the data
maxs <- apply(data,2,max)
maxs
mins <- apply(data,2,min)
mins
scaled.data <- scale(data,center=mins,scale=maxs-mins)
scaled <- as.data.frame(scaled.data)
head(scaled)

#split into test and train data
library(caTools)
split <- sample.split(scaled$medv,SplitRatio = 0.7)
train <- subset(scaled,split==T)
test <- subset(scaled,split==F)

#install the required package
install.packages("neuralnet")
library(neuralnet)

#Get column names
n <- names(train)
#Paste together
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
f

#Create neural net
nn <- neuralnet(f,data=train,hidden=c(5,3),linear.output = T)
plot(nn)

#Create predictions
predicted.nn.values <- compute(nn,test[1:13])
str(predicted.nn.values)
#net.result is the actual predicted values

# Convert back to non-scaled predictions
true.predictions <- predicted.nn.values$net.result*(max(data$medv)-min(data$medv))+min(data$medv)

# Convert the test data
test.r <- (test$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

#Calculate error
MSE.nn <- sum((test.r - true.predictions)^2)/nrow(test)
MSE.nn

#Visualize Error
error.df <- data.frame(test.r,true.predictions)
head(error.df)

library(ggplot2)
ggplot(error.df,aes(x=test.r,y=true.predictions)) + geom_point() + stat_smooth()
