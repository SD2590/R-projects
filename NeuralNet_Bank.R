bank <- read.csv('/resources/data/bank_note_data.csv')
head(bank)
str(bank)

#Split data
library(caTools)
set.seed(101)
split <- sample.split(bank$Class,SplitRatio = 0.7)
train <- subset(bank,split==T)
test <- subset(bank,split==F)

#Build neural net
library(neuralnet)
nn <- neuralnet(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy,data=train,hidden=10,linear.output=FALSE)

#Predictions
predicted.values <- compute(nn,test[1:4])
head(predicted.values$net.result)
predictions <- sapply(predicted.values$net.result,round)
table(predictions,test$Class)

#Build randomForest
library(randomForestSRC)
bank$Class <- as.factor(bank$Class)

library(caTools)
set.seed(101)
split <- sample.split(bank$Class,SplitRatio = 0.7)
train <- subset(bank,split==T)
test <- subset(bank,split==F)

model <- rfsrc(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy,data=train)
rf.pred <- predict(model,test)
head(rf.pred)
table(rf.pred$yvar,test$Class)
