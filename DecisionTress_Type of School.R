install.packages("ISLR")
library(ISLR)

head(College)
str(College)

#EDA
library(ggplot2)
#scatterplot of Grad.Rate versus Room.Board, colored by the Private column
ggplot(College,aes(Room.Board,Grad.Rate)) + geom_point(aes(color=Private))

#histogram of full time undergrad students, color by Private.
ggplot(College,aes(F.Undergrad)) + geom_histogram(aes(fill=Private), color="black")

#histogram of Grad.Rate colored by Private.
ggplot(College,aes(Grad.Rate)) + geom_histogram(aes(fill=Private), color="black")

#college had a Graduation Rate of above 100%
library(dplyr)
filter(College,Grad.Rate>100)
#Change it to 100%
df <- College
df['Cazenovia College','Grad.Rate'] <- 100

#Split data into train and test
library(caTools)
set.seed(101)
sample = sample.split(df$Private, SplitRatio = .70)
train = subset(df, sample == TRUE)
test = subset(df, sample == FALSE)

#Build the model
model1 <- rpart(Private ~ .,method='class',data=train)
model1.pred <- predict(model1,test)
head(model1.pred)
#Turn these two columns into one column to match the original Yes/No Label for a Private column
model1.pred <- as.data.frame(model1.pred)
joiner <- function(x){
  if (x>=0.5){
    return('Yes')
  }else{
    return("No")
  }
}
model1.pred$Private <- sapply(model1.pred$Yes,joiner)

# create a confusion matrix of the tree model
table(model1.pred$Private,test$Private)

#plot the model
library(rpart.plot)
prp(model1)

#Build Random Forest
rf.model1 <- rfsrc(Private ~ .,data=train,importance=T)
rf.model1$importance

#Predictions
p <- predict(rf.model1,test)
table(p$yvar,test$Private)

