df <- read.csv("/resources/data/student-mat.csv",sep=';')

#check if any data missing
any(is.na(df))

#structure of the dataframe
str(df)
summary(df)

#install packages ggplot2, ggthemes, dplyr, corrgram, corrplot
library(ggplot2)

#how failure is related to G3
ggplot(df,aes(failures, G3)) + geom_count(aes(color = ..n.., size = ..n..)) + guides(color = 'legend')

#influence of internet
ggplot(df,aes(internet,G3)) + geom_boxplot() + theme_bw()
ggplot(df,aes(internet,G2)) + geom_boxplot() + theme_bw()
ggplot(df,aes(internet,G1)) + geom_boxplot() + theme_bw()

#get numeric columns
num.cols <- sapply(df,is.numeric)
#apply correlation funstion on them
cor.data <- cor(df[,num.cols])
print(cor.data)
#for better visual understanding
library(corrplot)
print(corrplot(cor.data, method='color'))

#get frequency of G3
library(ggplot2)
ggplot(df,aes(x=G3)) + geom_histogram(bins=20,alpha=0.5,fill='blue')

#Split data into train and test sets
#install caTools
library(caTools)
#set a seed
set.seed(101)
#Split up sample
sample <- sample.split(df$G3,SplitRatio = 0.7)
#70% goes to train
train <- subset(df,sample==T)
#30% goes to test
test <- subset(df,sample==F)


#train and build model
model1 <- lm(G3 ~ .,data=train)
#run the model 
#intrepret the model
summary(model1)

#plot residuals
res <- as.data.frame(residuals(model1))
head(res)
ggplot(res,aes(x=residuals(model1))) +  geom_histogram(fill='blue',alpha=0.5)

plot(model1)

#Test the model with predictions
G3.predict <- predict(model1,test)
results <- cbind(G3.predict,test$G3)
colnames(results) <- c('predicted','actual')
results <- as.data.frame(results)

#function to change negative G3 results to 0
to_zero <- function(x){
  if(x<0){
    return(0)
  }
  else{
    return(x)
  }
}

#apply the function to the predicted results
results$predicted  <- sapply(results$predicted,to_zero)

#how good is the model?
#MSE
mse <- mean((results$predicted-results$actual)^2)
mse
#RMSE
rmse <- mse^0.5
rmse
#R2
SSE = sum((results$predicted - results$actual)^2)
SST = sum( (mean(df$G3) - results$actual)^2)

R2 = 1 - SSE/SST
R2

###############################################################################################

#train and build model
model2 <- lm(G3 ~ G1+G2+age+famrel+absences,data=train)
#run the model 
#intrepret the model
summary(model2)

#Test the model with predictions
G3.predict2 <- predict(model2,test)
results2 <- cbind(G3.predict2,test$G3)
colnames(results2) <- c('predicted','actual')
results2 <- as.data.frame(results2)

#apply the function to the predicted results
results2$predicted  <- sapply(results2$predicted,to_zero)

#how good is the model?
#MSE
mse <- mean((results2$predicted-results2$actual)^2)
mse
#RMSE
rmse <- mse^0.5
rmse
#R2
SSE = sum((results2$predicted - results2$actual)^2)
SST = sum( (mean(df$G3) - results2$actual)^2)

R2 = 1 - SSE/SST
R2
